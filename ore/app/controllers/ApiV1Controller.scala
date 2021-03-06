package controllers

import java.util.{Base64, UUID}

import play.api.i18n.Messages
import play.api.libs.json._
import play.api.mvc._

import controllers.sugar.Requests.AuthedProjectRequest
import form.OreForms
import ore.auth.CryptoUtils
import ore.db.access.ModelView
import ore.db.impl.OrePostgresDriver.api._
import ore.db.impl.schema.ProjectApiKeyTable
import ore.db.{DbRef, Model, ObjId}
import ore.models.api.ProjectApiKey
import ore.models.organization.Organization
import ore.models.project.factory.ProjectFactory
import ore.models.project.io.PluginUpload
import ore.models.project.{Channel, Page, Project, Version}
import ore.models.user.{LoggedActionProject, LoggedActionType, User}
import ore.permission.Permission
import ore.permission.role.Role
import ore.rest.{OreRestfulApiV1, OreWrites}
import _root_.util.syntax._
import _root_.util.{StatusZ, UserActionLogger}

import akka.http.scaladsl.model.Uri
import cats.data.{EitherT, OptionT}
import cats.instances.list._
import cats.syntax.all._
import com.typesafe.scalalogging
import zio.blocking.Blocking
import zio.interop.catz._
import zio.{Task, UIO, ZIO}

/**
  * Ore API (v1)
  */
final class ApiV1Controller(
    api: OreRestfulApiV1,
    status: StatusZ,
    forms: OreForms,
    factory: ProjectFactory
)(
    implicit oreComponents: OreControllerComponents
) extends OreBaseController
    with OreWrites {

  def AuthedProjectActionById(
      pluginId: String
  ): ActionBuilder[AuthedProjectRequest, AnyContent] =
    UserLock(ShowHome).andThen(authedProjectActionById(pluginId))

  private val Logger = scalalogging.Logger("SSO")

  private def ApiResult(json: Option[JsValue]): Result = json.map(Ok(_)).getOrElse(NotFound)

  /**
    * Returns a JSON view of all projects.
    *
    * @return           JSON view of projects
    */
  def listProjects(
      categories: Option[String],
      sort: Option[Int],
      q: Option[String],
      limit: Option[Long],
      offset: Option[Long]
  ): Action[AnyContent] = Action.asyncF {
    this.api.getProjectList(categories, sort, q, limit, offset).map(Ok(_))
  }

  /**
    * Returns a JSON view of a Project.
    *
    * @param pluginId   Plugin ID of project
    * @return           Project with Plugin ID
    */
  def showProject(pluginId: String): Action[AnyContent] = Action.asyncF {
    this.api.getProject(pluginId).map(ApiResult)
  }

  def createKey(pluginId: String): Action[AnyContent] =
    Action.andThen(AuthedProjectActionById(pluginId)).andThen(ProjectPermissionAction(Permission.EditApiKeys)).asyncF {
      implicit request =>
        val projectId = request.data.project.id.value
        val res = for {
          exists <- OptionT
            .liftF[UIO, Boolean](ModelView.now(ProjectApiKey).exists(k => k.projectId === projectId))
          if !exists
          pak <- OptionT.liftF(
            service.insert(
              ProjectApiKey(
                projectId = projectId,
                value = UUID.randomUUID().toString.replace("-", "")
              )
            )
          )
          _ <- OptionT.liftF(
            UserActionLogger.log(
              request.request,
              LoggedActionType.ProjectSettingsChanged,
              projectId,
              s"${request.user.name} created a new ApiKey",
              ""
            )(LoggedActionProject.apply)
          )
        } yield {
          //Gets around unused warning
          identity(exists)
          Created(Json.toJson(pak))
        }
        res.getOrElse(BadRequest)
    }

  def revokeKey(pluginId: String): Action[AnyContent] =
    AuthedProjectActionById(pluginId).andThen(ProjectPermissionAction(Permission.EditApiKeys)).asyncF {
      implicit request =>
        val res = for {
          optKey <- forms.ProjectApiKeyRevoke.bindOptionT[UIO]
          key    <- optKey
          if key.projectId == request.data.project.id.value
          _ <- OptionT.liftF(service.delete(key))
          _ <- OptionT.liftF(
            UserActionLogger.log(
              request.request,
              LoggedActionType.ProjectSettingsChanged,
              request.data.project.id,
              s"${request.user.name} removed an ApiKey",
              ""
            )(LoggedActionProject.apply)
          )
        } yield Ok
        res.getOrElse(BadRequest)
    }

  /**
    * Returns a JSON view of Versions meeting the specified criteria.
    *
    * @param pluginId Project plugin ID
    * @param channels Channels to get versions from
    * @param limit    Amount to take
    * @param offset   Amount to drop
    * @return         List of versions
    */
  def listVersions(
      pluginId: String,
      channels: Option[String],
      limit: Option[Int],
      offset: Option[Int]
  ): Action[AnyContent] = Action.asyncF {
    this.api.getVersionList(pluginId, channels, limit, offset, onlyPublic = true).map(Some.apply).map(ApiResult)
  }

  /**
    * Shows the specified Project Version.
    *
    * @param pluginId Project plugin ID
    * @param name     Version name
    * @return         JSON view of Version
    */
  def showVersion(pluginId: String, name: String): Action[AnyContent] = Action.asyncF {
    this.api.getVersion(pluginId, name).map(ApiResult)
  }

  private def error(key: String, error: String)(implicit messages: Messages) =
    Json.obj("errors" -> Map(key -> List(messages(error))))

  def deployVersion(pluginId: String, name: String): Action[AnyContent] =
    ProjectAction(pluginId).asyncF { implicit request =>
      val projectData = request.data
      val project     = projectData.project

      forms.VersionDeploy
        .bindEitherT[ZIO[Blocking, Nothing, *]](hasErrors => BadRequest(Json.obj("errors" -> hasErrors.errorsAsJson)))
        .flatMap { formData =>
          OptionT(formData.channel.value: ZIO[Blocking, Nothing, Option[Model[Channel]]])
            .toRight(BadRequest(Json.obj("errors" -> "Invalid channel")))
            .map(formData -> _)
        }
        .flatMap {
          case (formData, formChannel) =>
            val apiKeyTable = TableQuery[ProjectApiKeyTable]
            def queryApiKey(key: String, pId: DbRef[Project]) = {
              val query = for {
                k <- apiKeyTable if k.value === key && k.projectId === pId
              } yield {
                k.id
              }
              query.exists
            }

            val query = Query.apply(
              (
                queryApiKey(formData.apiKey, project.id),
                project.versions(ModelView.later(Version)).exists(_.versionString === name)
              )
            )

            EitherT
              .liftF[ZIO[Blocking, Nothing, *], Result, (Boolean, Boolean)](service.runDBIO(query.result.head))
              .ensure(Unauthorized(error("apiKey", "api.deploy.invalidKey")))(apiKeyExists => apiKeyExists._1)
              .ensure(BadRequest(error("versionName", "api.deploy.versionExists")))(nameExists => !nameExists._2)
              .semiflatMap(_ => project.user[Task].orDie)
              .semiflatMap(user =>
                user.toMaybeOrganization(ModelView.now(Organization)).semiflatMap(_.user[Task].orDie).getOrElse(user)
              )
              .flatMap { owner =>
                val pluginUpload = this.factory
                  .getUploadError(owner)
                  .map(err => BadRequest(error("user", err)))
                  .toLeft(PluginUpload.bindFromRequest())
                  .flatMap(_.toRight(BadRequest(error("files", "error.noFile"))))

                EitherT.fromEither[ZIO[Blocking, Nothing, *]](pluginUpload).flatMap { data =>
                  EitherT(
                    this.factory
                      .processSubsequentPluginUpload(data, owner, project)
                      .either
                  ).leftMap(err => BadRequest(error("upload", err)))
                }
              }
              .map { pendingVersion =>
                pendingVersion.copy(
                  createForumPost = formData.createForumPost,
                  channelName = formChannel.name,
                  description = formData.changelog
                )
              }
              .semiflatMap(_.complete(project, factory))
              .semiflatMap {
                case (newProject, newVersion, channel, tags) =>
                  val update =
                    if (formData.recommended)
                      service.update(project)(
                        _.copy(
                          recommendedVersionId = Some(newVersion.id)
                        )
                      )
                    else
                      ZIO.unit

                  update.as(Created(api.writeVersion(newVersion, newProject, channel, None, tags)))
              }
        }
        .merge
    }

  def listPages(pluginId: String, parentId: Option[DbRef[Page]]): Action[AnyContent] = Action.asyncF {
    this.api.getPages(pluginId, parentId).value.map(ApiResult)
  }

  /**
    * Returns a JSON view of Ore Users.
    *
    * @param limit    Amount of users to get
    * @param offset   Offset to drop
    * @return         List of users
    */
  def listUsers(limit: Option[Int], offset: Option[Int]): Action[AnyContent] = Action.asyncF {
    this.api.getUserList(limit, offset).map(Ok(_))
  }

  /**
    * Returns a JSON view of the specified User.
    *
    * @param username   Username of user
    * @return           User with username
    */
  def showUser(username: String): Action[AnyContent] = Action.asyncF {
    this.api.getUser(username).map(ApiResult)
  }

  /**
    * Get the tags for a single version
    *
    * @param plugin      Plugin Id
    * @param versionName Version of the plugin
    * @return Tags for the version of the plugin
    */
  def listTags(plugin: String, versionName: String): Action[AnyContent] = Action.asyncF {
    this.api.getTags(plugin, versionName).value.map(ApiResult)
  }

  def tagColor(id: String): Action[AnyContent] = Action {
    ApiResult(this.api.getTagColor(id.toInt))
  }

  /**
    * Returns a JSON statusz endpoint for Ore.
    *
    * @return statusz json
    */
  def showStatusZ: Action[AnyContent] = Action(Ok(this.status.json))

  def syncSso(): Action[AnyContent] = Action.asyncF { implicit request =>
    val confApiKey = this.config.security.sso.apikey
    val confSecret = this.config.security.sso.secret

    Logger.debug("Sync Request received")

    forms.SyncSso
      .bindEitherT[UIO](hasErrors => BadRequest(Json.obj("errors" -> hasErrors.errorsAsJson)))
      .ensure(BadRequest("API Key not valid"))(_._3 == confApiKey) //_3 is apiKey
      .ensure(BadRequest("Signature not matched"))(
        { case (ssoStr, sig, _) => CryptoUtils.hmac_sha256(confSecret, ssoStr.getBytes("UTF-8")) == sig }
      )
      .map(t => Uri.Query(Base64.getMimeDecoder.decode(t._1))) //_1 is sso
      .semiflatMap { q =>
        Logger.debug("Sync Payload: " + q)
        ModelView.now(User).get(q.get("external_id").get.toLong).value.tupleLeft(q)
      }
      .semiflatMap {
        case (query, optUser) =>
          Logger.debug("Sync user found: " + optUser.isDefined)

          val id        = ObjId(query.get("external_id").get.toLong)
          val email     = query.get("email")
          val username  = query.get("username")
          val fullName  = query.get("name")
          val addGroups = query.get("add_groups")

          val globalRoles = addGroups.map { groups =>
            if (groups.trim.isEmpty) Nil
            else groups.split(",").flatMap(Role.withValueOpt).toList
          }

          val updateRoles = (user: Model[User]) =>
            globalRoles.fold(UIO.unit) { roles =>
              user.globalRoles.deleteAllFromParent *> roles
                .map(_.toDbRole.id.value)
                .traverse(user.globalRoles.addAssoc)
                .unit
            }

          optUser
            .map { user =>
              service
                .update(user)(
                  _.copy(
                    email = email.orElse(user.email),
                    name = username.getOrElse(user.name),
                    fullName = fullName.orElse(user.fullName)
                  )
                )
                .flatMap(updateRoles)
            }
            .getOrElse {
              service.insert(User(ObjId(id), fullName, username.get, email)).flatMap(updateRoles)
            }
            .as(Ok(Json.obj("status" -> "success")))
      }
      .toZIO
  }
}
