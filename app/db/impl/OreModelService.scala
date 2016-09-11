package db.impl

import java.util.concurrent.TimeUnit
import javax.inject.{Inject, Singleton}

import db.impl.access.{FlagBase, ProjectBase, UserBase, VersionBase}
import db.impl.action.{PageActions, ProjectActions, UserActions, VersionActions}
import db.impl.pg.OrePostgresDriver
import db.impl.pg.OrePostgresDriver.api._
import db.impl.pg.OreTypeSetters._
import db.meta.ModelAssociation
import db.{ModelActions, ModelRegistry, ModelService}
import forums.DiscourseApi
import models.project.Channel
import models.user.Notification
import ore.Colors.Color
import ore.notification.NotificationTypes.NotificationType
import ore.permission.role.RoleTypes.RoleType
import ore.project.Categories.Category
import ore.project.FlagReasons.FlagReason
import play.api.db.slick.DatabaseConfigProvider
import slick.driver.JdbcProfile
import util.{OreConfig, OreEnv}

import scala.concurrent.duration.Duration

/**
  * The Ore ModelService implementation. Contains registration of Ore-specific
  * types and Models.
  *
  * @param db DatabaseConfig
  */
@Singleton
class OreModelService @Inject()(env: OreEnv,
                                config: OreConfig,
                                forums: DiscourseApi,
                                db: DatabaseConfigProvider)
                                extends ModelService {

  override lazy val registry = new ModelRegistry {}
  override lazy val processor = new OreModelProcessor(this, this.users, this.projects, this.config, this.forums)
  override lazy val driver = OrePostgresDriver
  override lazy val DB = db.get[JdbcProfile]
  override lazy val DefaultTimeout: Duration = Duration(config.app.getInt("db.default-timeout").get, TimeUnit.SECONDS)

  import registry.{registerActions, registerModelBase, registerTypeSetter}

  val users = registerModelBase(classOf[UserBase], new UserBase(this, forums, config))
  val projects = registerModelBase(classOf[ProjectBase], new ProjectBase(this, this.env, this.config, this.forums))
  val versions = registerModelBase(classOf[VersionBase], new VersionBase(this))
  val flags = registerModelBase(classOf[FlagBase], new FlagBase(this))

  // Custom types
  registerTypeSetter(classOf[Color], ColorTypeSetter)
  registerTypeSetter(classOf[RoleType], RoleTypeTypeSetter)
  registerTypeSetter(classOf[List[RoleType]], RoleTypeListTypeSetter)
  registerTypeSetter(classOf[Category], CategoryTypeSetter)
  registerTypeSetter(classOf[FlagReason], FlagReasonTypeSetter)
  registerTypeSetter(classOf[NotificationType], NotificationTypeTypeSetter)

  // Associations
  val projectWatchers = new ModelAssociation[ProjectWatchersTable](
    this, _.projectId, _.userId, classOf[ProjectWatchersTable], TableQuery[ProjectWatchersTable])

  // Ore models
  registerActions(new UserActions(this)).withAssociation(this.projectWatchers)
  registerActions(new ProjectActions(this)).withAssociation(this.projectWatchers)
  registerActions(new VersionActions(this))
  registerActions(new ModelActions(this, classOf[Channel], TableQuery[ChannelTable]))
  registerActions(new PageActions(this))
  registerActions(new ModelActions(this, classOf[Notification], TableQuery[NotificationTable]))

}