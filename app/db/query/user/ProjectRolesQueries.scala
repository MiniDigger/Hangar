package db.query.user

import java.sql.Timestamp

import db.OrePostgresDriver.api._
import db.UserProjectRolesTable
import db.query.Queries
import db.query.Queries.DB._
import models.user.{ProjectRole, User}
import ore.permission.role.RoleTypes.RoleType
import slick.lifted.TableQuery

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}

class ProjectRolesQueries extends Queries[UserProjectRolesTable, ProjectRole](TableQuery[UserProjectRolesTable]) {

  /**
    * Returns all the users that have one or more roles in the specified
    * Project.
    *
    * @param projectId  Project to get Users for
    * @return           Sequence of users
    */
  def distinctUsersIn(projectId: Int): Future[Seq[User]] = {
    val query = for { role <- this.models if role.projectId === projectId } yield role.userId
    val promise = Promise[Seq[User]]
    run(query.distinct.result).andThen {
      case Failure(thrown) => promise.failure(thrown)
      case Success(userIds) => promise.success(for (userId <- userIds) yield User.withId(userId).get)
    }
    promise.future
  }

  /**
    * Sets the [[RoleType]] of the specified [[ProjectRole]].
    *
    * @param role     Role to update
    * @param roleType Role type to set
    */
  def setRoleType(role: ProjectRole, roleType: RoleType) = {
    val query = for { model <- this.models if model.id === role.id.get } yield model.roleType
    run(query.update(roleType))
  }

  override def copyInto(id: Option[Int], theTime: Option[Timestamp], model: ProjectRole): ProjectRole = {
    model.copy(id = id, createdAt = theTime)
  }

}