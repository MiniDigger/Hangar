package form.project

import db.ModelService
import db.impl.access.{ProjectBase, UserBase}
import forums.DiscourseApi
import models.project.Project
import models.user.Notification
import ore.OreConfig
import ore.notification.NotificationTypes
import ore.permission.role.RoleTypes
import ore.project.Categories
import play.api.i18n.MessagesApi
import util.StringUtils._

/**
  * Represents the configurable Project settings that can be submitted via a
  * form.
  */
case class ProjectSettings(categoryName: String,
                           issues: String,
                           source: String,
                           description: String,
                           override val users: List[Int],
                           override val roles: List[String],
                           userUps: List[String],
                           roleUps: List[String],
                           updateIcon: Boolean,
                           ownerId: Option[Int])
                           extends TProjectRoleSetBuilder {

  /**
    * Saves these settings to the specified [[Project]].
    *
    * @param project Project to save to
    */
  def saveTo(project: Project)(implicit service: ModelService, projects: ProjectBase, forums: DiscourseApi,
                               config: OreConfig, messages: MessagesApi, users: UserBase) = {
    project.category = Categories.withName(this.categoryName)
    project.issues = nullIfEmpty(this.issues)
    project.source = nullIfEmpty(this.source)
    project.description = nullIfEmpty(this.description)

    this.ownerId.find(_ != project.ownerId).foreach(ownerId => project.owner = users.get(ownerId).get)

    if (this.updateIcon)
      projects.savePendingIcon(project)

    if (project.isDefined) {
      // Add new roles
      val dossier = project.memberships
      for (role <- this.build()) {
        val user = role.user
        dossier.addRole(role.copy(projectId = project.id.get))
        user.sendNotification(Notification(
          originId = project.ownerId,
          notificationType = NotificationTypes.ProjectInvite,
          message = messages("notification.project.invite", role.roleType.title, project.name)
        ))
      }

      // Update existing roles
      for ((user, i) <- this.userUps.zipWithIndex)
        project.memberships.members
          .find(_.username.equalsIgnoreCase(user)).get.headRole.roleType = RoleTypes.withName (roleUps(i))
    }
  }
}
