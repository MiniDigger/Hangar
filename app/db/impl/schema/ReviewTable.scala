package db.impl.schema

import java.sql.Timestamp

import db.DbRef
import db.impl.OrePostgresDriver.api._
import db.table.ModelTable
import models.admin.Review
import models.project.Version
import models.user.User

class ReviewTable(tag: Tag) extends ModelTable[Review](tag, "project_version_reviews") {

  def versionId = column[DbRef[Version]]("version_id")
  def userId    = column[DbRef[User]]("user_id")
  def endedAt   = column[Timestamp]("ended_at")

  override def * = mkProj((id.?, createdAt.?, versionId, userId, endedAt.?))(mkTuple[Review]())
}
