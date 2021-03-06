package ore.models.project

import java.time.OffsetDateTime

import scala.collection.immutable

import ore.db._
import ore.db.impl.ModelCompanionPartial
import ore.db.impl.common.Named
import ore.db.impl.schema.VersionTagTable

import enumeratum.values._
import slick.lifted.TableQuery

case class VersionTag(
    versionId: DbRef[Version],
    name: String,
    data: Option[String],
    color: TagColor
) extends Named
object VersionTag extends ModelCompanionPartial[VersionTag, VersionTagTable](TableQuery[VersionTagTable]) {

  override def asDbModel(
      model: VersionTag,
      id: ObjId[VersionTag],
      time: ObjOffsetDateTime
  ): Model[VersionTag] = Model(id, ObjOffsetDateTime(OffsetDateTime.MIN), model)

  implicit val query: ModelQuery[VersionTag] = ModelQuery.from(this)
}

sealed abstract class TagColor(val value: Int, val background: String, val foreground: String) extends IntEnumEntry
object TagColor extends IntEnum[TagColor] {

  val values: immutable.IndexedSeq[TagColor] = findValues

  // Tag colors
  case object Sponge        extends TagColor(1, "#F7Cf0D", "#333333")
  case object Forge         extends TagColor(2, "#dfa86a", "#FFFFFF")
  case object Unstable      extends TagColor(3, "#FFDAB9", "#333333")
  case object SpongeForge   extends TagColor(4, "#910020", "#FFFFFF")
  case object SpongeVanilla extends TagColor(5, "#50C888", "#FFFFFF")
  case object SpongeCommon  extends TagColor(6, "#5d5dff", "#FFFFFF")
  case object Lantern       extends TagColor(7, "#4EC1B4", "#FFFFFF")
  case object Mixin         extends TagColor(8, "#FFA500", "#333333")
  case object Paper         extends TagColor(27, "#0288d1", "#333333")
  case object Velocity      extends TagColor(28, "#039be5", "#333333")

  //From the normal color enum
  case object Purple      extends TagColor(9, "#B400FF", "#FFFFFF")
  case object Violet      extends TagColor(10, "#C87DFF", "#FFFFFF")
  case object Magenta     extends TagColor(11, "#E100E1", "#FFFFFF")
  case object Blue        extends TagColor(12, "#0000FF", "#FFFFFF")
  case object LightBlue   extends TagColor(13, "#B9F2FF", "#FFFFFF")
  case object Quartz      extends TagColor(14, "#E7FEFF", "#FFFFFF")
  case object Aqua        extends TagColor(15, "#0096FF", "#FFFFFF")
  case object Cyan        extends TagColor(16, "#00E1E1", "#FFFFFF")
  case object Green       extends TagColor(17, "#00DC00", "#FFFFFF")
  case object DarkGreen   extends TagColor(18, "#009600", "#FFFFFF")
  case object Chartreuse  extends TagColor(19, "#7FFF00", "#FFFFFF")
  case object Amber       extends TagColor(20, "#FFC800", "#FFFFFF")
  case object Gold        extends TagColor(21, "#CFB53B", "#FFFFFF")
  case object Orange      extends TagColor(22, "#FF8200", "#FFFFFF")
  case object Red         extends TagColor(23, "#DC0000", "#FFFFFF")
  case object Silver      extends TagColor(24, "#C0C0C0", "#FFFFFF")
  case object Gray        extends TagColor(25, "#A9A9A9", "#FFFFFF")
  case object Transparent extends TagColor(26, "transparent", "#FFFFFF")
}
