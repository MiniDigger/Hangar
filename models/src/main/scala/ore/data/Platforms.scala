package ore.data

import scala.language.higherKinds

import scala.collection.immutable

import ore.data.project.Dependency
import ore.db.{DbRef, Model, ModelService}
import ore.models.project.{TagColor, Version, VersionTag}

import enumeratum.values._

/**
  * The Platform a plugin/mod runs on
  *
  * @author phase
  */
sealed abstract class Platform(
    val value: Int,
    val name: String,
    val platformCategory: PlatformCategory,
    val priority: Int,
    val dependencyId: String,
    val tagColor: TagColor,
    val url: String
) extends IntEnumEntry {

  def createGhostTag(versionId: DbRef[Version], version: Option[String]): VersionTag =
    VersionTag(versionId, name, version, tagColor)
}
object Platform extends IntEnum[Platform] {

  val values: immutable.IndexedSeq[Platform] = findValues

  case object Paper
    extends Platform(
      0,
      "Paper",
      ServerCategory,
      0,
      "paperapi",
      TagColor.Paper,
      "https://papermc.io/downloads"
    )

  case object Waterfall
    extends Platform(
      2,
      "Waterfall",
      ProxyCategory,
      2,
      "waterfall",
      TagColor.Sponge,
      "https://papermc.io/downloads"
    )

  case object Velocity
    extends Platform(
      3,
      "Velocity",
      ProxyCategory,
      3,
      "velocity",
      TagColor.Velocity,
      "https://www.velocitypowered.com/"
    )

  def getPlatforms(dependencyIds: Seq[String]): Seq[Platform] = {
    Platform.values
      .filter(p => dependencyIds.contains(p.dependencyId))
      .groupBy(_.platformCategory)
      .flatMap(_._2.groupBy(_.priority).maxBy(_._1)._2)
      .toSeq
  }

  def ghostTags(versionId: DbRef[Version], dependencies: Seq[Dependency]): Seq[VersionTag] =
    getPlatforms(dependencies.map(_.pluginId))
      .map(p => p.createGhostTag(versionId, dependencies.find(_.pluginId == p.dependencyId).get.version))

  def createPlatformTags[F[_]](versionId: DbRef[Version], dependencies: Seq[Dependency])(
      implicit service: ModelService[F]
  ): F[Seq[Model[VersionTag]]] = service.bulkInsert(ghostTags(versionId, dependencies))

}

/**
  * The category of a platform.
  * Examples would be
  *
  * Sponge <- SpongeAPI, SpongeForge, SpongeVanilla
  * Forge <- Forge (maybe Rift if that doesn't die?)
  * Bukkit <- Bukkit, Spigot, Paper
  * Canary <- Canary, Neptune
  *
  * @author phase
  */
sealed trait PlatformCategory {
  def name: String
  def tagName: String

  def getPlatforms: Seq[Platform] = Platform.values.filter(_.platformCategory == this)
}

case object ServerCategory extends PlatformCategory {
  val name    = "Server Plugins"
  val tagName = "Server"
}

case object ProxyCategory extends PlatformCategory {
  val name    = "Proxy Plugins"
  val tagName = "Proxy"
}

//case object SpongeCategory extends PlatformCategory {
//  val name    = "Sponge Plugins"
//  val tagName = "Sponge"
//}
//
//case object ForgeCategory extends PlatformCategory {
//  val name    = "Forge Mods"
//  val tagName = "Forge"
//}

object PlatformCategory {
  def getPlatformCategories: Seq[PlatformCategory] = Seq(ServerCategory, ProxyCategory)
}
