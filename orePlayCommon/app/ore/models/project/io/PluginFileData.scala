package ore.models.project.io

import scala.language.higherKinds
import java.io.BufferedReader
import java.util
import java.util.UUID

import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal
import ore.data.project.Dependency
import ore.db.{DbRef, Model, ModelService}
import ore.models.project.{TagColor, Version, VersionTag}
import org.spongepowered.plugin.meta.McModInfo
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import scala.beans.BeanProperty
import scala.collection.immutable.HashMap
import scala.util.parsing.json.JSON

/**
  * The metadata within a [[PluginFile]]
  *
  * @author phase
  * @param data the data within a [[PluginFile]]
  */
class PluginFileData(data: Seq[DataValue]) {

  private val dataValues: Seq[DataValue] = data
    .groupBy(_.key)
    .flatMap {
      case (key: String, values: Seq[DataValue]) =>
        // combine dependency lists that may come from different files
        if (values.lengthIs > 1) {
          import cats.instances.vector._
          import cats.syntax.all._

          val (otherValues, depSeq) = values.toVector.partitionEither {
            case DependencyDataValue(_, deps) => Right(deps)
            case other                        => Left(other)
          }

          otherValues :+ DependencyDataValue(key, depSeq.flatten)
        } else values
    }
    .toSeq

  def id: Option[String] =
    getString("id")

  def name: Option[String] =
    getString("name")

  def description: Option[String] =
    getString("description")

  def version: Option[String] =
    getString("version")

  def authors: Seq[String] =
    getStrings("authors").getOrElse(Seq())

  def dependencies: Seq[Dependency] =
    getDeps("dependencies").getOrElse(Seq())

  private def getString(key: String): Option[String] = get(key).collect {
    case StringDataValue(_, value) => value
  }

  private def getStrings(key: String): Option[Seq[String]] = get(key).collect {
    case StringListValue(_, value) => value
  }

  private def getDeps(key: String): Option[Seq[Dependency]] = get(key).collect {
    case DependencyDataValue(_, value) => value
  }

  private def get(key: String): Option[DataValue] = dataValues.find(_.key == key)

  def isValidPlugin: Boolean = dataValues.exists {
    case _: StringDataValue => true
    case _                  => false
  }

  def createTags[F[_]](versionId: DbRef[Version])(implicit service: ModelService[F]): F[Seq[Model[VersionTag]]] = {
    val buffer = new ArrayBuffer[VersionTag]

    if (containsMixins) {
      val mixinTag = VersionTag(versionId, "Mixin", None, TagColor.Mixin)
      buffer += mixinTag
    }

    service.bulkInsert(buffer.toSeq)
  }

  /**
    * A mod using Mixins will contain the "MixinConfigs" attribute in their MANIFEST
    *
    * @return
    */
  def containsMixins: Boolean =
    dataValues.exists {
      case p: StringDataValue => p.key == "MixinConfigs"
      case _                  => false
    }

}

object PluginFileData {
  val fileTypes: Seq[FileTypeHandler] = Seq(McModInfoHandler, ManifestHandler, ModTomlHandler, PluginYmlHandler, BungeeYmlHandler, VelocityFileHandler)

  def fileNames: Seq[String] = fileTypes.map(_.fileName).distinct

  def getData(fileName: String, stream: BufferedReader): Seq[DataValue] =
    fileTypes.filter(_.fileName == fileName).flatMap(_.getData(stream))

}

/**
  * A data element in a data file.
  */
sealed trait DataValue {
  def key: String
}

/**
  * A data element that is a String, such as the plugin id or version
  *
  * @param value the value extracted from the file
  */
case class StringDataValue(key: String, value: String) extends DataValue

/**
  * A data element that is a list of strings, such as an authors list
  *
  * @param value the value extracted from the file
  */
case class StringListValue(key: String, value: Seq[String]) extends DataValue

/**
  * A data element that is a list of [[Dependency]]
  *
  * @param value the value extracted from the file
  */
case class DependencyDataValue(key: String, value: Seq[Dependency]) extends DataValue

sealed abstract case class FileTypeHandler(fileName: String) {
  def getData(bufferedReader: BufferedReader): Seq[DataValue]
}

object McModInfoHandler extends FileTypeHandler("mcmod.info") {

  @SuppressWarnings(Array("scalafix:DisableSyntax.null"))
  override def getData(bufferedReader: BufferedReader): Seq[DataValue] = {
    val dataValues = new ArrayBuffer[DataValue]
    try {
      val info = McModInfo.DEFAULT.read(bufferedReader).asScala
      if (info.lengthCompare(1) < 0) Nil
      else {
        val metadata = info.head

        if (metadata.getId != null)
          dataValues += StringDataValue("id", metadata.getId)

        if (metadata.getVersion != null)
          dataValues += StringDataValue("version", metadata.getVersion)

        if (metadata.getName != null)
          dataValues += StringDataValue("name", metadata.getName)

        if (metadata.getDescription != null)
          dataValues += StringDataValue("description", metadata.getDescription)

        if (metadata.getUrl != null)
          dataValues += StringDataValue("url", metadata.getUrl)

        if (metadata.getAuthors != null)
          dataValues += StringListValue("authors", metadata.getAuthors.asScala.toSeq)

        if (metadata.getDependencies != null) {
          val dependencies = metadata.getDependencies.asScala.map(p => Dependency(p.getId, Option(p.getVersion))).toSeq
          dataValues += DependencyDataValue("dependencies", dependencies)
        }

        dataValues.toSeq
      }
    } catch {
      case NonFatal(e) =>
        e.printStackTrace()
        Nil
    }
  }
}

object ManifestHandler extends FileTypeHandler("META-INF/MANIFEST.MF") {
  override def getData(bufferedReader: BufferedReader): Seq[DataValue] = {
    val dataValues = new ArrayBuffer[DataValue]

    val lines = LazyList.continually(bufferedReader.readLine()).takeWhile(_ != null) // scalafix:ok
    // Check for Mixins
    for (line <- lines if line.startsWith("MixinConfigs: ")) {
      val mixinConfigs = line.split(": ")(1)
      dataValues += StringDataValue("MixinConfigs", mixinConfigs)
    }

    dataValues.toSeq
  }
}

object ModTomlHandler extends FileTypeHandler("mod.toml") {
  override def getData(bufferedReader: BufferedReader): Seq[DataValue] =
    // TODO: Get format from Forge once it has been decided on
    Nil
}

object PluginYmlHandler extends FileTypeHandler("plugin.yml") {

  override def getData(bufferedReader: BufferedReader): Seq[DataValue] = {
    val dataValues = new ArrayBuffer[DataValue]
    try {
      val yaml = new Yaml()
      val value = yaml.load(bufferedReader).asInstanceOf[java.util.Map[String, Any]]
      if (value == null || value.size() == 0) Nil
      else {
        if (value.containsKey("version"))
          dataValues += StringDataValue("version", value.get("version").asInstanceOf[String])

        if (value.containsKey("name" ))
          dataValues += StringDataValue("name", value.get("name").asInstanceOf[String])

        if (value.containsKey("description"))
          dataValues += StringDataValue("description", value.get("description").asInstanceOf[String])

        if (value.containsKey("website"))
          dataValues += StringDataValue("url", value.get("website").asInstanceOf[String])

        if (value.containsKey("author"))
          dataValues += StringListValue("authors", Seq(value.get("author").asInstanceOf[String]))

        if (value.containsKey("authors"))
          dataValues += StringListValue("authors", value.get("authors").asInstanceOf[java.util.ArrayList[String]].asScala.toSeq)

        if (value.containsKey("depend")) {
          val dependencies = value.get("depend").asInstanceOf[java.util.ArrayList[String]].asScala.map(p => Dependency(p, Option.empty)).toSeq
          dataValues += DependencyDataValue("dependencies", dependencies)
        }

        val version = Some(value.get("api-version").asInstanceOf[Double].toString).orElse(Some("unknown")).asInstanceOf[Some[String]]
        dataValues += DependencyDataValue("dependencies", Seq(Dependency("paperapi", version)))

        dataValues.toSeq
      }
    } catch {
      case NonFatal(e) =>
        e.printStackTrace()
        Nil
    }
  }
}

object BungeeYmlHandler extends FileTypeHandler("bungee.yml") {

  override def getData(bufferedReader: BufferedReader): Seq[DataValue] = {
    val dataValues = new ArrayBuffer[DataValue]
    try {
      val yaml = new Yaml()
      val value = yaml.load(bufferedReader).asInstanceOf[java.util.Map[String, Any]]
      if (value == null || value.size() == 0) Nil
      else {
        if (value.containsKey("version"))
          dataValues += StringDataValue("version", value.get("version").asInstanceOf[String])

        if (value.containsKey("name" ))
          dataValues += StringDataValue("name", value.get("name").asInstanceOf[String])

        if (value.containsKey("description"))
          dataValues += StringDataValue("description", value.get("description").asInstanceOf[String])

        if (value.containsKey("website"))
          dataValues += StringDataValue("url", value.get("website").asInstanceOf[String])

        if (value.containsKey("author"))
          dataValues += StringListValue("authors", Seq(value.get("author").asInstanceOf[String]))

        if (value.containsKey("depends")) {
          val dependencies = value.get("depends").asInstanceOf[java.util.ArrayList[String]].asScala.map(p => Dependency(p, Option.empty)).toSeq
          dataValues += DependencyDataValue("dependencies", dependencies)
        }

        dataValues += DependencyDataValue("dependencies", Seq(Dependency("waterfall", Some("unknown"))))

        dataValues.toSeq
      }
    } catch {
      case NonFatal(e) =>
        e.printStackTrace()
        Nil
    }
  }
}

object VelocityFileHandler extends FileTypeHandler("velocity-plugin.json") {

  override def getData(bufferedReader: BufferedReader): Seq[DataValue] = {
    val dataValues = new ArrayBuffer[DataValue]
    try {
      val string = LazyList.continually(bufferedReader.readLine()).takeWhile(_ != null).mkString("\n")
      val value = JSON.parseFull(string).asInstanceOf[Option[HashMap[String, Any]]].get
      if (value == null || value.isEmpty) Nil
      else {
        if (value.contains("version"))
          dataValues += StringDataValue("version", value.get("version").asInstanceOf[Some[String]].get)

        if (value.contains("name" ))
          dataValues += StringDataValue("name", value.get("name").asInstanceOf[Some[String]].get)

        if (value.contains("description"))
          dataValues += StringDataValue("description", value.get("description").asInstanceOf[Some[String]].get)

        if (value.contains("website"))
          dataValues += StringDataValue("url", value.get("website").asInstanceOf[Some[String]].get)

        if (value.contains("author"))
          dataValues += StringListValue("authors", Seq(value.get("author").asInstanceOf[Some[String]].get))

        if (value.contains("authors"))
          dataValues += StringListValue("authors", value.get("authors").asInstanceOf[Some[List[String]]].get)

        if (value.contains("depend")) {
          val dependencies = value.get("depend").asInstanceOf[Some[List[String]]].get.map(p => Dependency(p, Option.empty)).toSeq
          dataValues += DependencyDataValue("dependencies", dependencies)
        }

        dataValues += DependencyDataValue("dependencies", Seq(Dependency("velocity", Some("unknown"))))

        dataValues.toSeq
      }
    } catch {
      case NonFatal(e) =>
        e.printStackTrace()
        Nil
    }
  }
}
