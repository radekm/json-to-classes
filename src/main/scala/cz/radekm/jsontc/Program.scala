package cz.radekm.jsontc

import java.nio.file.{Files, Path}
import java.time.LocalDate
import java.time.format.DateTimeFormatter

import io.circe._
import io.circe.parser._

import scala.collection.mutable.ArrayBuffer
import scala.util.Try

/**
 * Inferred type.
 */
sealed abstract class InfType {
  def opt: Boolean
}
object InfType {
  case object Null extends InfType {
    override def opt: Boolean = true
  }
  case class Bool(opt: Boolean) extends InfType
  case class Num(values: Set[JsonNumber], opt: Boolean) extends InfType
  case class Str(values: Set[String], opt: Boolean) extends InfType
  case class Arr(elemType: Option[InfType], opt: Boolean) extends InfType
  case class Obj(fields: Map[String, InfType], opt: Boolean) extends InfType
}

object Inference {
  def mergeManyTypes(types: Seq[InfType]): Option[InfType] = types.reduceOption(mergeTypes(_, _))

  def mergeTypes(a: InfType, b: InfType, exchanged: Boolean = false): InfType = (a, b) match {
    case (InfType.Null, InfType.Null) => InfType.Null
    case (InfType.Null, InfType.Bool(_)) => InfType.Bool(true)
    case (InfType.Null, InfType.Num(values, _)) => InfType.Num(values, true)
    case (InfType.Null, InfType.Str(values, _)) => InfType.Str(values, true)
    case (InfType.Null, InfType.Arr(elemType, _)) => InfType.Arr(elemType, true)
    case (InfType.Null, InfType.Obj(fields, _)) => InfType.Obj(fields, true)

    case (InfType.Bool(o), InfType.Bool(o2)) =>
      InfType.Bool(o || o2)
    case (InfType.Num(values, o), InfType.Num(values2, o2)) =>
      InfType.Num(values union values2, o || o2)
    case (InfType.Str(values, o), InfType.Str(values2, o2)) =>
      InfType.Str(values union values2, o || o2)
    case (InfType.Arr(t, o), InfType.Arr(t2, o2)) =>
      val types = List(t, t2).collect { case Some(t) => t }
      InfType.Arr(mergeManyTypes(types), o || o2)
    case (InfType.Obj(fields, o), InfType.Obj(fields2, o2)) =>
      val mergedFields = (fields.keys ++ fields2.keys).map { k =>
        val types = List(fields.get(k), fields2.get(k)).collect { case Some(t) => t }
        k -> mergeManyTypes(types).get
      }.toMap
      InfType.Obj(mergedFields, o || o2)

    case _ if !exchanged => mergeTypes(b, a, exchanged = true)
    case _ => sys.error(s"Cannot merge types $a and $b")
  }

  def inferType(json: Json): InfType = json.fold(
    jsonNull = InfType.Null,
    jsonBoolean = _ => InfType.Bool(false),
    jsonNumber = num => InfType.Num(Set(num), false),
    jsonString = str => InfType.Str(Set(str), false),
    jsonArray = arr => InfType.Arr(mergeManyTypes(arr.map(inferType).toList), false),
    jsonObject = obj => InfType.Obj(obj.toMap.map { case (k, v) => k -> inferType(v) }, false)
  )
}

sealed abstract class ScalaType {
  def opt: Boolean
}
object ScalaType {
  case object Null extends ScalaType {
    def opt = true
  }

  case class Bool(opt: Boolean) extends ScalaType

  case class Long(opt: Boolean) extends ScalaType
  case class BigInt(opt: Boolean) extends ScalaType
  case class BigDecimal(opt: Boolean) extends ScalaType

  case class Date(pattern: String, opt: Boolean) extends ScalaType
  case class DateTime(pattern: String, opt: Boolean) extends ScalaType
  case class Str(opt: Boolean) extends ScalaType

  case class Array(elemType: ScalaType, opt: Boolean) extends ScalaType
  case class Class(className: String, opt: Boolean) extends ScalaType

  case class ClassDef(fields: Set[ClassDef.Field])
  object ClassDef {
    case class Field(scalaName: String, jsonName: String, fieldType: ScalaType)
  }

  val datePatterns = Seq(
    "yyyy-MM-dd" // 2014-07-19
  )

  val dateTimePatterns = Seq(
    "yyyy-MM-dd HH:mm:ss" // 2014-07-19 15:30:10
  )

  def findPattern[T](patterns: Seq[String], values: Set[String])(
    parse: (String, DateTimeFormatter) => T
  ): Option[String] = {
    patterns.find { pat =>
      val formatter = DateTimeFormatter.ofPattern(pat)
      values.forall { value =>
        Try { parse(value, formatter) }.isSuccess
      }
    }
  }

  def fieldNameFromJson(s: String): String =
    // Make first letter lowercase.
    if (s.length == 0 || !s.head.isUpper) s
    else s.updated(0, s.head.toLower)

  def classNameFromJson(s: String): String = fieldNameFromJson(s).capitalize

  def fromInfType(
    newClassName: String,
    tpe: InfType,
    createdClasses: ArrayBuffer[(String, ClassDef)] // Order matters
  ): ScalaType = tpe match {
    case InfType.Null => Null
    case InfType.Bool(opt) => Bool(opt)
    case InfType.Num(values, opt) =>
      if (values.forall { v => v.toLong.isDefined }) Long(opt)
      else if (values.forall { v => v.toBigInt.isDefined }) Long(opt)
      else BigDecimal(opt)
    case InfType.Str(values, opt) =>
      lazy val datePattern = findPattern(datePatterns, values) { LocalDate.parse }
      lazy val dateTimePattern = findPattern(dateTimePatterns, values) { LocalDate.parse }
      if (datePattern.isDefined) Date(datePattern.get, opt)
      else if (dateTimePattern.isDefined) DateTime(dateTimePattern.get, opt)
      else Str(opt)
    case InfType.Arr(Some(elemType), opt) =>
      Array(fromInfType(newClassName, elemType, createdClasses), opt)
    case InfType.Obj(fields, opt) =>
      val classDef = ClassDef(fields.map { case (fieldName, fieldType) =>
        ClassDef.Field(
          scalaName = fieldNameFromJson(fieldName),
          jsonName = fieldName,
          fieldType = fromInfType(classNameFromJson(fieldName), fieldType, createdClasses))
      }.toSet)
      val className = createdClasses.find { case (_, cd) => cd == classDef } match {
        case None =>
          // Find class name which isn't already used.
          val name = (newClassName +: (2 to 10).map(i => s"$newClassName$i"))
            .find(newName => !createdClasses.exists(_._1 == newName))
            .getOrElse(sys.error(s"Cannot free variant of class name $newClassName"))
          createdClasses += name -> classDef
          name
        case Some((name, _)) => name
      }
      Class(className, opt)
    case _ => sys.error(s"Cannot convert inferred type to Scala type: $tpe")
  }
}

object Generator {
  import scala.meta._

  private def generateCodeForType(tpe: ScalaType): Type = {
    val t = tpe match {
      // `Type.Name("Nothing")` is correct, but doesn't work very will with implicit expansion.
      case ScalaType.Null => Type.Name("String")
      case ScalaType.Bool(_) => Type.Name("Boolean")
      case ScalaType.Long(_) => Type.Name("Long")
      case ScalaType.BigInt(_) => Type.Name("BigInt")
      case ScalaType.BigDecimal(_) => Type.Name("BigDecimal")
      case ScalaType.Date(pattern, _) => Type.Name("LocalDate")
      case ScalaType.DateTime(pattern, _) => Type.Name("LocalDateTime")
      case ScalaType.Str(_) => Type.Name("String")
      case ScalaType.Array(elemType, _) => Type.Apply(Type.Name("List"), List(generateCodeForType(elemType)))
      case ScalaType.Class(className, _) => Type.Name(className)
    }
    if (tpe.opt) Type.Apply(Type.Name("Option"), List(t))
    else t
  }

  def generateImports(): List[Import] = {
    List(
      q"""import io.circe._""",
      q"""import io.circe.parser._""",
      q"""import io.circe.syntax._"""
    )
  }

  def generateCaseClasses(
    createdClasses: ArrayBuffer[(String, ScalaType.ClassDef)]
  ): List[Defn.Class] = {
    createdClasses.toList.map { case (className, ScalaType.ClassDef(fields)) =>
      val typeName = Type.Name(className)
      val params = fields.toList.sortBy(_.scalaName).map { field =>
        Term.Param(
          Nil,
          Term.Name(field.scalaName),
          Some(generateCodeForType(field.fieldType)),
          None)
      }
      q"""case class $typeName(..$params)"""
    }
  }

  def generateCompanionObjects(
    createdClasses: ArrayBuffer[(String, ScalaType.ClassDef)]
  ): List[Defn.Object] = {
    createdClasses.toList.map { case (className, ScalaType.ClassDef(fields)) =>
      val termName = Term.Name(className)
      val typeName = Type.Name(className)
      val forProductName = Term.Name(s"forProduct${fields.size}")
      val jsonFieldNames = fields.toList.sortBy(_.scalaName).map { field =>
        Lit.String(field.jsonName)
      }
      q"""
        object $termName {
          implicit val circeDecoder: Decoder[$typeName] =
            Decoder.$forProductName(..$jsonFieldNames)($termName.apply)
          implicit val circeEncoder: Encoder[$typeName] =
            Encoder.$forProductName(..$jsonFieldNames)(x => $termName.unapply(x).get)
        }
      """
    }
  }
}

object Program {
  def main(args: Array[String]): Unit = {
    // Parse JSON.
    val json = parse(Files.readString(Path.of(args.head))) match {
      case Left(value) => throw value
      case Right(value) => value
    }

    val inferredType = Inference.inferType(json)
    // Description of case classes.
    val createdClasses = ArrayBuffer.empty[(String, ScalaType.ClassDef)]
    ScalaType.fromInfType("Root", inferredType, createdClasses)

    // Print generated source code.
    Generator.generateImports().foreach(println)
    Generator.generateCaseClasses(createdClasses).foreach(println)
    Generator.generateCompanionObjects(createdClasses).foreach(println)
  }
}
