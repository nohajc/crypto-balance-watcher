import java.io.FileNotFoundException

import argonaut._
import Argonaut._

import scala.io.Source

case class Credentials(key: String, secret: String)

object Credentials {
  def empty: Credentials = Credentials("", "")
}

object Secret {
  var store: Json = Json.jEmptyObject

  def initFromFile(path: String): Unit = {
    try {
      val fileContents = Source.fromFile(path).mkString
      store = fileContents.parseOption.getOrElse(Json.jEmptyObject)
    }
    catch {
      case e: FileNotFoundException => println("Config file not found")
    }
  }

  def initFromString(config: String): Unit = {
    store = config.parseOption.getOrElse(Json.jEmptyObject)
  }

  trait JsonGetter[T] {
    def get(field: Json): T
  }

  implicit val stringGetter: JsonGetter[String] = new JsonGetter[String] {
    def get(field: Json): String = field.as[String].getOr("")
  }

  implicit val doubleGetter: JsonGetter[Double] = new JsonGetter[Double] {
    def get(field: Json): Double = field.as[Double].getOr(0.0)
  }

  implicit val credentialsGetter: JsonGetter[Credentials] = new JsonGetter[Credentials] {
    def get(obj: Json): Credentials = {
      val key = obj.fieldOrEmptyString("Key").as[String].getOr("")
      val secret = obj.fieldOrEmptyString("Secret").as[String].getOr("")
      Credentials(key, secret)
    }
  }

  def get[T: JsonGetter](key: String): Option[T] = {
    val getter = implicitly[JsonGetter[T]]
    store.field(key).map(v => getter.get(v))
  }


  trait JsonArrayGetter[T] {
    def get(field: Json): Array[T]
  }

  implicit val stringArrayGetter: JsonArrayGetter[String] = new JsonArrayGetter[String] {
    def get(field: Json): Array[String] = field.arrayOrEmpty.toArray[Json].map(v => v.as[String].getOr(""))
  }

  def getArray[T: JsonArrayGetter](key: String): Option[Array[T]] = {
    val getter = implicitly[JsonArrayGetter[T]]
    store.field(key).map(v => getter.get(v))
  }
}
