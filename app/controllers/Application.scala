package controllers

import play.api.mvc._

import scala.collection.mutable.Map

object Application extends Controller {

  val Alphabet = Range('A', 'Z') union Range('a', 'z') union Range('0', '9')
  val URLMap = Map[String, String]()

  def encode(url: String): String = {
    var seed = url.map(x => {x.toInt}).reduce((x,y) => {x + y})
    val urlKey = ""
    while(seed > 0) {
      val remainder = seed % Alphabet.size
      urlKey concat Alphabet(remainder).toString
      seed /= Alphabet.size
    }
    val decoded = URLMap get urlKey
    decoded match {
      case Some(x) => urlKey
      case None => URLMap + (urlKey -> url)
    }
    urlKey
  }

  def decode(id: String): String = {
    val decoded = URLMap get id
    decoded match {
      case Some(x) => x
      case None => id
    }
  }

  def getShort(longURL: String) = Action {
    val encodedUrl = encode(longURL)
    Ok(encodedUrl)
  }

  def redirectShort(id: String) = Action {
    Redirect(decode(id), 301)
  }

}