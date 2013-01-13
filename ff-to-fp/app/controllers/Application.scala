package controllers

import play.api._
import play.api.mvc._

object Application extends Controller {
  import scala.util.Random.nextInt

  case class User(firstName:String, lastName:String, age:Int, id:String=java.util.UUID.randomUUID.toString)

  def alpha = ('a' to 'z').toList
  def randomString(n:Int) =
    (1 to n)
      .map {
        _ => alpha(nextInt(26))
      }
      .mkString
  val userList = List.fill(1000)(User(randomString(10), randomString(15), nextInt(114)))


  def index = Action {
    Ok("Get ready!")
  }

  def users = Action { request =>
    Ok(views.html.users(
      userList
        .groupBy(u => u.lastName.head) //group by first letter of the lastName
        .map { case (letter, list) =>
          //sorted by lastName | chunked by 10
          (letter, list.sortBy(_.lastName).sliding(10, 10).toList)
        }
    ))
  }

  def user(id:String) = Action {
    userList.find(_.id == id) match {
      case None => NotFound("user not found")
      case Some(user) => Ok(views.html.showUser(user))
    }
  }

  def byFirstName = Action { request =>
    val user:Option[User] = for {
      firstNames  <- request.queryString.get("firstname")
      firstName   <- firstNames.headOption
      user        <- userList.filter(_.firstName == firstName).headOption
      //user        <- userList.find(_.firstName == firstName)
    } yield user

    user match {
      case None => NotFound("user not found")
      case Some(user) => Ok(views.html.showUser(user))
    }

  }

}