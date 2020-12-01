package controllers

import javax.inject._
import scala.util.{Try,Success,Failure}

import play.api.libs.json.JsValue
import play.api.mvc._

import services.MessageService
import models.{CheckingAccount,AccountsInMemoryModel}


@Singleton
class MessageController @Inject()
  (messageService: MessageService, cc: ControllerComponents)
    extends AbstractController(cc) {

  private val INVALID = "invalid"
  private val EMPTY = "empty"
  private val NOTAUTHORIZED = "not authorized"

  private val accounts = new Object with CheckingAccount with AccountsInMemoryModel

  def printMessage: Action[JsValue] = Action(parse.json) { implicit request =>
    val message = (request.body \ "message").as[String].trim.replaceAll("[ \t]+", " ")
    val parts = message.split(" ").toList

    def money(double: Double): String = f"${double}%.2f"

    def print(message: String): Result =
      Ok(messageService.printToTerminal(message.toString))

    def accountCreate(account: String): Result = {
      val message =
        accounts.create(account) match {
          case Left(message) => Some(message)
          case Right(double) => Some(money(double))
        }
      print(message.get)
    }

    def accountClose(account: String): Result = {
      val message =
        accounts.close(account) match {
          case Left(message) => Some(message)
          case Right(double) => Some(money(double))
        }
      print(message.get)
    }

    def accountBalance(account: String): Result = {
      val message =
        accounts.balance(account) match {
          case Left(message) => Some(message)
          case Right(double) => Some(money(double))
        }
      print(message.get)
    }

    def accountTransaction(account: String, data: String): Result = {
      Try(data.toDouble) match {
        case Failure(_) => print(INVALID)
        case Success(double) =>
          val message =
            accounts.transaction(account, double) match {
              case Left(message) => Some(message)
              case Right(double) => Some(money(double))
            }
          print(message.get)
      }
    }

    parts match {
      case List(account, "create")  => accountCreate(account)
      case List(account, "close")   => accountClose(account)
      case List(account, "balance") => accountBalance(account)
      case List(account, data)      => accountTransaction(account, data)
      case _ => print(INVALID)
    }
  }

}
