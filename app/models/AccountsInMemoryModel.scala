package models

import javax.annotation.concurrent.ThreadSafe

// As a best practice, when dealing with money always employ fixed point arithmetic
// instead of floating point arithmetic.
//
// The public API exposes Double values, since external components may probably find
// float values far more convenient for data input and output. However, all calculations
// involving money must be necessarily be performed using fixed point arithmetic.
//
// This is actually overkill here, since we are not performing any operation which
// could possibly cause floating point round errors. However, a good design is a good
// design from day one.

/** We are mimicking a database record here  */
private case class Account(account: String, balance: Long)


sealed trait AccountProfile {
  def factor: Double
  def limit : Double =
    Long.MaxValue *
      Math.max(AccountProfile.CHECKING_FACTOR, AccountProfile.FOREX_FACTOR)
}
private object AccountProfile {
  val CHECKING_FACTOR = 0.01
  val FOREX_FACTOR = 0.00001
}

trait CheckingAccount extends AccountProfile {
  override def factor = AccountProfile.CHECKING_FACTOR
}
trait ForexAccount extends AccountProfile {
  override def factor = AccountProfile.FOREX_FACTOR
}


@ThreadSafe
trait AccountsInMemoryModel {
  profile: AccountProfile =>

  import AccountsInMemoryModel.{NOTFOUND, DECLINED, REJECTED}

  import scala.collection._
  import scala.collection.JavaConverters._
  import java.util.concurrent.ConcurrentHashMap

  private val map: concurrent.Map[String, Account] = new ConcurrentHashMap().asScala

  def create(account: String): Either[String, Double] =
    map.get(account) match {
      case Some(c) => Left(DECLINED)
      case None =>
        map.putIfAbsent(account, Account(account, 0)) match {
          case None => Right(0.00f)
          case _ => Left(DECLINED)
        }
    }

  def close(account: String): Either[String, Double] =
    map.get(account) match {
      case None => Left(NOTFOUND)
      case Some(c) =>
        if (c.balance != 0)
          Left(DECLINED)
        else if (map.remove(account, Account(account, 0)))
          Right(0.00f)
        else
          Left(DECLINED)
    }

  def balance(account: String): Either[String, Double] =
    map.get(account) match {
      case None => Left(NOTFOUND)
      case Some(c) => _convert(c.balance)
    }

  def transaction(account: String, amount: Double): Either[String, Double] =
    map.get(account) match {
      case None => Left(NOTFOUND)
      case Some(o) =>
        _transaction(o, amount)
          .flatMap(n =>
            if (!map.replace(account, o, n))
              Left(DECLINED)
            else
              _convert(n.balance))
    }

  private def _transaction(c: Account, amount: Double): Either[String, Account] = {
    val double: Double = amount / profile.factor
    if (_invalid(double))
      Left(DECLINED)
    else if (!double.isWhole)
      Left(REJECTED)
    else {
      val balance = c.balance + double.toLong
      if (balance < 0) {
        // println(s"********* ${c.account} -- ${double} -- ${profile.limit}")
        Left(DECLINED)
      } else
          Right(Account(c.account, balance))
    }
  }

  private def _convert(amount: Long): Either[String, Double] =
    Right(amount * profile.factor)

  private def _invalid(double: Double): Boolean =
    double.isNaN || double.isInfinity || double.isNegInfinity || double.isPosInfinity
}
object AccountsInMemoryModel {
  val NOTFOUND = "unknown account"
  val DECLINED = "transaction declined"
  val REJECTED = "transaction rejected"
}
