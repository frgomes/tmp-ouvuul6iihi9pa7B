package models

import support.UnitSpec

class AccountsInMemoryModelSpec extends UnitSpec {

  import AccountsInMemoryModel.{NOTFOUND, DECLINED, REJECTED}

  "AccountsInMemoryModel" should {

    "detect a non-existent account" in {
      val accounts = new Object with CheckingAccount with AccountsInMemoryModel
      accounts.balance("joe") shouldBe Left(NOTFOUND)
    }

    "be able to create accounts" in {
      val accounts = new Object with CheckingAccount with AccountsInMemoryModel

      accounts.balance("joe") shouldBe Left(NOTFOUND)
      accounts.create("joe") shouldBe Right(0.00)

      accounts.balance("nancy") shouldBe Left(NOTFOUND)
      accounts.create("nancy") shouldBe Right(0.00)
    }

    "decline double account creation" in {
      val accounts = new Object with CheckingAccount with AccountsInMemoryModel
      accounts.balance("joe") shouldBe Left(NOTFOUND)
      accounts.create("joe") shouldBe Right(0.00)
      accounts.create("joe") shouldBe Left(DECLINED)
    }

    "perform transactions on multiple accounts" in {
      val accounts = new Object with CheckingAccount with AccountsInMemoryModel
      accounts.balance("joe") shouldBe Left(NOTFOUND)
      accounts.balance("ian") shouldBe Left(NOTFOUND)
      accounts.create("ian") shouldBe Right(0.00)
      accounts.create("joe") shouldBe Right(0.00)

      accounts.transaction("joe", 10.00) shouldBe Right(10.00)
      accounts.transaction("ian", 10.00) shouldBe Right(10.00)

      accounts.balance("ian") shouldBe Right(10.0)
      accounts.balance("joe") shouldBe Right(10.0)

      accounts.transaction("joe", 10.00) shouldBe Right(20.00)
      accounts.transaction("ian", 10.00) shouldBe Right(20.00)

      accounts.transaction("joe", 10.00) shouldBe Right(30.00)
      accounts.transaction("ian", 10.00) shouldBe Right(30.00)

      accounts.transaction("ian", -10.00) shouldBe Right(20.00)
      accounts.transaction("joe", -10.00) shouldBe Right(20.00)

      accounts.transaction("joe", -10.00) shouldBe Right(10.00)
      accounts.transaction("ian", -10.00) shouldBe Right(10.00)

      accounts.transaction("joe", -15.00) shouldBe Left(DECLINED)
      accounts.transaction("ian", -10.00) shouldBe Right(0.00)

      accounts.balance("ian") shouldBe Right( 0.0)
      accounts.balance("joe") shouldBe Right(10.0)

      accounts.transaction("joe", -10.01) shouldBe Left(DECLINED)
      accounts.transaction("ian",  -0.01) shouldBe Left(DECLINED)

      accounts.balance("ian") shouldBe Right( 0.0)
      accounts.balance("joe") shouldBe Right(10.0)
    }

    "be able to close accounts" in {
      val accounts = new Object with CheckingAccount with AccountsInMemoryModel
      accounts.balance("joe") shouldBe Left(NOTFOUND)
      accounts.balance("ian") shouldBe Left(NOTFOUND)
      accounts.create("ian") shouldBe Right(0.00)
      accounts.create("joe") shouldBe Right(0.00)

      accounts.transaction("ian", 10.00) shouldBe Right(10.00)

      accounts.close("joe") shouldBe Right(0.00)
      accounts.close("ian") shouldBe Left(DECLINED)
    }

    "be able to deposit up to the limit value" in {
      val accounts = new Object with CheckingAccount with AccountsInMemoryModel
      accounts.balance("bezos") shouldBe Left(NOTFOUND)
      accounts.create("bezos") shouldBe Right(0.00)

      accounts.transaction("bezos", accounts.limit).isRight shouldBe true
      accounts.transaction("bezos", 0.01) shouldBe Left(DECLINED)
    }

    "be able to detect Forex transaction on a checking account" in {
      val accounts = new Object with CheckingAccount with AccountsInMemoryModel
      accounts.balance("lucy") shouldBe Left(NOTFOUND)
      accounts.create("lucy") shouldBe Right(0.00)

      accounts.transaction("lucy", 10.001) shouldBe Left(REJECTED)
    }

  }
}
