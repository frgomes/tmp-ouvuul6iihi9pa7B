package controllers

import play.api.libs.json.Json
import play.api.test.Helpers._
import play.api.test._
import services.MessageService
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer

class MessageControllerSpec extends ControllerTestResources {

  implicit val sys = ActorSystem("MyTest")
  implicit val mat = ActorMaterializer()

  private val mockMessageService = mock[MessageService]
  private val messageController = new MessageController(mockMessageService, mockCC)

  "MessageController POST /print" should {

    def test(input: String, output: String) = {
      (mockMessageService.printToTerminal _).expects(*).onCall { s: String => s"Printed '${s}' to terminal" }.once()

      val request  = FakeRequest().withBody(messageJson(input))
      val response = messageController.printMessage.apply(request)
      status(response) shouldBe OK
      contentAsString(response) shouldBe s"Printed '${output}' to terminal"
    }

    "report invalid message in the absence of a command" in {
      test("", "invalid")
    }

    "report invalid message when a command has more than 2 tokens" in {
      test("joe 34.56 extra", "invalid")
    }

    "report invalid message when command after account is invalid" in {
      test("joe report", "invalid")
    }

    "be able to open accounts" in {
      test("joe create", "0.00")
      test("nancy create", "0.00")
    }

    "complain on an attempt to open an already existing account" in {
      test("joe create", "transaction declined")
    }

    "be able to inform the balance of an existing account" in {
      test("joe balance", "0.00")
      test("nancy balance", "0.00")
    }

    "complain on an attempt to obtain the balance of a non existent account" in {
      test("lucy balance", "unknown account")
    }

    "allow operations of depositing and withdrawing funds from existing accounts" in {
      test("nancy  20000",    "20000.00")
      test("joe       10.23",    "10.23")
      test("joe       20.23",    "30.46")
      test("joe       30.00",    "60.46")
      test("nancy     10.23", "20010.23")
      test("nancy     20.23", "20030.46")
      test("nancy     30.00", "20060.46")
      test("joe      -10.23",    "50.23")
      test("joe      -50.23",     "0.00")
      test("nancy    -10.23", "20050.23")
      test("nancy    -50.23", "20000.00")
    }

    "complain on insufficient funds" in {
      test("joe                  -0.01", "transaction declined")
      test("nancy    -1000000000000.00", "transaction declined")
    }

    "should work when depositing or withdrawing zero funds, even though this does not make sense" in {
      test("joe    0.00",     "0.00")
      test("joe   -0.00",     "0.00")
      test("nancy  0.00", "20000.00")
      test("nancy -0.00", "20000.00")
    }

    "allow closing an account which balance is zero" in {
      test("joe close", "0.00")
    }

    "complain on an attempt to close an account which balance is not zero" in {
      test("nancy close", "transaction declined")
    }

    "complain on an attempt to close a non-exitend account" in {
      test("lucy close", "unknown account")
      test("paul close", "unknown account")
    }

  }
}
