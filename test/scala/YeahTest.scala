package scala

import play.api.mvc._
import play.api.test._
import play.api.test.Helpers._
import scala.concurrent.Future
import controllers.HomeController
import org.scalatestplus.play.PlaySpec

class YeahTest extends PlaySpec with Results {
  "HomeController" should {
    "work fine for the `banana` method" in {
      val ctrl = new HomeController(Helpers.stubControllerComponents())
      val result: Future[Result] = ctrl.banana(12).apply(FakeRequest())
      val resultStr = contentAsString(result)

      resultStr mustBe "Okay your long was 12"
    }
  }
}