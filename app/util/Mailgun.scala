package util

import play.api.libs.ws.{WSAuthScheme, WSClient}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

class Mailgun(ws: WSClient, domain: String, apiKey: String) {
  val API_URL = s"https://api.mailgun.net/v3/${domain}"

  def sendEmail(recipient: String, subject: String, body: String)(implicit ec: ExecutionContext): Future[Unit] =
    send(recipient, subject, body)

  def sendEmailSync(recipient: String, subject: String, body: String, delay: Duration = 30 seconds)(implicit ec: ExecutionContext): Unit = {
    val f = send(recipient, subject, body)

    Await.result(f, delay)
  }

  private def send(recipient: String, subject: String, body: String)(implicit ec: ExecutionContext): Future[Unit] = {
    println(s"Sending stuff to ${recipient}")
    val request = ws
      .url(s"${API_URL}/messages")
      .withAuth("api", apiKey, WSAuthScheme.BASIC)
      .withFollowRedirects(true)
      .post(Map(
        "to" -> Seq(recipient),
        "from" -> Seq("test@domynation.com"),
        "subject" -> Seq(subject),
        "text" -> Seq(body),
      ))

    request.map(_ => ())
  }
}
