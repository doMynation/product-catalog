package util

import play.api.libs.ws.{WSAuthScheme, WSClient}
import scala.concurrent.{ExecutionContext, Future}

class Mailgun(ws: WSClient, domain: String, apiKey: String) {
  val API_URL = s"https://api.mailgun.net/v3/${domain}"

  def sendEmail(recipient: String, subject: String, body: String)(implicit ec: ExecutionContext): Future[Unit] = {
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
