package controllers

import java.security.{KeyFactory, Signature}
import java.security.spec.X509EncodedKeySpec
import java.util.Base64
import javax.inject._

import play.api.Play
import play.api.mvc._

import scala.io.Source

/**
  * This controller creates an `Action` to handle HTTP requests to the
  * application's home page.
  */
@Singleton
class HomeController @Inject()(cc: ControllerComponents) extends AbstractController(cc) {

  def verify(data: String) = Action { req =>
    val path = s"${Play.unsafeApplication.path.getAbsolutePath}/public/pub.pem"
    //    val bytes =
    //    val tmp = bytes.toString.replace("-----BEGIN PUBLIC KEY-----\n", "").replace("-----END PUBLIC KEY-----", "")
    //    val pemBytes = Base64.getDecoder.decode(tmp)
    //
    val pem = Source.fromFile(path).getLines.mkString
    val tmp = pem.replace("-----BEGIN PUBLIC KEY-----", "").replace("-----END PUBLIC KEY-----", "")
    println(pem)
    println(tmp)

    val pubKeySpec = new X509EncodedKeySpec(Base64.getDecoder.decode(tmp))
    val keyFactory = KeyFactory.getInstance("RSA")
    val publicKey = keyFactory.generatePublic(pubKeySpec)
    val sig = Signature.getInstance("SHA1withRSA")
    System.out.println(publicKey)

//    val hash = "cmKSZUinCRU1UADTL8lBM8Jc6NnoSTCZhJ1ZJp+ZIDsOal0hSLYUkkzRyEz24z5kRiDvDzHr33ObvU5LQXrvGnqkY4+23rfvh8YJlIDvuN1RGNaDAnlFCt3J5TvqS6u7fwqMLoZ2XwguzYiBc1oo8YWEZvYoMvlbIaK4K374h5w="
    val hash = "cmKSZUinCRU1UADTL8lBM8Jc6NnoSTCZhJ1ZJp+ZIDsOal0hSLYUkkzRyEz24z5kRiDvDzHr33ObvU5LQXrvGnqkY4+23rfvh8YJlIDvuN1RGNaDAnlFCt3J5TvqS6u7fwqMLoZ2XwguzYiBc1oo8YWEZvYoMvlbIaK4K374h5w="
    val hashBytes = Base64.getDecoder.decode(hash)
    println(hashBytes)
//    val data = "1513828471317|theglendale|9252"
    val dataBytes = data.getBytes("UTF-16LE")

    println(data, hash)

    sig.initVerify(publicKey)
    sig.update(dataBytes)

    if (sig.verify(hashBytes)) {
      Ok("OMG VERIFIED")
    } else Ok("NOPE")
  }

  def index() = Action { req =>
    Ok(views.html.index("Your new application is ready."))
  }

  def banana(id: Long) = Action { req =>
    Ok(s"Okay your long was ${id} and your ip is ${req.remoteAddress}")
  }
}
