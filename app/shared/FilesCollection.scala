package shared

import play.api.libs.json.{JsArray, JsValue, Json, Writes}
import shared.entities.File

object FilesCollection {
  implicit val filesWrites: Writes[FilesCollection] = new Writes[FilesCollection] {
    override def writes(o: FilesCollection): JsValue = JsArray(o.files.map(Json.toJson(_)))
  }
}

case class FilesCollection(files: Seq[File]) extends Includable
