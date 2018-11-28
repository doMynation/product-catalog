package inventory.util

import java.nio.file.Paths
import java.util.UUID
import org.apache.commons.io.FilenameUtils
import play.api.libs.Files
import play.api.libs.Files.TemporaryFile
import play.api.mvc.MultipartFormData.FilePart

class FileUploader(uploadPath: String) {

  /**
    * Uploads a given file to the upload directory with a specific name.
    *
    * @param file
    * @param newName
    * @return
    */
  def upload(file: FilePart[Files.TemporaryFile], newName: String): TemporaryFile = {
    file.ref.moveTo(Paths.get(s"$uploadPath/$newName"), true)
  }

  /**
    * Uploads a given file to the upload directory with a random name.
    *
    * @param file
    * @return
    */
  def upload(file: FilePart[Files.TemporaryFile]): TemporaryFile = {
    val fileName = Paths.get(file.filename).getFileName
    val ext = FilenameUtils.getExtension(fileName.toString)
    val newFileName = s"${UUID.randomUUID().toString}.$ext"

    upload(file, newFileName)
  }
}
