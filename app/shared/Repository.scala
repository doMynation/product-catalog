package shared

import inventory.util.SearchRequest

import scala.concurrent.Future

trait Repository[T] {

  def get(id: Long): Option[T]

  def search(sr: SearchRequest, inclusions: Seq[String] = Seq.empty[String]): Future[Seq[T]]
}
