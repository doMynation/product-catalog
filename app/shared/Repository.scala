package shared

import inventory.util.SearchRequest

import scala.concurrent.Future

trait Repository[T] {

  //  def get(id: Long): Future[Option[T]]

//  def get(id: Identifier, lang: String): Future[Option[T]]
//
//  def create(entity: T): Long
//
//  def update(entity: T): Long

  def search(sr: SearchRequest, inclusions: Seq[String] = Seq.empty[String]): Future[Seq[T]]
}
