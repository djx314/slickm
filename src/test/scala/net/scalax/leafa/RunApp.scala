package net.scalax.leafa.slickimpl

import net.scalax.asuna.slick.umr.rmu.RepConverterUtils
import slick.jdbc.H2Profile.api._

import scala.concurrent.{duration, Await, Future}

case class Friends(
    id: Option[Long] = None
  , name: String
  , nick: String
  , age: Int
)

class FriendTable(tag: slick.lifted.Tag) extends Table[Friends](tag, "friend") {
  self =>

  def id       = column[Option[Long]]("id", O.AutoInc)
  def name()() = column[String]("name")
  def nick     = column[String]("nick")
  def age      = column[Int]("age")

  def * = (id, name()(), nick, age).mapTo[Friends]

}

object SqlGen extends App {

  def await[A](f: Future[A]): A = Await.result(f, duration.Duration.Inf)

  val friendTq = TableQuery[FriendTable]

  val db = Database.forURL(s"jdbc:h2:mem:leafaaa;DB_CLOSE_DELAY=-1", driver = "org.h2.Driver", keepAliveConnection = true)

  class AATable(val friendTable: FriendTable) extends Table[Any](friendTable.tableTag, friendTable.tableName) {
    def * = RepConverterUtils.fromTable(friendTable)
  }

  val aaTq = TableQuery(cons => new AATable(new FriendTable(cons)))

  println(s"""
       |原 table 建表语句
       |${friendTq.schema.createStatements.toList}
       |macro 生成的建表语句
       |${aaTq.schema.createStatements.toList}
       |是否相等
       |${friendTq.schema.createStatements.toList == aaTq.schema.createStatements.toList}
     """.stripMargin)

}
