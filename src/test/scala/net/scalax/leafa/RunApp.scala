package net.scalax.leafa.slickimpl

import net.scalax.asuna.slick.umr.rmu.RepConverterUtils
import slick.jdbc.H2Profile.api._

import scala.concurrent.{duration, Await, Future}

case class Friends(
    id: Option[Long] = None
  , name: String
  , nick: Option[String]
  , age: Int
)

class FriendTable(tag: slick.lifted.Tag) extends Table[Friends](tag, "friend") {
  self =>

  def id       = column[Option[Long]]("id", O.AutoInc)
  def name()() = column[String]("name") //只要是无参可以直接调用的属性都可以通过，包括 val var
  def nick     = column[String]("nick").? //.? 变成 option 并不影响建表时需要 not null，请使用 Option type
  def age      = column[Int]("age")

  def pk2 = primaryKey("name", name()())

  def * = (id, name()(), nick, age).mapTo[Friends]

}

class FriendTable1234(tag: slick.lifted.Tag) extends Table[Unit](tag, "friend") {
  self =>

  def id                                       = column[Option[Long]]("id", O.AutoInc)
  def name()()()()()()()()()()()()()()()()()() = column[String]("name")
  def nick                                     = column[String]("nick")
  def age                                      = column[Int]("age")

  def pk2 = primaryKey("name", name()())

  def * = ()

}

object SqlGen extends App {

  def await[A](f: Future[A]): A = Await.result(f, duration.Duration.Inf)

  val friendTq = TableQuery[FriendTable]

  val db = Database.forURL(s"jdbc:h2:mem:leafaaa;DB_CLOSE_DELAY=-1", driver = "org.h2.Driver", keepAliveConnection = true)

  class AATable(friendTable: FriendTable1234) extends Table[Any](friendTable.tableTag, friendTable.tableName) {
    override def tableConstraints = friendTable.tableConstraints
    override def *                = RepConverterUtils.fromTable(friendTable)
  }

  val aaTq = TableQuery(cons => new AATable(new FriendTable1234(cons)))

  val bbTq = RepConverterUtils.fromInstance(cons => new FriendTable1234(cons))

  println(s"""
    |原 table 建表语句
    |${friendTq.schema.createStatements.toList}
    |macro 生成的建表语句
    |${aaTq.schema.createStatements.toList}
    |更高级的 macro 生成语句
    |${bbTq.schema.createStatements.toList}
    |是否相等1
    |${friendTq.schema.createStatements.toList == aaTq.schema.createStatements.toList}
    |是否相等2
    |${friendTq.schema.createStatements.toList == bbTq.schema.createStatements.toList}
    |""".stripMargin)

}
