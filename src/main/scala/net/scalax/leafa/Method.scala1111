package net.scalax.asuna.slick.umr.rmu

import net.scalax.leafa.slickimpl.{BaseTypedTypeAst, LeftData, SqlColumn}
import slick.ast.{OptionTypedType, Type, TypedType}
import slick.jdbc.{JdbcType, PositionedParameters, SQLActionBuilder, SetParameter}
import slick.lifted.Rep

trait ExtMethod {

  import SimpleSlickHelper._

  trait ExtMoethods[T] {
    val base: SqlColumn[T]

    def ++[R](litValue: R)(implicit bt: TypedType[R]): SqlColumn[T] = {
      val sql = customSet(bt, litValue, isParam = false)
      SqlColumn(base.columnName, { t: T =>
        LeftData(base.dataToSql(t).sql concat sql""" + """ concat sql)
      })
    }

    def -[R](litValue: R)(implicit bt: TypedType[R]): SqlColumn[T] = {
      val sql = customSet(bt, litValue, isParam = false)
      SqlColumn(base.columnName, { t: T =>
        LeftData(base.dataToSql(t).sql concat sql""" - """ concat sql)
      })
    }

  }

  def liftCol[T](rep: Rep[T]): SqlColumn[T] = BaseTypedTypeAst(rep)

  def customSet[D](t: Type, data: D, isParam: Boolean): SQLActionBuilder = {

    t match {
      case optType: OptionTypedType[_] =>
        data match {
          case Some(innerData) =>
            customSet(optType.elementType, innerData, isParam)
          case None =>
            customSet(optType.elementType, null, isParam)
          case n if (n == null) =>
            throw new NullPointerException
        }
      case jdbcType: JdbcType[Any @unchecked] =>
        if (isParam) {
          implicit val setter = new SetParameter[Any] {
            override def apply(v1: Any, v2: PositionedParameters): Unit = {
              if (null == v1) {
                v2.setNull(jdbcType.sqlType)
              } else {
                val npos = v2.pos + 1
                jdbcType.setValue(v1, v2.ps, npos)
                v2.pos = npos
              }
            }
          }
          sql"""${data.asInstanceOf[Any]}"""
        } else {
          if (null == data) {
            implicit val setter = new SetParameter[Any] {
              override def apply(v1: Any, v2: PositionedParameters): Unit = {
                v2.setNull(jdbcType.sqlType)
              }
            }
            sql"""${null.asInstanceOf[Any]}"""
          } else
            sql"""#${t.asInstanceOf[JdbcType[Any]].valueToSQLLiteral(data)}"""
        }
    }

  }

}

object ExtMethod extends ExtMethod
