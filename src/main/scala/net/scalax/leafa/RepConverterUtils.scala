package net.scalax.asuna.slick.umr.rmu

import net.scalax.leafa.RepConverter
import net.scalax.leafa.slickimpl.CollectMacro
import slick.jdbc.JdbcProfile
import slick.lifted._

import scala.language.experimental.macros
import scala.language.implicitConversions

trait TableExport[T <: AbstractTable[_]] {
  def convert(table: T): MappedProjection[Any, Any]
}

object TableExport {
  implicit def tableExportImplicit[T <: AbstractTable[_]]: TableExport[T] = macro CollectMacro.CollectMacroImpl.impl2[T]
}

trait RepConverterUtils {

  implicit final def anyToShapedValue[T, U](value: T)(implicit shape: Shape[_ <: FlatShapeLevel, T, U, _]): ShapedValue[T, U] = new ShapedValue[T, U](value, shape)

  def fromCol(r: MappedProjection[Any, Any]*): MappedProjection[Any, Any] = {
    val cv = implicitly[RepConverter[Unit]]
    val zero = (cv.convert(()), cv.convert(())).shaped
      .<>(s => s: Any, { _: Any =>
        Option.empty
      })
      .asInstanceOf[MappedProjection[Any, Any]]

    r.toList.foldLeft(zero) { (convert, item) =>
      (convert, item).shaped
        .<>(s => s: Any, { _: Any =>
          Option.empty
        })
        .asInstanceOf[MappedProjection[Any, Any]]
    }
  }

  def fromPro[T](r: => T)(implicit cv: RepConverter[T]): MappedProjection[Any, Any] = {
    lazy val r1 = r
    cv.convert(r1)
  }

  def fromTable[T](table: T): MappedProjection[Any, Any] = macro CollectMacro.CollectMacroImpl.impl[T]

  def fromInstance[T <: AbstractTable[_]](cons: Tag => T)(implicit cv: TableExport[T], jdbcProfile: JdbcProfile): TableQuery[jdbcProfile.Table[Any]] = {
    import jdbcProfile.api._
    val func = { tag: Tag =>
      val table = cons(tag)
      class TempTable extends Table[Any](table.tableTag, table.tableName) {
        override def tableConstraints = table.tableConstraints
        override def *                = cv.convert(table)
      }
      new TempTable
    }

    TableQuery(c => func(c))
  }

}

object RepConverterUtils extends RepConverterUtils
