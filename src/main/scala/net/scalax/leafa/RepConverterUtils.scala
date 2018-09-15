package net.scalax.asuna.slick.umr.rmu

import net.scalax.leafa.RepConverter
import net.scalax.leafa.slickimpl.CollectMacro
import slick.lifted.{FlatShapeLevel, MappedProjection, Shape, ShapedValue}
import scala.language.experimental.macros
import scala.language.implicitConversions

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

}

object RepConverterUtils extends RepConverterUtils
