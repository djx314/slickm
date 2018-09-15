package net.scalax.leafa

import slick.lifted._

trait RepConverter[T] {
  def convert(rep: => T): MappedProjection[Any, Any]
}

object RepConverter extends RepConverter1

trait RepConverter1 extends RepConverter2 {
  implicit def convert1[L <: FlatShapeLevel, T, D, R](implicit shape: Shape[L, Rep[T], D, R]): RepConverter[Rep[T]] = new RepConverter[Rep[T]] {
    override def convert(rep: => Rep[T]): MappedProjection[Any, Any] = {
      ShapedValue(rep, shape)
        .<>(s => s: Any, { _: Any =>
          Option.empty
        })
        .asInstanceOf[MappedProjection[Any, Any]]
    }
  }
}

trait RepConverter2 {
  implicit def convert2[T]: RepConverter[T] = new RepConverter[T] {

    override def convert(notToUse: => T): MappedProjection[Any, Any] = {
      ShapedValue((), implicitly[Shape[FlatShapeLevel, Unit, Unit, Unit]])
        .<>(s => s: Any, { _: Any =>
          Option.empty
        })
        .asInstanceOf[MappedProjection[Any, Any]]
    }
  }
}
