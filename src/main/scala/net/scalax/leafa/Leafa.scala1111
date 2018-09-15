package net.scalax.asuna.slick.umr.rmu

import net.scalax.asuna.core.encoder.EncoderShape
import net.scalax.asuna.helper.encoder.{EncoderContent, EncoderWrapperHelper}
import net.scalax.leafa.slickimpl._
import slick.ast.TableNode
import slick.dbio.DBIO
import slick.jdbc.JdbcProfile
import slick.lifted.Rep

trait RmuWriterQuery {

  trait Abc[DataType] {
    def inputData(param: DataType): DBIO[Int]
  }
  trait WithCols[RepOut, DataType] extends EncoderContent[RepOut, DataType] {
    def withCols(tableNode: TableNode)(implicit jdbcProfile: JdbcProfile): Abc[DataType]
  }

  object rmu extends EncoderWrapperHelper[List[SqlColumnBase], List[Any], WithCols] {
    override def effect[Rep, D, Out](rep: Rep)(implicit shape: EncoderShape.Aux[Rep, D, Out, List[SqlColumnBase], List[Any]]): WithCols[Out, D] = {
      val wrapCol = shape.wrapRep(rep)
      val reps    = shape.toLawRep(wrapCol, List.empty)
      new WithCols[Out, D] {
        override def withCols(tableNode: TableNode)(implicit jdbcProfile: JdbcProfile): Abc[D] = {

          val tableNode1 = tableNode
          val tableInsert = new SimpleInsert {
            override val typedTypeAstList = reps
            override val tableName        = tableNode1
            override val profile          = jdbcProfile
          }

          new Abc[D] {
            override def inputData(param: D): DBIO[Int] = {
              tableInsert.takeColumn(shape.buildData(param, wrapCol, List.empty)).sql.asUpdate
            }
          }
        }
      }
    }
  }

  implicit def rmuImplicit[R]: EncoderShape.Aux[Rep[R], R, BaseTypedTypeAst[R], List[SqlColumnBase], List[Any]] = {
    new EncoderShape[Rep[R], List[SqlColumnBase], List[Any]] {
      override type Target = BaseTypedTypeAst[R]
      override type Data   = R
      override def wrapRep(base: Rep[R]): BaseTypedTypeAst[R]                                            = BaseTypedTypeAst(base)
      override def toLawRep(base: BaseTypedTypeAst[R], oldRep: List[SqlColumnBase]): List[SqlColumnBase] = base :: oldRep
      override def buildData(data: R, rep: BaseTypedTypeAst[R], oldData: List[Any]): List[Any]           = data :: oldData
    }
  }

  implicit def rmuImplicitForSqlColumn[R]: EncoderShape.Aux[SqlColumn[R], R, SqlColumn[R], List[SqlColumnBase], List[Any]] = {
    new EncoderShape[SqlColumn[R], List[SqlColumnBase], List[Any]] {
      override type Target = SqlColumn[R]
      override type Data   = R
      override def wrapRep(base: SqlColumn[R]): SqlColumn[R]                                      = base
      override def toLawRep(base: SqlColumn[R], oldRep: List[SqlColumnBase]): List[SqlColumnBase] = base :: oldRep
      override def buildData(data: R, rep: SqlColumn[R], oldData: List[Any]): List[Any]           = data :: oldData
    }
  }

}
