package net.scalax.leafa.slickimpl

import net.scalax.asuna.slick.umr.rmu.RepConverterUtils
import slick.jdbc._
import slick.lifted.MappedProjection

import scala.reflect.macros.blackbox.Context

case class LeftData(sql: SQLActionBuilder)

object CollectMacro {

  class CollectMacroImpl(val c: Context) {

    import c.universe._

    def impl[Table: c.WeakTypeTag](table: c.Expr[Table]): c.Expr[MappedProjection[Any, Any]] = {

      val tableType         = weakTypeOf[Table]
      val simpleSlickHelper = weakTypeOf[RepConverterUtils]

      val fieldNamesInTable = tableType.members
        .map { s =>
          if (s.isTerm && s.isPublic) {
            val term = s.asTerm
            if (term.isVar || term.isVal) {
              Option((s, 0))
            } else {
              if (term.isMethod) {
                val m = term.asMethod
                if (m.typeParams.size > 0)
                  Option.empty
                else {
                  if (m.paramLists.size == 0)
                    Option((s, 0))
                  else {
                    if (m.paramLists.forall(s => s.isEmpty))
                      Option((s, m.paramLists.size))
                    else Option.empty
                  }
                }
              } else {
                Option.empty
              }
            }
          } else {
            Option.empty
          }
        }
        .toList
        .reverse
        .collect { case Some(s) => s }

      def callMethod(tree: Tree): Tree = q"""${tree}()"""

      def callMethodForTimes(tree: Tree, times: Int): Tree = if (times > 0) callMethodForTimes(callMethod(tree), times - 1) else tree

      val q = c.Expr[MappedProjection[Any, Any]] {
        q"""${simpleSlickHelper.typeSymbol.companion}.fromCol(..${fieldNamesInTable.map {
          case (term, num) => q"""${simpleSlickHelper.typeSymbol.companion}.fromPro(${callMethodForTimes(q"""${table}.${TermName(term.name.toString.trim)}""", num)})"""
        }})"""
      }
      q
    }

  }

}
