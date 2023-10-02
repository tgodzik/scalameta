package scala.meta.internal.macros

import scala.language.experimental.macros
import scala.annotation.implicitNotFound
import scala.reflect.ClassTag
import org.scalameta.internal.MacroHelpers
import scala.meta.internal.trees.Metadata.Ast

import scala.quoted._
import scala.meta.internal.trees.AstInfo

object AstInfoMacro{
  inline given materialize[T <: Ast: Type: ClassTag]: AstInfo[T] = ${ AstInfoMacros.materialize[T] }
}

object AstInfoMacros {

  def materialize[T <: Ast: Type: ClassTag](using Quotes): Expr[AstInfo[T]] = {
    import quotes.reflect._

    val tpe = TypeRepr.of[T]

    val QuasiSymbol = Symbol.requiredClass("scala.meta.internal.trees.Quasi")
    val TreeSymbol = Symbol.requiredClass("scala.meta.Tree")

    val QuasiFactory = {
      if (tpe <:< TypeRepr.of(using QuasiSymbol)) {
        Ref(tpe.typeSymbol.companionModule)
      } else if (tpe <:< TypeRepr.of(using TreeSymbol)) {
        '{ ${Ref(TreeSymbol.companionModule)}.Quasi }.asTerm
      } else {
        report.throwError(s"${tpe.show} is not an ast class and can't be used here.")
      }
    }

    '{
      new AstInfo[T] {
        def runtimeClass: Class[T] = summon[ClassTag[T]].runtimeClass.asInstanceOf[Class[T]]
        def quasi(rank: Int, tree: Tree): T with ${QuasiSymbol} = ${QuasiFactory.asExprOf[T with Quasi]}.apply(rank, tree)
      }
    }
  }
}