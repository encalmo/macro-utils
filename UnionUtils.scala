package org.encalmo.utils

import scala.quoted.*

object UnionUtils {

  object TypeReprIsUnion {
    def unapply(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[List[quotes.reflect.TypeRepr]] = {
      import quotes.reflect.*
      // Helper to recursively flatten "A | B | C" into List(A, B, C)
      def flatten(tpe: TypeRepr): List[TypeRepr] = tpe.dealias match {
        case OrType(left, right) => flatten(left) ++ flatten(right)
        case other               => List(other)
      }

      tpe.dealias match {
        case OrType(_, _) => Some(flatten(tpe.dealias).distinct)
        case _            => None
      }
    }
  }

  /** Check if a type is a union. */
  def isUnion[A: Type](using Quotes): Boolean = {
    import quotes.reflect.*
    TypeRepr.of[A].dealias match {
      case OrType(_, _) => true
      case _            => false
    }
  }

  /** Visit an union and create a pattern match expression of each union type.
    *
    * @param valueExpr
    * @param functionExpr
    * @param quotes
    * @return
    *   Unit
    */
  def transformToMatchExpression[In: Type](
      label: Expr[String],
      valueExpr: Expr[In],
      functionExpr: [A: Type] => (Expr[String], Expr[A]) => Expr[Any]
  )(using quotes: Quotes): Expr[Unit] = {
    import quotes.reflect.*

    val matchCaseDefs =
      TypeUtils
        .inspectUnionType(TypeRepr.of[In])
        .getOrElse(Nil)
        .map { tpe =>
          val typeTree = tpe.asType match {
            case '[t] => TypeTree.of[t]
          }
          val matchCasePattern = Typed(Wildcard(), typeTree)
          val matchCaseBody =
            tpe.asType match {
              case '[t] =>
                functionExpr.apply[t](
                  label,
                  '{ $valueExpr.asInstanceOf[t] }
                )
            }

          CaseDef(matchCasePattern, None, matchCaseBody.asTerm)
        }

    Match(valueExpr.asTerm, matchCaseDefs).asExprOf[Unit]
  }

  /** Visit an union and create a pattern match term of each union type.
    *
    * @param valueExpr
    * @param functionExpr
    * @param cache
    * @return
    *   Unit
    */
  def transformToMatchTerm(using
      cache: StatementsCache
  )(
      tpe: cache.quotes.reflect.TypeRepr,
      valueTerm: cache.quotes.reflect.Term,
      functionOnCase: (cache.quotes.reflect.TypeRepr, cache.quotes.reflect.Term) => cache.quotes.reflect.Term
  ): cache.quotes.reflect.Term = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    val matchCaseDefs =
      TypeUtils
        .inspectUnionType(tpe)
        .getOrElse(Nil)
        .map { tpe =>
          val bindSym = Symbol.newBind(
            Symbol.spliceOwner,
            TypeNameUtils.valueNameOf(tpe),
            Flags.EmptyFlags,
            tpe
          )
          val typeCheckPattern = Typed(Wildcard(), Inferred(tpe))
          val matchCasePattern = Bind(bindSym, typeCheckPattern)
          val matchCaseBody = functionOnCase(tpe, Ref(bindSym))
          CaseDef(matchCasePattern, None, matchCaseBody)
        }

    Match(valueTerm, matchCaseDefs)
  }

}
