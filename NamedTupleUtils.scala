package org.encalmo.utils

import scala.quoted.*

object NamedTupleUtils {

  object TypeReprIsNamedTuple {
    def unapply(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean = {
      tpe.dealias.asType match {
        case '[NamedTuple.AnyNamedTuple] => true
        case _                           => false
      }
    }
  }

  def isNamedTuple[A: Type](using Quotes): Boolean =
    Type.of[A] match {
      case '[NamedTuple.AnyNamedTuple] => true
      case _                           => false
    }

  /** Visit a named tuple and collect the results into a block of unit.
    *
    * @param valueExpr
    * @param functionExpr
    * @param quotes
    * @return
    *   Unit
    */
  def collect[In: Type](using
      quotes: Quotes
  )(
      valueExpr: Expr[In],
      functionOnField: [A: Type] => Quotes ?=> (
          Option[Expr[String]],
          Expr[A],
          Int
      ) => Expr[Any]
  ): Expr[Unit] = {
    import quotes.reflect.*

    Type.of[In] match {
      case '[NamedTuple.AnyNamedTuple] =>
        TypeRepr.of[In].dealias match {
          case AppliedType(_, List(AppliedType(_, nameTypeList), AppliedType(_, valueTypeList))) =>
            Expr.block(
              nameTypeList
                .zip(valueTypeList)
                .zipWithIndex
                .map { case ((name, valueType), index) =>
                  valueType.asType match {
                    case '[value] =>
                      functionOnField.apply[value](
                        Some(Expr(TypeNameUtils.shortBaseName(name.show(using Printer.TypeReprShortCode)))),
                        '{ $valueExpr.asInstanceOf[Product].productElement(${ Expr(index) }).asInstanceOf[value] },
                        index
                      )
                  }
                },
              '{}
            )
          case _ => '{}
        }

      case _ =>
        '{}
    }
  }

  /** Visit a named tuple using a statements cache.
    *
    * @param valueExpr
    * @param functionExpr
    * @param cache
    * @return
    *   Unit
    */
  def visit(using
      cache: StatementsCache
  )(
      tpe: cache.quotes.reflect.TypeRepr,
      valueTerm: cache.quotes.reflect.Term,
      functionOnField: (
          cache.quotes.reflect.TypeRepr,
          String,
          cache.quotes.reflect.Term,
          Int
      ) => Unit
  ): Unit = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    tpe.dealias.asType match {
      case '[NamedTuple.AnyNamedTuple] =>
        val productElementMethodSym = MethodUtils.findMethodByArity(TypeRepr.of[Product], "productElement", 1)
        tpe.dealias.widen match {
          case AppliedType(_, List(AppliedType(_, nameTypeList), AppliedType(_, valueTypeList))) =>
            nameTypeList
              .zip(valueTypeList)
              .zipWithIndex
              .foreach { case ((nameTpe, valueTpe), index) =>
                functionOnField(
                  valueTpe,
                  TypeNameUtils.shortBaseName(nameTpe.show(using Printer.TypeReprShortCode)),
                  Apply(
                    Select(valueTerm.callAsInstanceOf[Product], productElementMethodSym),
                    List(Literal(IntConstant(index)))
                  ).callAsInstanceOf(Inferred(valueTpe)),
                  index
                )
              }
          case _ => ()
        }

      case _ => ()
    }
  }

  /** Visit a named tuple using a statements cache.
    *
    * @param functionExpr
    * @param cache
    * @return
    *   Unit
    */
  def visitTermless(using
      cache: StatementsCache
  )(
      tpe: cache.quotes.reflect.TypeRepr,
      functionOnField: (
          cache.quotes.reflect.TypeRepr,
          String,
          Int
      ) => Unit
  ): Unit = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    tpe.dealias.asType match {
      case '[NamedTuple.AnyNamedTuple] =>
        val productElementMethodSym = MethodUtils.findMethodByArity(TypeRepr.of[Product], "productElement", 1)
        tpe.dealias.widen match {
          case AppliedType(_, List(AppliedType(_, nameTypeList), AppliedType(_, valueTypeList))) =>
            nameTypeList
              .zip(valueTypeList)
              .zipWithIndex
              .foreach { case ((nameTpe, valueTpe), index) =>
                functionOnField(
                  valueTpe,
                  TypeNameUtils.shortBaseName(nameTpe.show(using Printer.TypeReprShortCode)),
                  index
                )
              }
          case _ => ()
        }

      case _ => ()
    }
  }

}
