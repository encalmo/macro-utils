package org.encalmo.utils

import scala.quoted.*

object SelectableUtils {

  object TypeReprIsSelectable {
    def unapply(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[quotes.reflect.TypeRepr] = {
      import quotes.reflect.*
      val tped = tpe.dealias
      if (tped <:< TypeRepr.of[Selectable]) {
        findFieldsType(tped)
      } else {
        None
      }
    }
  }

  /** Find the tuple type of the fields of a Selectable. */
  def findFieldsType(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[quotes.reflect.TypeRepr] =
    import quotes.reflect.*
    tpe match {
      case Refinement(_, "Fields", bounds: TypeBounds) => Some(bounds.hi)
      case Refinement(parent, _, _)                    => findFieldsType(parent)
      case _                                           =>
        val fields = tpe.typeSymbol.typeMember("Fields")
        if (fields.exists)
        then {
          tpe.memberType(fields) match {
            case bounds: TypeBounds => Some(bounds.hi)
            case _                  => None
          }
        } else {
          None
        }
    }

  /** Check if a type is a Selectable and apply a function eventually returning a block of unit. */
  def maybeTransformSelectableIntoBlockOfUnit[T: Type](using
      Quotes
  )(
      functionExpr: [Fields: Type] => Quotes ?=> Expr[Unit]
  ): Option[Expr[Unit]] = {
    import quotes.reflect.*
    val tpe = TypeRepr.of[T].dealias
    if (tpe <:< TypeRepr.of[Selectable]) {
      findFieldsType(tpe)
        .map { fieldsType =>
          fieldsType.asType match {
            case '[fields] => functionExpr.apply[fields]
          }
        }
    } else {
      None
    }
  }

  /** Transform the fields of a Selectable into a block of unit by applying a function to each field. */
  def transformFieldsIntoBlockOfUnit[In: Type, Fields: Type](using
      Quotes
  )(
      valueExpr: Expr[In],
      functionExpr: [A: Type] => (Expr[String], Expr[A]) => Expr[Unit]
  ): Expr[Unit] = {
    import quotes.reflect.*
    TypeRepr.of[Fields].dealias match {
      case AppliedType(_, List(AppliedType(_, nameTypeList), AppliedType(_, valueTypeList))) =>

        val term = valueExpr.asTerm
        val selectDynamicSym = term.tpe.typeSymbol
          .methodMember("selectDynamic")
          .headOption
          .getOrElse(report.errorAndAbort(s"Method selectDynamic not found on ${term.tpe.show}"))

        Expr.block(
          nameTypeList
            .zip(valueTypeList)
            .zipWithIndex
            .map { case ((nameType, valueType), index) =>
              val name = TypeNameUtils.shortBaseName(nameType.show(using Printer.TypeReprShortCode))
              valueType.asType match {
                case '[value] =>
                  functionExpr.apply[value](
                    Expr(name), {
                      val arg = Literal(StringConstant(name))
                      val methodCall = Apply(Select(term, selectDynamicSym), List(arg))
                      '{ ${ methodCall.asExpr }.asInstanceOf[value] }.asExprOf[value]
                    }
                  )
              }
            },
          '{}
        )

      case _ =>
        '{}
    }
  }

  /** Check if a type is a Selectable and eventually apply a function using a statements cache. */
  def maybeVisitSelectable(using
      cache: StatementsCache
  )(
      tpe: cache.quotes.reflect.TypeRepr,
      functionWhenSelectable: cache.quotes.reflect.TypeRepr => Unit
  ): Option[Unit] = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    val tped = tpe.dealias
    if (tped <:< TypeRepr.of[Selectable]) {
      findFieldsType(tped)
        .foreach { fieldsType => functionWhenSelectable(fieldsType) }
      Some(())
    } else {
      None
    }
  }

  /** Visit the fields of a Selectable and apply a function to each field using a statements cache. */
  def visitFields(using
      cache: StatementsCache
  )(
      fieldsTpe: cache.quotes.reflect.TypeRepr,
      valueTerm: cache.quotes.reflect.Term,
      functionOnField: (cache.quotes.reflect.TypeRepr, String, cache.quotes.reflect.Term) => Unit
  ): Unit = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    fieldsTpe.dealias match {
      case AppliedType(_, List(AppliedType(_, nameTypeList), AppliedType(_, valueTypeList))) =>

        val selectDynamicSym = valueTerm.tpe.typeSymbol
          .methodMember("selectDynamic")
          .headOption
          .getOrElse(report.errorAndAbort(s"Method selectDynamic not found on ${valueTerm.tpe.show}"))

        nameTypeList
          .zip(valueTypeList)
          .zipWithIndex
          .map { case ((nameType, valueTpe), index) =>
            val name = TypeNameUtils.shortBaseName(nameType.show(using Printer.TypeReprShortCode))
            functionOnField(
              valueTpe,
              name, {
                val arg = Literal(StringConstant(name))
                Apply(Select(valueTerm, selectDynamicSym), List(arg))
              }
            )
          }

      case _ =>
        '{}
    }
  }
}
