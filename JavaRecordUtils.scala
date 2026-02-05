package org.encalmo.utils

import scala.quoted.*

object JavaRecordUtils {

  object TypeReprIsJavaRecord {
    def unapply(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean = {
      import quotes.reflect.*
      val recordType = TypeRepr.typeConstructorOf(classOf[java.lang.Record])
      tpe.dealias <:< recordType
    }
  }

  def isJavaRecord[A: Type](using Quotes): Boolean = {
    import quotes.reflect.*
    val recordType = TypeRepr.typeConstructorOf(classOf[java.lang.Record])
    TypeRepr.of[A].dealias <:< recordType
  }

  /** Visit a Java record type and collect the results into a block of unit.
    *
    * @param valueExpr
    * @param functionExpr
    * @param quotes
    * @return
    *   Unit
    */
  def collect[In: Type](
      label: Expr[String],
      valueExpr: Expr[In],
      functionExpr: [A: Type] => (Expr[String], Expr[A]) => Expr[Any]
  )(using quotes: Quotes): Expr[Unit] = {
    import quotes.reflect.*

    val tpe = TypeRepr.of[In]
    val sym = tpe.typeSymbol
    val recordType = TypeRepr.typeConstructorOf(classOf[java.lang.Record])

    if (!(tpe <:< recordType)) then '{}
    else {
      val primaryCtor = sym.primaryConstructor
      if (primaryCtor.isNoSymbol) {
        report.errorAndAbort("Could not find Java Record primary constructor")
      }
      val components = primaryCtor.paramSymss.headOption.getOrElse(Nil)
      Expr.block(
        components.map { paramSym =>
          val name = paramSym.name
          val accessorMethod = sym.methodMember(name).headOption.getOrElse {
            report.errorAndAbort(s"Could not find accessor method for Java Record component: $name")
          }
          val methodType = tpe.memberType(accessorMethod)
          val returnType: TypeRepr = methodType match {
            // Standard method: def foo(x: Int): String  => MethodType(..., String)
            case mt: MethodType => mt.resType
            // Generic method: def foo[A]: A => PolyType
            case pt: PolyType =>
              pt.resType match {
                case mt: MethodType => mt.resType // Unpack the underlying method
                case other          => other
              }
            // Parameterless method (in some contexts) or ByName
            case byName: ByNameType => byName.underlying
            // Fallback (e.g. it was already a value type)
            case other => other
          }
          returnType.asType match {
            case '[t] =>
              val valueAccess = Apply(Select(valueExpr.asTerm, accessorMethod), Nil).asExprOf[t]
              functionExpr.apply[t](Expr(name), '{ ${ valueAccess }.asInstanceOf[t] })
          }
        },
        '{}
      )
    }
  }

  /** Visit a Java record type and collect the results into a block of unit.
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
      label: String,
      tpe: cache.quotes.reflect.TypeRepr,
      valueTerm: cache.quotes.reflect.Term,
      functionOnField: (cache.quotes.reflect.TypeRepr, String, cache.quotes.reflect.Term) => Unit
  ): Unit = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    val sym = tpe.typeSymbol
    val recordType = TypeRepr.typeConstructorOf(classOf[java.lang.Record])

    if (!(tpe <:< recordType)) then '{}
    else {
      val primaryCtor = sym.primaryConstructor
      if (primaryCtor.isNoSymbol) {
        report.errorAndAbort("Could not find Java Record primary constructor")
      }
      val components = primaryCtor.paramSymss.headOption.getOrElse(Nil)

      components.foreach { paramSym =>
        val name = paramSym.name
        val accessorMethod = sym.methodMember(name).headOption.getOrElse {
          report.errorAndAbort(s"Could not find accessor method for Java Record component: $name")
        }
        val methodType = tpe.memberType(accessorMethod)
        val returnType: TypeRepr = methodType match {
          // Standard method: def foo(x: Int): String  => MethodType(..., String)
          case mt: MethodType => mt.resType
          // Generic method: def foo[A]: A => PolyType
          case pt: PolyType =>
            pt.resType match {
              case mt: MethodType => mt.resType // Unpack the underlying method
              case other          => other
            }
          // Parameterless method (in some contexts) or ByName
          case byName: ByNameType => byName.underlying
          // Fallback (e.g. it was already a value type)
          case other => other
        }
        returnType.asType match {
          case '[t] =>
            val valueAccess = Apply(Select(valueTerm, accessorMethod), Nil)
            functionOnField(returnType, name, valueAccess)
        }
      }
    }
  }
}
