package org.encalmo.utils

import scala.quoted.*

object MethodUtils {

  /** Wrap a method call in a method definition and return the result of the method call. */
  def wrapInMethodCall[T: Type](
      methodName: String,
      methodBody: Expr[T]
  )(using quotes: Quotes): Expr[T] = {
    import quotes.reflect.*

    val methodSymbol: Symbol = Symbol.newMethod(
      Symbol.spliceOwner,
      methodName,
      MethodType(Nil)(_ => Nil, _ => TypeRepr.of[Unit]),
      Flags.Synthetic,
      Symbol.noSymbol
    )

    val methodDef = DefDef(
      methodSymbol,
      { _ => Some(methodBody.asTerm.changeOwner(methodSymbol)) }
    )

    Block(
      List(methodDef),
      Apply(Ref(methodSymbol), Nil)
    ).asExprOf[T]
  }

  /** Maybe select a value from expression using a selector and call the provided function if found, or call fallback
    * function if not found.
    */
  def maybeSelectedValue[T: Type](
      selector: String,
      label: Expr[String],
      expr: Expr[T],
      functionExpr: [A: Type] => (Expr[String], Expr[A]) => Expr[Unit],
      fallbackExpr: => Expr[Unit]
  )(using quotes: Quotes): Expr[Unit] = {
    import quotes.reflect.*
    val tpe = TypeRepr.of[T].dealias
    val classSymbol = tpe.typeSymbol

    // Try to find a field with the selector name.
    val fieldSymbol = classSymbol.fieldMember(selector)
    val targetSymbol =
      if (fieldSymbol.exists && !fieldSymbol.flags.is(Flags.Private | Flags.PrivateLocal))
      then Some(fieldSymbol)
      else {
        classSymbol
          .methodMember(selector)
          .find { sym =>
            val params = sym.paramSymss
            // Matches: def foo: T (Nil) OR def foo(): T (List(Nil))
            params == Nil || params == List(Nil)
          }
          .orElse {
            val getterName = s"get${selector.capitalize}"
            classSymbol.methodMember(getterName).find { sym =>
              val params = sym.paramSymss
              // Matches: def getFoo: T (Nil) OR def getFoo(): T (List(Nil))
              params == Nil || params == List(Nil)
            }
          }
      }

    targetSymbol match {
      case Some(sym) =>
        val actualType: TypeRepr =
          tpe.memberType(sym) match {
            case MethodType(_, _, res) => res
            case ByNameType(u)         => u
            case PolyType(_, _, res)   => res
            case other                 => other
          }

        val select = Select(expr.asTerm, sym)
        val term =
          if (sym.paramSymss == List(Nil))
          then Apply(select, Nil)
          else select

        actualType.asType match {
          case '[a] =>
            functionExpr.apply[a](Expr(selector), term.asExprOf[a])
        }

      case None => fallbackExpr
    }
  }

  /** Dynamically calls a method on 'target' with the given name and arguments. Adds the method call to the statements
    * cache and returns it.
    */
  def methodCall(using
      cache: StatementsCache
  )(
      targetTerm: cache.quotes.reflect.Term,
      methodName: String,
      argTerms: List[cache.quotes.reflect.Term]
  ): cache.quotes.reflect.Term = {
    val tpe = targetTerm.tpe.dealias.widen
    val methodSym = findMethodByArity(tpe, methodName, argTerms.size)
    buildMethodCall(targetTerm, methodSym, argTerms)
  }

  def findMethodByArity(using
      cache: StatementsCache
  )(
      tpe: cache.quotes.reflect.TypeRepr,
      name: String,
      arity: Int
  ): cache.quotes.reflect.Symbol = {
    import cache.quotes.reflect.*

    val clsSym = tpe.typeSymbol
    val candidates = clsSym.methodMember(name)

    // Filter by argument count (ignoring type parameters like [T])
    val matched = candidates.find { sym =>
      val valueParamLists = sym.paramSymss.filterNot { clause =>
        clause.headOption.exists(_.isType)
      }
      // Sum up arguments across all curried lists: (a: Int)(b: Int) = 2 args
      val totalArgs = valueParamLists.flatten.size
      totalArgs == arity
    }

    matched.getOrElse {
      report.errorAndAbort(s"Method '$name' with $arity arguments not found in ${tpe.show}")
    }
  }

  def buildMethodCall(using
      cache: StatementsCache
  )(
      target: cache.quotes.reflect.Term,
      methodSym: cache.quotes.reflect.Symbol,
      allArgs: List[cache.quotes.reflect.Term]
  ): cache.quotes.reflect.Term = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    // Start with the selection: target.method
    var fun: Term = Select(target, methodSym)
    var argsRemaining = allArgs

    // Iterate over parameter lists (handle [T], (x), (y))
    methodSym.paramSymss.foreach { clause =>
      if (clause.headOption.exists(_.isType)) {
        // LAYER 1: Generics (PolyType) -> TypeApply
        // We inject [Any] for generic parameters to satisfy the Type System
        val typeArgs = clause.map(_ => Inferred(TypeRepr.of[Any]))
        fun = TypeApply(fun, typeArgs)
      } else if (clause.headOption.exists(_.isTerm) || clause.isEmpty) {
        // LAYER 2: Values (MethodType) -> Apply
        // Consume the exact number of args this list expects
        val argCount = clause.length
        val (argsForThisClause, rest) = argsRemaining.splitAt(argCount)
        argsRemaining = rest

        fun = Apply(fun, argsForThisClause)
      }
    }

    fun
  }

  def callPrintln(using
      cache: StatementsCache
  )(term: cache.quotes.reflect.Term, terms: cache.quotes.reflect.Term*): cache.quotes.reflect.Term = {
    import cache.quotes.reflect.*
    given cache.quotes.type = cache.quotes
    val predefTerm = Ref(defn.PredefModule)
    val printlnSym = defn.PredefModule
      .methodMember("println")
      .find { sym =>
        sym.paramSymss match {
          case List(List(_)) => true
          case _             => false
        }
      }
      .getOrElse(report.errorAndAbort("Could not find println(Any)"))

    Apply(
      Select(predefTerm, printlnSym),
      List(StringUtils.concat(term, terms*))
    )
  }

}
