package org.encalmo.utils

import scala.quoted.*
import org.encalmo.utils.StatementsCache.Scope

/** A cache for statements and symbols. It can be nested to create a hierarchy of caches. Captures Quotes context.
  */
class StatementsCache(val cacheId: String = "default")(implicit val quotes: Quotes) {
  import quotes.reflect.*

  private val statements: collection.mutable.ListBuffer[Statement] =
    collection.mutable.ListBuffer.empty

  protected val index: collection.mutable.Map[String, Ref] =
    collection.mutable.Map.empty

  protected val symbols: collection.mutable.Map[String, Symbol] =
    collection.mutable.Map.empty

  /** Lookup a method or value by name. */
  def lookupStatement(name: String): Option[Statement] = {
    index.get(name)
  }

  /** Lookup a symbol by name. */
  def lookupSymbol(name: String): Option[quotes.reflect.Symbol] = {
    symbols.get(name)
  }

  /** Add method or value definition to statements list and index reference to the definition by provided name */
  def declare(
      scope: StatementsCache.Scope,
      name: String,
      definition: Any,
      reference: Any
  ): Unit = {
    index.put(name, reference.asInstanceOf[quotes.reflect.Ref])
    put(definition.asInstanceOf[quotes.reflect.ValOrDefDef])
  }

  /** Create a nested statements cache that fallbacks to lookup in the outer cache if statement or symbol is not found
    * in the nested cache. Lists of statements stay separated.
    */
  def createNestedScope(cacheId: String = "nested"): StatementsCache = {
    val outer = this
    new StatementsCache(outer.cacheId + " > " + cacheId)(using outer.quotes) {

      /** Lookup a method or value by name in the nested cache or the outer caches. */
      override def lookupStatement(name: String): Option[quotes.reflect.Statement] = {
        this.index
          .get(name)
          .orElse(outer.lookupStatement(name).map(_.asInstanceOf[quotes.reflect.Statement]))
      }

      /** Lookup a symbol by name in the nested cache or the outer caches. */
      override def lookupSymbol(name: String): Option[quotes.reflect.Symbol] = {
        this.symbols
          .get(name)
          .orElse(outer.lookupSymbol(name).map(_.asInstanceOf[quotes.reflect.Symbol]))
      }

      override def declare(
          scope: StatementsCache.Scope,
          name: String,
          definition: Any,
          reference: Any
      ): Unit = {
        scope match {
          case StatementsCache.Scope.Local =>
            this.index.put(name, reference.asInstanceOf[quotes.reflect.Ref])
            this.put(definition.asInstanceOf[quotes.reflect.ValOrDefDef])

          case StatementsCache.Scope.TopLevel =>
            outer.declare(
              scope,
              name,
              definition.asInstanceOf[outer.quotes.reflect.ValOrDefDef],
              reference.asInstanceOf[outer.quotes.reflect.Term]
            )

          case StatementsCache.Scope.Outer =>
            outer.declare(
              StatementsCache.Scope.Local,
              name,
              definition.asInstanceOf[outer.quotes.reflect.ValOrDefDef],
              reference.asInstanceOf[outer.quotes.reflect.Term]
            )
        }
      }
    }
  }

  /** Lookup named method call of type Unit and add to the statements list, otherwise abort with an error. */
  def putMethodCall(methodName: String, parameters: List[Term]): Unit = {
    lookupStatement(methodName) match {
      case Some(methodRef) =>
        put(methodRef.asInstanceOf[quotes.reflect.Ref].appliedToArgs(parameters))

      case None =>
        report.errorAndAbort("[" + cacheId + s"] Method call '$methodName' not found in statements cache")
    }
  }

  /** Lookup or create a new method of type T and return the method call */
  def createMethodOf[T: Type](
      methodName: String,
      parameterNames: List[String],
      parameterTypes: List[TypeRepr],
      buildMethodBody: StatementsCache ?=> List[Tree] => Unit,
      scope: StatementsCache.Scope = Scope.Local
  ): quotes.reflect.Term = {
    lookupStatement(methodName) match {
      case Some(methodRef) =>
        methodRef.asInstanceOf[quotes.reflect.Ref]

      case None => {

        if (parameterNames.length != parameterTypes.length)
        then report.errorAndAbort("Parameter names and types must have the same length for method " + methodName)

        val methodType = MethodType(parameterNames)(
          (_: MethodType) => parameterTypes, // Argument types
          (_: MethodType) => TypeRepr.of[T] // Return type
        )

        val methodSymbol: Symbol =
          Symbol.newMethod(
            Symbol.spliceOwner,
            methodName,
            methodType,
            Flags.EmptyFlags,
            Symbol.noSymbol
          )

        val methodDef = DefDef(
          methodSymbol,
          {
            case List(argSymbols) =>
              Some({
                val nested = createNestedScope(
                  "createMethodOf[" + TypeRepr.of[T].show(using Printer.TypeReprShortCode) + "]:" + methodName
                )
                buildMethodBody(using nested)(argSymbols.map(_.asInstanceOf[quotes.reflect.Tree]))
                if (
                    TypeRepr.of[T] <:< TypeRepr.of[Unit]
                    && !(nested.typeRepr <:< nested.quotes.reflect.TypeRepr.of[Unit])
                  )
                then nested.put(nested.unit)
                nested.asTerm.asInstanceOf[Term]
              }.changeOwner(methodSymbol))

            case other =>
              report.errorAndAbort("Unexpected parameter structure " + other + " for method " + methodName)
          }
        )

        val methodRef = Ref(methodSymbol)

        declare(scope, methodName, methodDef, methodRef)
        methodRef
      }
    }
  }

  /** Lookup or create a new method of type T and add the method call to the statements list */
  def putMethodCallOf[T: Type](
      methodName: String,
      parameterNames: List[String],
      parameterTypes: List[TypeRepr],
      parameters: List[Term],
      buildMethodBody: StatementsCache ?=> List[Tree] => Unit,
      scope: StatementsCache.Scope = Scope.Local
  ): Unit = {
    if (parameters.length != parameterNames.length)
    then report.errorAndAbort("Parameter lists must have the same length for method " + methodName)
    val methodRef: quotes.reflect.Term =
      createMethodOf[T](methodName, parameterNames, parameterTypes, buildMethodBody, scope)
    put(methodRef.appliedToArgs(parameters))
  }

  /** Lookup or create a new method of type T and add the method call to the statements list */
  def putParamlessMethodCallOf[T: Type](
      methodName: String,
      buildMethodBody: StatementsCache ?=> Unit,
      scope: StatementsCache.Scope = Scope.Local
  ): Unit = {
    val methodRef: quotes.reflect.Term =
      createMethodOf[T](methodName, Nil, Nil, _ => buildMethodBody, scope)
    put(methodRef.appliedToArgs(Nil))
  }

  /** Lookup value reference by name and return the reference, otherwise abort with an error. */
  def getValueRef(valueName: String): quotes.reflect.Ref = {
    lookupStatement(valueName) match {
      case Some(valueRef) =>
        valueRef.asInstanceOf[quotes.reflect.Ref]
      case None =>
        report.errorAndAbort("[" + cacheId + s"] Value ref '$valueName' not found in statements cache")
    }
  }

  /** Lookup or create a new value reference of type T and add the value definition to the statements list, then return
    * the value reference
    */
  def getValueRefOfExpr[T: Type](
      valueName: String,
      valueBody: => Expr[T],
      scope: StatementsCache.Scope = Scope.Local
  ): quotes.reflect.Ref = {
    lookupStatement(valueName) match {
      case Some(valueRef) =>
        valueRef.asInstanceOf[quotes.reflect.Ref]

      case None => {

        val valueSymbol: Symbol =
          Symbol.newVal(
            Symbol.spliceOwner,
            valueName,
            TypeRepr.of[T],
            Flags.EmptyFlags,
            Symbol.noSymbol
          )

        val valueDef = ValDef(valueSymbol, Some(valueBody.asTerm))
        val valueRef = Ref(valueSymbol)

        declare(scope, valueName, valueDef, valueRef)
        valueRef
      }
    }
  }

  /** Lookup or create a new value reference of type T and add the value definition to the statements list, then return
    * the value reference
    */
  def getValueRefOfTerm[T: Type](
      valueName: String,
      valueBody: => quotes.reflect.Term,
      scope: StatementsCache.Scope = Scope.Local
  ): quotes.reflect.Ref = {
    lookupStatement(valueName) match {
      case Some(valueRef) =>
        valueRef.asInstanceOf[quotes.reflect.Ref]

      case None => {

        val valueSymbol: Symbol =
          Symbol.newVal(
            Symbol.spliceOwner,
            valueName,
            TypeRepr.of[T],
            Flags.EmptyFlags,
            Symbol.noSymbol
          )

        val valueDef = ValDef(valueSymbol, Some(valueBody))
        val valueRef = Ref(valueSymbol)

        declare(scope, valueName, valueDef, valueRef)
        valueRef
      }
    }
  }

  /** Lookup symbol by name and return the symbol, otherwise abort with an error. */
  def getSymbol(symbolName: String): Symbol = {
    lookupSymbol(symbolName) match {
      case Some(symbol) => symbol
      case None         =>
        report.errorAndAbort("[" + cacheId + s"] Symbol '$symbolName' not found in statements cache")
    }
  }

  /** Lookup or create a new symbol and add it to the statements list, then return the symbol */
  def getSymbol(symbolName: String, symbolBody: => Symbol): Symbol = {
    lookupSymbol(symbolName) match {
      case Some(symbol) => symbol
      case None         =>
        val symbol = symbolBody
        symbols.put(symbolName, symbol)
        symbol
    }
  }

  def unit: Literal = {
    Literal(UnitConstant())
  }

  def stringLiteral(value: String): Literal = {
    Literal(StringConstant(value))
  }

  def addUnitStatement(): Unit = {
    put(unit)
  }

  def put(statement: Statement): Unit = {
    this.statements.append(statement)
  }

  def putAll(statements: Iterable[Statement]): Unit = {
    this.statements.appendAll(statements)
  }

  def toList: List[Statement] = {
    statements.toList
  }

  def typeRepr: TypeRepr = {
    statements.lastOption
      .map {
        case term: Term => term.tpe
        case statement  => TypeRepr.of[Unit]
      }
      .getOrElse(TypeRepr.of[Unit])
  }

  /** Convert the statements list to a term, otherwise abort with an error. */
  def asTerm: Term = {
    if statements.isEmpty
    then unit
    else if statements.size == 1
    then
      statements.head match {
        case term: Term => term
        case statement  => Block(statements.toList, unit)
      }
    else
      statements.last match {
        case term: Term => Block(statements.init.toList, term)
        case statement  => Block(statements.toList, unit)
      }
  }

  /** Convert the statements list to a term of the outer cache type, otherwise abort with an error. */
  def asTermOf(outer: StatementsCache): outer.quotes.reflect.Term = {
    asTerm.asInstanceOf[outer.quotes.reflect.Term]
  }

  def getBlockExprOfUnit: Expr[Unit] = {
    Block(statements.toList, unit).asExprOf[Unit]
  }

  def getBlockExprOf[T: Type]: Expr[T] = {
    if statements.isEmpty
    then
      report.errorAndAbort(
        "[" + cacheId + "] No statements to get block expression of type " + TypeRepr
          .of[T]
          .show(using Printer.TypeReprShortCode)
      )
    else if statements.size == 1
    then
      statements.head match {
        case term: Term if term.tpe <:< TypeRepr.of[T] => term.asExprOf[T]
        case term: Term                                =>
          report.errorAndAbort(
            "[" + cacheId + "] Expected first statement to be a term of type " + TypeRepr
              .of[T]
              .show(using Printer.TypeReprShortCode) + " but got: " + term.tpe.show(using Printer.TypeReprShortCode)
          )
        case statement =>
          report.errorAndAbort(
            "[" + cacheId + "] Expected first statement to be a term but got: " + statement.show(using Printer.TreeCode)
          )
      }
    else
      statements.last match {
        case term: Term if term.tpe <:< TypeRepr.of[T] =>
          Block(statements.init.toList, term).asExprOf[T]
        case term: Term =>
          report.errorAndAbort(
            "[" + cacheId + "] Expected last statement to be a term of type " + TypeRepr
              .of[T]
              .show(using Printer.TypeReprShortCode) + " but got: " + term.tpe.show(using Printer.TypeReprShortCode)
          )
        case statement =>
          report.errorAndAbort(
            "[" + cacheId + "] Expected last statement to be a term but got: " + statement.show(using Printer.TreeCode)
          )
      }
  }

  def getBlockExprOf[T: Type](returnExpr: Expr[T]): Expr[T] = {
    Block(statements.toList, returnExpr.asTerm).asExprOf[T]
  }

}

object StatementsCache {

  enum Scope {
    case TopLevel
    case Outer
    case Local
  }

  def createNestedScope(cacheId: String = "nested")(using cache: StatementsCache): StatementsCache =
    cache.createNestedScope(cacheId)

  def block(using outer: StatementsCache)(buildBlock: StatementsCache ?=> Unit): outer.quotes.reflect.Term = {
    val nested = outer.createNestedScope("block")
    given nested.quotes.type = nested.quotes
    buildBlock(using nested)
    nested.asTerm.asInstanceOf[outer.quotes.reflect.Term]
  }

  def put(using cache: StatementsCache)(statement: cache.quotes.reflect.Statement): Unit = {
    cache.put(statement)
  }

  def unit(using cache: StatementsCache): cache.quotes.reflect.Literal = {
    cache.unit
  }

  def stringLiteral(using cache: StatementsCache)(value: String): cache.quotes.reflect.Literal = {
    cache.stringLiteral(value)
  }

  extension (using cache: StatementsCache)(term: cache.quotes.reflect.Term) {

    def put(statement: cache.quotes.reflect.Statement): Unit =
      cache.put(statement)

    def applyToString: cache.quotes.reflect.Term =
      StringUtils.applyToString(using cache)(term)

    def methodCall(
        methodName: String,
        args: List[cache.quotes.reflect.Term],
        moreArgs: List[cache.quotes.reflect.Term]*
    ): cache.quotes.reflect.Term =
      MethodUtils.methodCall(term, methodName, args, moreArgs*)

    def callAsInstanceOf[T: Type]: cache.quotes.reflect.Term =
      import cache.quotes.reflect.*
      val asInstanceOfSym = defn.AnyClass.methodMember("asInstanceOf").head
      TypeApply(
        Select(term, asInstanceOfSym),
        List(TypeTree.of[T])
      )

    def callAsInstanceOf(typeTree: cache.quotes.reflect.TypeTree): cache.quotes.reflect.Term =
      import cache.quotes.reflect.*
      val asInstanceOfSym = defn.AnyClass.methodMember("asInstanceOf").head
      TypeApply(
        Select(term, asInstanceOfSym),
        List(typeTree)
      )
  }

  extension (term: Any) {

    def toTermOf(other: StatementsCache): other.quotes.reflect.Term =
      term.asInstanceOf[other.quotes.reflect.Term]

    def toTerm(using nested: StatementsCache): nested.quotes.reflect.Term =
      term.asInstanceOf[nested.quotes.reflect.Term]

    def toTypeRepr(using nested: StatementsCache): nested.quotes.reflect.TypeRepr =
      term.asInstanceOf[nested.quotes.reflect.TypeRepr]
  }
}
