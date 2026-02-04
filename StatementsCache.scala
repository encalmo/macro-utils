package org.encalmo.utils

import scala.quoted.*

/** A cache for statements and symbols. It can be nested to create a hierarchy of caches. Captures Quotes context.
  */
class StatementsCache(val cacheId: String = "default")(implicit val quotes: Quotes) {
  import quotes.reflect.*

  private val statements: collection.mutable.ListBuffer[Statement] =
    collection.mutable.ListBuffer.empty

  protected val index: collection.mutable.Map[String, Statement] =
    collection.mutable.Map.empty

  protected val symbols: collection.mutable.Map[String, Symbol] =
    collection.mutable.Map.empty

  /** Lookup a method or value by name. */
  def lookupStatement(name: String): Option[Statement] = {
    index.get(name)
  }

  /** Lookup a symbol by name. */
  def lookupSymbol(name: String): Option[Symbol] = {
    symbols.get(name)
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
    }
  }

  /** Lookup named method call of type Unit and add to the statements list, otherwise abort with an error. */
  def putMethodCall(methodName: String): Unit = {
    lookupStatement(methodName) match {
      case Some(methodCall) =>
        statements.append(methodCall)

      case None =>
        report.errorAndAbort("[" + cacheId + s"] Method call '$methodName' not found in statements cache")
    }
  }

  /** Lookup or create a new method of type Unit and add the method call to the statements list */
  def putMethodCall(methodName: String, methodBody: => Expr[Unit]): Unit = {
    lookupStatement(methodName) match {
      case Some(methodCall) =>
        statements.append(methodCall)

      case None => {

        val methodSymbol: Symbol =
          Symbol.newMethod(
            Symbol.spliceOwner,
            methodName,
            MethodType(Nil)(_ => Nil, _ => TypeRepr.of[Unit]),
            Flags.EmptyFlags,
            Symbol.noSymbol
          )

        val methodDef = DefDef(
          methodSymbol,
          { _ => Some(methodBody.asTerm.changeOwner(methodSymbol)) }
        )

        val methodCall = Apply(Ref(methodSymbol), Nil)

        index.put(methodName, methodCall)
        statements.append(methodDef)
        statements.append(methodCall)
      }
    }
  }

  /** Lookup or create a new method of type Unit and add the method call to the statements list */
  def createMethodOfUnit(methodName: String, buildMethodBody: StatementsCache ?=> Unit): quotes.reflect.Term = {
    lookupStatement(methodName) match {
      case Some(methodCall) =>
        methodCall.asInstanceOf[quotes.reflect.Term]

      case None => {

        val methodBody: Term = {
          val nested = createNestedScope("createMethodOfUnit:" + methodName)
          buildMethodBody(using nested)
          nested.addUnitStatement()
          nested.asTerm.asInstanceOf[Term]
        }

        val methodSymbol: Symbol =
          Symbol.newMethod(
            Symbol.spliceOwner,
            methodName,
            MethodType(Nil)(_ => Nil, _ => TypeRepr.of[Unit]),
            Flags.EmptyFlags,
            Symbol.noSymbol
          )

        val methodDef = DefDef(
          methodSymbol,
          { _ => Some(methodBody.changeOwner(methodSymbol)) }
        )

        val methodCall = Apply(Ref(methodSymbol), Nil)

        index.put(methodName, methodCall)
        put(methodDef)
        methodCall
      }
    }
  }

  /** Lookup or create a new method of type Unit and add the method call to the statements list */
  def putMethodOfUnitCall(methodName: String, buildMethodBody: StatementsCache ?=> Unit): Unit = {
    val methodCall: quotes.reflect.Term = createMethodOfUnit(methodName, buildMethodBody)
    put(methodCall)
  }

  /** Lookup or create a new method of type T and return the method call */
  def createMethodOf[T: Type](methodName: String, buildMethodBody: StatementsCache ?=> Unit): quotes.reflect.Term = {
    lookupStatement(methodName) match {
      case Some(methodCall) =>
        methodCall.asInstanceOf[quotes.reflect.Term]

      case None => {

        val methodBody: Term = {
          val nested = createNestedScope(
            "createMethodOf[" + TypeRepr.of[T].show(using Printer.TypeReprShortCode) + "]:" + methodName
          )
          buildMethodBody(using nested)
          nested.asTerm.asInstanceOf[Term]
        }

        val methodSymbol: Symbol =
          Symbol.newMethod(
            Symbol.spliceOwner,
            methodName,
            MethodType(Nil)(_ => Nil, _ => TypeRepr.of[T]),
            Flags.EmptyFlags,
            Symbol.noSymbol
          )

        val methodDef = DefDef(
          methodSymbol,
          { _ => Some(methodBody.changeOwner(methodSymbol)) }
        )

        val methodCall = Apply(Ref(methodSymbol), Nil)

        index.put(methodName, methodCall)
        put(methodDef)
        methodCall
      }
    }
  }

  /** Lookup or create a new method of type T and add the method call to the statements list */
  def putMethodCallOf[T: Type](methodName: String, buildMethodBody: StatementsCache ?=> Unit): Unit = {
    val methodCall: quotes.reflect.Term = createMethodOf[T](methodName, buildMethodBody)
    put(methodCall)
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
  def getValueRefOfExpr[T: Type](valueName: String, valueBody: => Expr[T]): quotes.reflect.Ref = {
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

        index.put(valueName, valueRef)
        put(valueDef)
        valueRef
      }
    }
  }

  /** Lookup or create a new value reference of type T and add the value definition to the statements list, then return
    * the value reference
    */
  def getValueRefOfTerm[T: Type](valueName: String, valueBody: => quotes.reflect.Term): quotes.reflect.Ref = {
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

        index.put(valueName, valueRef)
        put(valueDef)
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

  /** Convert the statements list to a term, otherwise abort with an error. */
  def asTerm: Term = {
    if statements.isEmpty
    then report.errorAndAbort("[" + cacheId + "] No statements to get block expression of")
    else if statements.size == 1
    then
      statements.head match {
        case term: Term =>
          term
        case statement =>
          report.errorAndAbort(
            "[" + cacheId + "] Expected term but got: " + statement.show(using Printer.TreeCode)
          )
      }
    else
      statements.last match {
        case term: Term =>
          Block(statements.init.toList, term)
        case statement =>
          report.errorAndAbort(
            "[" + cacheId + "] Expected last statement to be a term but got: " + statement.show(using Printer.TreeCode)
          )
      }
  }

  /** Convert the statements list to a term of the outer cache type, otherwise abort with an error. */
  def asTermOf(outer: StatementsCache): outer.quotes.reflect.Term = {
    asTerm.asInstanceOf[outer.quotes.reflect.Term]
  }

  def getBlockExprOfUnit: Expr[Unit] = {
    Block(statements.toList, '{}.asTerm).asExprOf[Unit]
  }

  def getBlockExprOf[T: Type]: Expr[T] = {
    if statements.isEmpty
    then report.errorAndAbort("[" + cacheId + "] No statements to get block expression of")
    else if statements.size == 1
    then statements.head.asExprOf[T]
    else
      statements.last match {
        case term: Term =>
          Block(statements.init.toList, term).asExprOf[T]
        case statement =>
          report.errorAndAbort(
            "[" + cacheId + "] Expected last statement to be a term but got: " + statement.show(using Printer.TreeCode)
          )
      }
  }

  def getBlockExprOf[T: Type](lastStatementExpr: Expr[T]): Expr[T] = {
    Block(statements.toList, lastStatementExpr.asTerm).asExprOf[T]
  }

}

object StatementsCache {

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

  def unit(using cache: StatementsCache)(value: String): cache.quotes.reflect.Literal = {
    cache.unit
  }

  def stringLiteral(using cache: StatementsCache)(value: String): cache.quotes.reflect.Literal = {
    cache.stringLiteral(value)
  }

  extension (using cache: StatementsCache)(any: Any) {
    def term = any.asInstanceOf[cache.quotes.reflect.Term]
    def statement = any.asInstanceOf[cache.quotes.reflect.Statement]
    def symbol = any.asInstanceOf[cache.quotes.reflect.Symbol]
    def ref = any.asInstanceOf[cache.quotes.reflect.Ref]
  }

  extension (using cache: StatementsCache)(term: cache.quotes.reflect.Term) {

    def put(statement: cache.quotes.reflect.Statement): Unit =
      cache.put(statement)

    def applyToString: cache.quotes.reflect.Term =
      StringUtils.applyToString(using cache.quotes)(term)

    def methodCall(methodName: String, args: List[cache.quotes.reflect.Term]): cache.quotes.reflect.Term =
      MethodUtils.methodCall(term, methodName, args)

    def callAsInstanceOf[T: Type]: cache.quotes.reflect.Term =
      import cache.quotes.reflect.*
      val asInstanceOfSym = defn.AnyClass.methodMember("asInstanceOf").head
      TypeApply(
        Select(term, asInstanceOfSym),
        List(TypeTree.of[T])
      )
  }
}
