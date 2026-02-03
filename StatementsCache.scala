package org.encalmo.utils

import scala.quoted.*

class StatementsCache(implicit val quotes: Quotes) {
  import quotes.reflect.*

  private val statements: collection.mutable.ListBuffer[Statement] =
    collection.mutable.ListBuffer.empty

  private val index: collection.mutable.Map[String, Statement] =
    collection.mutable.Map.empty

  private val symbols: collection.mutable.Map[String, Symbol] =
    collection.mutable.Map.empty

  def addMethodCall(methodName: String): Unit = {
    index.get(methodName) match {
      case Some(methodCall) =>
        statements.append(methodCall)

      case None =>
        report.errorAndAbort(s"Method call '$methodName' not found in statements cache")
    }
  }

  def addMethodCall(methodName: String, methodBody: => Expr[Unit]): Unit = {
    index.get(methodName) match {
      case Some(methodCall) =>
        statements.append(methodCall)

      case None => {

        val methodSymbol: Symbol =
          Symbol.newMethod(
            Symbol.spliceOwner,
            methodName,
            MethodType(Nil)(_ => Nil, _ => TypeRepr.of[Unit]),
            Flags.Protected,
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

  def addMethodOfUnit(methodName: String, buildMethodBody: StatementsCache ?=> Unit): quotes.reflect.Term = {
    index.get(methodName) match {
      case Some(methodCall) =>
        methodCall.asInstanceOf[quotes.reflect.Term]

      case None => {

        val methodBody: Term = {
          val nested = new StatementsCache(using quotes)
          buildMethodBody(using nested)
          nested.asTerm.asInstanceOf[Term]
        }

        val methodSymbol: Symbol =
          Symbol.newMethod(
            Symbol.spliceOwner,
            methodName,
            MethodType(Nil)(_ => Nil, _ => TypeRepr.of[Unit]),
            Flags.Protected,
            Symbol.noSymbol
          )

        val methodDef = DefDef(
          methodSymbol,
          { _ => Some(methodBody.changeOwner(methodSymbol)) }
        )

        val methodCall = Apply(Ref(methodSymbol), Nil)

        index.put(methodName, methodCall)
        statements.append(methodDef)
        methodCall
      }
    }
  }

  def addMethodOfUnitCall(methodName: String, buildMethodBody: StatementsCache ?=> Unit): Unit = {
    val methodCall: quotes.reflect.Term = addMethodOfUnit(methodName, buildMethodBody)
    addStatement(methodCall)
  }

  def getValueRef(valueName: String): quotes.reflect.Ref = {
    index.get(valueName) match {
      case Some(valueRef) =>
        valueRef.asInstanceOf[quotes.reflect.Ref]
      case None =>
        report.errorAndAbort(s"Value ref '$valueName' not found in statements cache")
    }
  }

  def getValueRefOfExpr[T: Type](valueName: String, valueBody: => Expr[T]): quotes.reflect.Ref = {
    index.get(valueName) match {
      case Some(valueRef) =>
        valueRef.asInstanceOf[quotes.reflect.Ref]

      case None => {

        val valueSymbol: Symbol =
          Symbol.newVal(
            Symbol.spliceOwner,
            valueName,
            TypeRepr.of[T],
            Flags.Private,
            Symbol.noSymbol
          )

        val valueDef = ValDef(valueSymbol, Some(valueBody.asTerm))
        val valueRef = Ref(valueSymbol)

        index.put(valueName, valueRef)
        statements.append(valueDef)
        valueRef
      }
    }
  }

  def getValueRefOfTerm[T: Type](valueName: String, valueBody: => quotes.reflect.Term): quotes.reflect.Ref = {
    index.get(valueName) match {
      case Some(valueRef) =>
        valueRef.asInstanceOf[quotes.reflect.Ref]

      case None => {

        val valueSymbol: Symbol =
          Symbol.newVal(
            Symbol.spliceOwner,
            valueName,
            TypeRepr.of[T],
            Flags.Private,
            Symbol.noSymbol
          )

        val valueDef = ValDef(valueSymbol, Some(valueBody))
        val valueRef = Ref(valueSymbol)

        index.put(valueName, valueRef)
        statements.append(valueDef)
        valueRef
      }
    }
  }

  def getSymbol(symbolName: String): Symbol = {
    symbols.get(symbolName) match {
      case Some(symbol) => symbol
      case None         =>
        report.errorAndAbort(s"Symbol '$symbolName' not found in statements cache")
    }
  }

  def getSymbol(symbolName: String, symbolBody: => Symbol): Symbol = {
    symbols.get(symbolName) match {
      case Some(symbol) => symbol
      case None         =>
        val symbol = symbolBody
        symbols.put(symbolName, symbol)
        symbol
    }
  }

  def addStatement(statement: Statement): Unit = {
    statements.append(statement)
  }

  def addStatements(statements: Iterable[Statement]): Unit = {
    this.statements.appendAll(statements)
  }

  def getStatements: List[Statement] = {
    statements.toList
  }

  def asTerm[T: Type]: Term = {
    if statements.isEmpty
    then report.errorAndAbort("No statements to get block expression of")
    else if statements.size == 1
    then
      statements.head match {
        case term: Term =>
          term
        case _ =>
          report.errorAndAbort("Statement is not a term")
      }
    else
      statements.last match {
        case term: Term =>
          Block(statements.init.toList, term)
        case _ =>
          report.errorAndAbort("Last statement is not a term")
      }
  }

  def getBlockExprOfUnit: Expr[Unit] = {
    Block(statements.toList, '{}.asTerm).asExprOf[Unit]
  }

  def getBlockExprOf[T: Type]: Expr[T] = {
    if statements.isEmpty
    then report.errorAndAbort("No statements to get block expression of")
    else if statements.size == 1
    then statements.head.asExprOf[T]
    else
      statements.last match {
        case term: Term =>
          Block(statements.init.toList, term).asExprOf[T]
        case _ =>
          report.errorAndAbort("Last statement is not a term")
      }
  }

  def getBlockExprOf[T: Type](lastStatementExpr: Expr[T]): Expr[T] = {
    Block(statements.toList, lastStatementExpr.asTerm).asExprOf[T]
  }

}

object StatementsCache {

  def block(using outer: StatementsCache)(buildBlock: StatementsCache ?=> Unit): outer.quotes.reflect.Term = {
    val nested = new StatementsCache(using outer.quotes)
    given nested.quotes.type = nested.quotes
    buildBlock(using nested)
    nested.asTerm.asInstanceOf[outer.quotes.reflect.Term]
  }

  def addStatement(using cache: StatementsCache)(statement: cache.quotes.reflect.Statement): Unit = {
    cache.addStatement(statement)
  }

  def asNestedTerm(using cache: StatementsCache)(term: Any): cache.quotes.reflect.Term = {
    term.asInstanceOf[cache.quotes.reflect.Term]
  }
}
