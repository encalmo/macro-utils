package org.encalmo.utils

import scala.quoted.*

class StatementsCache(implicit val quotes: Quotes) {
  import quotes.reflect.*

  private val index: collection.mutable.Map[String, Statement] =
    collection.mutable.Map.empty

  private val statements: collection.mutable.ListBuffer[Statement] =
    collection.mutable.ListBuffer.empty

  def getOrElseCreateMethod(methodName: String, methodBody: Expr[Unit]): Unit = {
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

  def addStatement(statement: Statement): Unit = {
    statements.append(statement)
  }

  def addStatements(statements: Iterable[Statement]): Unit = {
    this.statements.appendAll(statements)
  }

  def getStatements: List[Statement] = {
    statements.toList
  }

  def getBlockExprOfUnit: Expr[Unit] = {
    Block(statements.toList, '{}.asTerm).asExprOf[Unit]
  }

  def getBlockExprOf[T: Type](valueExpr: Expr[T]): Expr[T] = {
    Block(statements.toList, valueExpr.asTerm).asExprOf[T]
  }

}
