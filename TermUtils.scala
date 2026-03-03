package org.encalmo.utils

import scala.annotation.tailrec

object TermUtils {

  def unwrap(using cache: StatementsCache)(term: cache.quotes.reflect.Term): cache.quotes.reflect.Term = {
    import cache.quotes.reflect.*
    @tailrec
    def unwrap(t: Term): Term = t match
      case Inlined(_, _, expansion) => unwrap(expansion) // Peek inside Inlined
      case Typed(expr, _)           => unwrap(expr) // Peek inside Typed
      case Block(Nil, expr)         => unwrap(expr) // Peek inside empty blocks
      case other                    => other // We hit the core term!
    unwrap(term)
  }

  def isLiteralConstant(using cache: StatementsCache)(term: cache.quotes.reflect.Term): Boolean = {
    import cache.quotes.reflect.*
    unwrap(term) match {
      case Literal(_) => true
      case _          => false
    }
  }

  def isNullConstant(using cache: StatementsCache)(term: cache.quotes.reflect.Term): Boolean = {
    import cache.quotes.reflect.*
    unwrap(term) match {
      case Literal(NullConstant()) => true
      case other                   => false
    }
  }

  def isStringConstant(using cache: StatementsCache)(term: cache.quotes.reflect.Term): Boolean = {
    import cache.quotes.reflect.*
    unwrap(term) match {
      case Literal(StringConstant(_)) => true
      case _                          => false
    }
  }

  def isIntConstant(using cache: StatementsCache)(term: cache.quotes.reflect.Term): Boolean = {
    import cache.quotes.reflect.*
    unwrap(term) match {
      case Literal(IntConstant(_)) => true
      case _                       => false
    }
  }

  def isLongConstant(using cache: StatementsCache)(term: cache.quotes.reflect.Term): Boolean = {
    import cache.quotes.reflect.*
    unwrap(term) match {
      case Literal(LongConstant(_)) => true
      case _                        => false
    }
  }

  def isFloatConstant(using cache: StatementsCache)(term: cache.quotes.reflect.Term): Boolean = {
    import cache.quotes.reflect.*
    unwrap(term) match {
      case Literal(FloatConstant(_)) => true
      case _                         => false
    }
  }

  def isDoubleConstant(using cache: StatementsCache)(term: cache.quotes.reflect.Term): Boolean = {
    import cache.quotes.reflect.*
    unwrap(term) match {
      case Literal(DoubleConstant(_)) => true
      case _                          => false
    }
  }

  def isCharConstant(using cache: StatementsCache)(term: cache.quotes.reflect.Term): Boolean = {
    import cache.quotes.reflect.*
    unwrap(term) match {
      case Literal(CharConstant(_)) => true
      case _                        => false
    }
  }

  def isBooleanConstant(using cache: StatementsCache)(term: cache.quotes.reflect.Term): Boolean = {
    import cache.quotes.reflect.*
    unwrap(term) match {
      case Literal(BooleanConstant(_)) => true
      case _                           => false
    }
  }

  def isByteConstant(using cache: StatementsCache)(term: cache.quotes.reflect.Term): Boolean = {
    import cache.quotes.reflect.*
    unwrap(term) match {
      case Literal(ByteConstant(_)) => true
      case _                        => false
    }
  }

  def isShortConstant(using cache: StatementsCache)(term: cache.quotes.reflect.Term): Boolean = {
    import cache.quotes.reflect.*
    unwrap(term) match {
      case Literal(ShortConstant(_)) => true
      case _                         => false
    }
  }
}
