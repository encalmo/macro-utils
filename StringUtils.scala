package org.encalmo.utils

import scala.quoted.*

object StringUtils {

  /** Concatenate a list of terms to a string. */
  def concat(using
      cache: StatementsCache
  )(term: cache.quotes.reflect.Term, terms: cache.quotes.reflect.Term*): cache.quotes.reflect.Term = {
    import cache.quotes.reflect.*
    given cache.quotes.type = cache.quotes

    val stringPlusSym = defn.StringClass
      .methodMember("+")
      .find { sym =>
        // We want the overload: def +(x: Any): String
        sym.paramSymss match {
          case List(List(param)) => true
          case _                 => false
        }
      }
      .getOrElse(report.errorAndAbort("Could not find String.+ method"))

    terms.foldLeft(term) { (acc, term) =>
      term match {
        case literal @ Literal(StringConstant(string)) =>
          Apply(Select(acc, stringPlusSym), List(literal))
        case _ =>
          Apply(Select(acc, stringPlusSym), List(applyToString(term)))
      }

    }

  }

  /** Best effort to convert a term to a string. */
  def applyToString(using cache: StatementsCache)(term: cache.quotes.reflect.Term): cache.quotes.reflect.Term = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    term.tpe match {
      case t if t <:< TypeRepr.of[String] => term
      case _                              =>
        val sym = term.tpe.dealias.widen.typeSymbol
        sym.methodMember("toString").match {
          case toStringSym :: _ => Apply(Select(term, toStringSym), Nil)
          case Nil              => term
        }
    }
  }

}
