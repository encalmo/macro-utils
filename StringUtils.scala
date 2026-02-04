package org.encalmo.utils

import scala.quoted.*

object StringUtils {

  /** Concatenate a list of terms to a string. */
  def concat(using Quotes)(term: quotes.reflect.Term, terms: quotes.reflect.Term*): quotes.reflect.Term = {
    import quotes.reflect.*

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
  def applyToString(using Quotes)(term: quotes.reflect.Term): quotes.reflect.Term = {
    import quotes.reflect.*

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
