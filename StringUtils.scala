package org.encalmo.utils

import scala.quoted.*

object StringUtils {

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

  def applyToString(using Quotes)(term: quotes.reflect.Term): quotes.reflect.Term = {
    import quotes.reflect.*

    val toStringSym = term.tpe.widen.typeSymbol.methodMember("toString").head
    Apply(Select(term, toStringSym), Nil)
  }

}
