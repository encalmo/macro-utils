package org.encalmo.utils

import scala.quoted.*

object QuotesUtils {

  extension (using quotes: Quotes)(term: quotes.reflect.Term) {

    inline def applyToString: quotes.reflect.Term =
      StringUtils.applyToString(term)

    inline def callMethod(methodName: String, args: List[quotes.reflect.Term]): quotes.reflect.Term =
      MethodUtils.callMethod(term, methodName, args)

    inline def callAsInstanceOf[T: Type]: quotes.reflect.Term =
      import quotes.reflect.*
      val asInstanceOfSym = defn.AnyClass.methodMember("asInstanceOf").head
      TypeApply(
        Select(term, asInstanceOfSym),
        List(TypeTree.of[T])
      )

  }

}
