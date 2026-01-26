/*
 * Copyright 2026 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.encalmo.utils

import scala.quoted.*

object TypeNameUtils {

  def shortBaseName(name: String): String = {
    val name2 = name.indexOf('[') match {
      case -1  => name
      case idx => name.substring(0, idx)
    }
    name2.lastIndexOf('.') match {
      case -1  => name2.replace("\"", "")
      case idx => name2.substring(idx + 1)
    }
  }

  def underscored(name: String): String = {
    name.replace(".", "_")
  }

  transparent inline def typeName[A] = ${ typeNameExpr[A] }
  def typeNameExpr[A: Type](using Quotes): Expr[String] =
    import quotes.reflect.*
    val name = TypeRepr.of[A].dealias.show(using Printer.TypeReprShortCode)
    Expr(shortBaseName(name))

  inline def typeNames[A]: Tuple = ${ typeNamesExpr[A] }
  def typeNamesExpr[A: Type](using Quotes): Expr[Tuple] =
    Type.of[A] match
      case '[elem *: elems] => '{ typeName[elem] *: typeNames[elems] }
      case _                => '{ EmptyTuple }

}
