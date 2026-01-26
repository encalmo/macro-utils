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

object EnumUtils {

  /** Check if a type is an enum or a sealed abstract type. */
  def isEnumOrSealedADT[A: Type](using Quotes): Boolean = {
    import quotes.reflect.*
    TypeRepr.of[A].dealias.typeSymbol.flags.is(Flags.Sealed)
    && (
      TypeRepr.of[A].dealias.typeSymbol.flags.is(Flags.Trait)
        || TypeRepr.of[A].dealias.typeSymbol.flags.is(Flags.Abstract)
    )
  }

  /** Check if a type is a Java enum. */
  def isJavaEnum[A: Type](using Quotes): Boolean = {
    import quotes.reflect.*
    val sym = TypeRepr.of[A].dealias.typeSymbol
    sym.flags.is(Flags.JavaDefined) && sym.flags.is(Flags.Enum)
  }

  /** Visit an enum and create a pattern match of enum cases.
    *
    * @param valueExpr
    * @param functionExpr
    * @param quotes
    * @return
    *   Unit
    */
  def visit[In: Type](
      valueExpr: Expr[In],
      functionWhenCaseValueExpr: [A: Type] => Quotes ?=> (
          Expr[String],
          Expr[A],
          List[quotes.reflect.Term]
      ) => Expr[Any],
      functionWhenCaseClassExpr: [A: Type] => Quotes ?=> (
          Expr[String],
          Expr[A],
          List[quotes.reflect.Term]
      ) => Expr[Any]
  )(using quotes: Quotes): Expr[Unit] = {
    import quotes.reflect.*

    val enumType = TypeRepr.of[In].dealias
    val enumSymbol = enumType.typeSymbol
    val enumCompanion = enumSymbol.companionModule

    def enumCases: List[Symbol] = {
      if enumSymbol.flags.is(Flags.JavaDefined)
        && enumSymbol.flags.is(Flags.Enum)
      then
        enumSymbol.companionModule.moduleClass.declarations
          .filter { member => member.flags.is(Flags.Enum) }
      else if enumSymbol.flags.is(Flags.Sealed)
        && (enumSymbol.flags.is(Flags.Enum) || enumSymbol.flags.is(Flags.Trait))
      then enumSymbol.children
      else Nil
    }

    if (enumCases.isEmpty)
    then '{}
    else {

      val matchCaseDefs = enumCases
        .map { enumCase =>
          val isEnumCaseValue = enumCompanion.declaredField(enumCase.name).exists
          val isEnumCaseType = enumCompanion.declaredType(enumCase.name).nonEmpty
          val isJavaEnumCase = enumCase.flags.is(Flags.JavaDefined) && enumCase.flags.is(Flags.Enum)

          val enumCaseSymbol =
            if isEnumCaseType
            then enumCompanion.declaredType(enumCase.name).headOption.get
            else if isEnumCaseValue
            then enumCompanion.declaredField(enumCase.name)
            else enumCase

          val tpe =
            if isEnumCaseType
            then TypeSelect(Ref(enumCompanion), enumCase.name).tpe
            else if isEnumCaseValue
            then Select(Ref(enumCompanion), enumCase).tpe
            else enumCase.typeRef

          val matchCasePattern =
            if isEnumCaseType
            then Typed(Wildcard(), TypeSelect(Ref(enumCompanion), enumCase.name))
            else if enumCompanion.declaredField(enumCase.name).exists
            then Ref(enumCase)
            else if isJavaEnumCase
            then Ref(enumCase)
            else Typed(Wildcard(), TypeTree.of(using enumCase.typeRef.asType))

          val matchCaseBody =
            tpe.asType match {
              case '[t] =>
                if (enumCase.isTerm)
                then
                  functionWhenCaseValueExpr.apply[t](
                    Expr(enumCase.name),
                    '{ $valueExpr.asInstanceOf[t] },
                    enumCaseSymbol.annotations
                  )
                else
                  functionWhenCaseClassExpr.apply[t](
                    Expr(enumCase.name),
                    '{ $valueExpr.asInstanceOf[t] },
                    enumCaseSymbol.annotations
                  )
            }

          CaseDef(matchCasePattern, None, matchCaseBody.asTerm)
        }

      Match(valueExpr.asTerm, matchCaseDefs).asExprOf[Unit]
    }
  }

}
