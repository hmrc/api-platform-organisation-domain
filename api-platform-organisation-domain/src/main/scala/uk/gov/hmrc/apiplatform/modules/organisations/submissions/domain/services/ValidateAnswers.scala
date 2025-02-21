/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.apiplatform.modules.organisations.submissions.domain.services

import java.time.LocalDate
import scala.util.Try

import cats.implicits._

import uk.gov.hmrc.apiplatform.modules.organisations.submissions.domain.models._

object ValidateAnswers {

  def validate(question: Question, rawAnswers: Map[String, Seq[String]]): Either[String, ActualAnswer] = {
    question match {
      case _: Question.AcknowledgementOnly                                      => validateAcknowledgement(rawAnswers.get(Question.answerKey).exists(_.nonEmpty))
      case _ if question.isOptional && !rawAnswers.contains(Question.answerKey) => Either.right(ActualAnswer.NoAnswer)
      case q: Question.MultiChoiceQuestion                                      => rawAnswers.get(Question.answerKey).map(a => validateAgainstPossibleAnswers(q, a.toSet))
          .getOrElse(Either.left("Question requires an answer"))
      case q: Question.SingleChoiceQuestion                                     =>
        rawAnswers.get(Question.answerKey).filter(_.length == 1).map(a => validateAgainstPossibleAnswers(q, a.head)).getOrElse(Either.left("Question requires a single answer"))
      case q: Question.TextQuestion                                             =>
        rawAnswers.get(Question.answerKey).filter(_.length == 1).map(a => validateAgainstPossibleTextValidationRule(q, a.head)).getOrElse(Either.left(
          "Question requires a single answer"
        ))
      case _: Question.DateQuestion                                             => validateDate(rawAnswers)
    }

  }

  def validateAcknowledgement(hasAnswer: Boolean) = {
    if (hasAnswer) Either.left("Acknowledgement cannot accept answers") else Either.right(ActualAnswer.AcknowledgedAnswer)
  }

  def validateAgainstPossibleTextValidationRule(question: Question.TextQuestion, rawAnswer: String): Either[String, ActualAnswer] = {
    question.validation
      .fold(rawAnswer.asRight[String])(v => v.validate(rawAnswer))
      .map(ActualAnswer.TextAnswer(_))
  }

  def validateAgainstPossibleAnswers(question: Question.MultiChoiceQuestion, rawAnswers: Set[String]): Either[String, ActualAnswer] = {
    if (rawAnswers subsetOf question.choices.map(_.value)) {
      Either.right(ActualAnswer.MultipleChoiceAnswer(rawAnswers))
    } else {
      Either.left("Not all answers are valid")
    }
  }

  def validateDate(rawAnswers: Map[String, Seq[String]]): Either[String, ActualAnswer] = {
    (rawAnswers.get("day"), rawAnswers.get("month"), rawAnswers.get("year")) match {
      case (Some(day :: Nil), Some(month :: Nil), Some(year :: Nil)) =>
        Try(LocalDate.of(year.toInt, month.toInt, day.toInt)).fold(_ => Either.left("Invalid Date"), date => Either.right(ActualAnswer.DateAnswer(date)))
      case _                                                         => Either.left("Invalid Date")

    }
  }

  def validateAgainstPossibleAnswers(question: Question.SingleChoiceQuestion, rawAnswer: String): Either[String, ActualAnswer] = {
    if (question.choices.map(_.value).contains(rawAnswer)) {
      Either.right(ActualAnswer.SingleChoiceAnswer(rawAnswer))
    } else {
      Either.left("Answer is not valid")
    }
  }
}
