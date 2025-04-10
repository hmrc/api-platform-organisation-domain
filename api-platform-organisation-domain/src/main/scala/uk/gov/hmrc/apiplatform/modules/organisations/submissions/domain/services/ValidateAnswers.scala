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

import play.api.libs.json.{Json, OFormat}

import uk.gov.hmrc.apiplatform.modules.organisations.submissions.domain.models._

case class ValidationErrors(errors: ValidationError*)
case class ValidationError(key: String = Question.answerKey, message: String)

object ValidationError  {
  implicit val validationFormat: OFormat[ValidationError] = Json.format[ValidationError]
}

object ValidationErrors {
  implicit val validationErrorsFormat: OFormat[ValidationErrors] = Json.format[ValidationErrors]
}

object ValidateAnswers  {

  def validate(question: Question, rawAnswers: Map[String, Seq[String]]): Either[ValidationErrors, ActualAnswer] = {
    question match {
      case _: Question.AcknowledgementOnly                                                      => validateAcknowledgement(rawAnswers.get(Question.answerKey).exists(_.nonEmpty))
      case _ if question.isOptional && rawAnswers.get(Question.answerKey).fold(true)(_.isEmpty) => Either.right(ActualAnswer.NoAnswer)
      case q: Question.MultiChoiceQuestion                                                      => rawAnswers.get(Question.answerKey).map(a => validateAgainstPossibleAnswers(q, a.toSet))
          .getOrElse(ValidationErrors(ValidationError(message = "Question requires an answer")).asLeft)
      case q: Question.SingleChoiceQuestion                                                     =>
        rawAnswers.get(Question.answerKey).filter(_.length == 1)
          .map(a => validateAgainstPossibleAnswers(q, a.head))
          .getOrElse(ValidationErrors(ValidationError(message = "Question requires an answer")).asLeft)
      case q: Question.TextQuestion                                                             =>
        rawAnswers.get(Question.answerKey).filter(_.length == 1)
          .map(a => validateAgainstPossibleTextValidationRule(q, a.head))
          .getOrElse(ValidationErrors(ValidationError(message = "Question requires an answer")).asLeft)
      case _: Question.DateQuestion                                                             => validateDate(rawAnswers)
      case _: Question.AddressQuestion                                                          => validateAddress(rawAnswers)
    }
  }

  def validateAcknowledgement(hasAnswer: Boolean): Either[ValidationErrors, ActualAnswer] = {
    if (hasAnswer) Either.left(ValidationErrors(ValidationError(message = "Acknowledgement cannot accept answers")))
    else Either.right(ActualAnswer.AcknowledgedAnswer)
  }

  def validateAgainstPossibleTextValidationRule(question: Question.TextQuestion, rawAnswer: String): Either[ValidationErrors, ActualAnswer] = {
    question.validation
      .fold(rawAnswer.asRight[String])(v => v.validate(rawAnswer))
      .map(ActualAnswer.TextAnswer(_))
      .left.map(msg => ValidationErrors(ValidationError(Question.answerKey, msg)))
  }

  def validateAgainstPossibleAnswers(question: Question.MultiChoiceQuestion, rawAnswers: Set[String]): Either[ValidationErrors, ActualAnswer] = {
    if (rawAnswers subsetOf question.choices.map(_.value)) {
      Either.right(ActualAnswer.MultipleChoiceAnswer(rawAnswers))
    } else {
      Either.left(ValidationErrors(ValidationError(Question.answerKey, "Not all answers are valid")))
    }
  }

  def validateIntField(key: String, rawAnswers: Map[String, Seq[String]], error: ValidationError): Either[Seq[ValidationError], Int] = {
    rawAnswers
      .get(key)
      .flatMap(_.headOption)
      .flatMap(_.toIntOption)
      .map(_.asRight)
      .getOrElse(Seq(error).asLeft)

  }

  def validateStringField(key: String, rawAnswers: Map[String, Seq[String]], error: ValidationError): Either[Seq[ValidationError], String] = {
    rawAnswers
      .get(key)
      .flatMap(_.headOption)
      .map(_.asRight)
      .getOrElse(Seq(error).asLeft)

  }

  def validateDate(rawAnswers: Map[String, Seq[String]]): Either[ValidationErrors, ActualAnswer] = {
    (
      validateIntField("day", rawAnswers = rawAnswers, error = ValidationError("day", "Valid Day Needed")),
      validateIntField("month", rawAnswers = rawAnswers, error = ValidationError("month", "Valid Month Needed")),
      validateIntField("year", rawAnswers = rawAnswers, error = ValidationError("year", "Valid Year Needed"))
    ).parFlatMapN((d, m, y) =>
      Try(LocalDate.of(y, m, d)).fold(
        _ => Either.left(Seq(ValidationError(message = "Invalid Date"))),
        date => Either.right(ActualAnswer.DateAnswer(date))
      )
    ).leftMap(err => ValidationErrors(err: _*))
  }

  def validateAddress(rawAnswers: Map[String, Seq[String]]): Either[ValidationErrors, ActualAnswer] = {
    (
      validateStringField("addressLineOne", rawAnswers = rawAnswers, error = ValidationError("addressLineOne", "Address Line One Required")),
      validateStringField("postcode", rawAnswers = rawAnswers, error = ValidationError("postcode", "Postcode required"))
    ).parMapN((addressLineOne, postcode) =>
      ActualAnswer.AddressAnswer(RegisteredOfficeAddress(
        Some(addressLineOne),
        rawAnswers.get("addressLineTwo").flatMap(_.headOption),
        rawAnswers.get("locality").flatMap(_.headOption),
        rawAnswers.get("region").flatMap(_.headOption),
        Some(postcode)
      ))
    ).leftMap(err => ValidationErrors(err: _*))
  }

  def validateAgainstPossibleAnswers(question: Question.SingleChoiceQuestion, rawAnswer: String): Either[ValidationErrors, ActualAnswer] = {
    if (question.choices.map(_.value).contains(rawAnswer)) {
      Either.right(ActualAnswer.SingleChoiceAnswer(rawAnswer))
    } else {
      Either.left(ValidationErrors(ValidationError(message = "Answer is not valid")))
    }
  }
}
