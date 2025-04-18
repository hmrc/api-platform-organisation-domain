/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.apiplatform.modules.organisations.submissions.domain.models

import scala.collection.immutable.{ListMap, ListSet}

import play.api.libs.json.{Format, Json, OFormat}
import uk.gov.hmrc.apiplatform.modules.common.domain.services.MapJsonFormatters

sealed trait Mark

object Mark {

  case object Fail extends Mark
  case object Warn extends Mark
  case object Pass extends Mark

  import cats.Monoid

  implicit val markMonoid: Monoid[Mark] = new Monoid[Mark] {
    def empty: Mark = Pass

    def combine(x: Mark, y: Mark): Mark = (x, y) match {
      case (Fail, _)    => Fail
      case (_, Fail)    => Fail
      case (Warn, _)    => Warn
      case (_, Warn)    => Warn
      case (Pass, Pass) => Pass
    }
  }
}

sealed trait Question {
  def id: Question.Id
  def wording: Wording
  def statement: Option[Statement]
  def afterStatement: Option[Statement]

  def absence: Option[(String, Mark)]

  def absenceText: Option[String] = absence.map(_._1)
  def absenceMark: Option[Mark]   = absence.map(_._2)

  final def isOptional: Boolean = absence.isDefined
}

case class PossibleAnswer(value: String) extends AnyVal {
  def htmlValue: String = value.replace(" ", "-").filter(c => c.isLetterOrDigit || c == '-')
}

trait LabelAndHints {
  self: Question =>

  def label: Option[Question.Label]
  def hintText: Option[NonBulletStatementFragment]
}

case class ErrorInfo private (summary: String, message: Option[String])

object ErrorInfo {
  def apply(summary: String): ErrorInfo                  = new ErrorInfo(summary, None)
  def apply(summary: String, message: String): ErrorInfo = if (summary == message) apply(summary) else new ErrorInfo(summary, Some(message))

  implicit val format: OFormat[ErrorInfo] = Json.format[ErrorInfo]
}

trait ErrorMessaging {
  self: Question =>

  def errorInfo: Option[ErrorInfo]
}

case class Wording(value: String) extends AnyVal

object Wording {
  implicit val format: Format[Wording] = Json.valueFormat[Wording]
}

object Question extends MapJsonFormatters {
  case class Id(value: String) extends AnyVal
  val answerKey = "answer"

  object Id {
    def random = Id(java.util.UUID.randomUUID.toString)

    implicit val format: Format[Id] = Json.valueFormat[Id]
  }

  case class Label(value: String) extends AnyVal

  object Label {
    implicit val format: Format[Label] = Json.valueFormat[Label]
  }

  case class TextQuestion(
      id: Question.Id,
      wording: Wording,
      statement: Option[Statement],
      afterStatement: Option[Statement] = None,
      label: Option[Question.Label] = None,
      hintText: Option[NonBulletStatementFragment] = None,
      validation: Option[TextValidation] = None,
      absence: Option[(String, Mark)] = None,
      errorInfo: Option[ErrorInfo] = None
    ) extends Question with LabelAndHints with ErrorMessaging

  case class DateQuestion(
      id: Question.Id,
      wording: Wording,
      statement: Option[Statement],
      afterStatement: Option[Statement] = None,
      label: Option[Question.Label] = None,
      hintText: Option[NonBulletStatementFragment] = None,
      absence: Option[(String, Mark)] = None,
      errorInfo: Option[ErrorInfo] = None
    ) extends Question with LabelAndHints with ErrorMessaging

  case class AddressQuestion(
      id: Question.Id,
      wording: Wording,
      statement: Option[Statement],
      afterStatement: Option[Statement] = None,
      label: Option[Question.Label] = None,
      hintText: Option[NonBulletStatementFragment] = None,
      absence: Option[(String, Mark)] = None,
      errorInfo: Option[ErrorInfo] = None
    ) extends Question with LabelAndHints with ErrorMessaging

  case class AcknowledgementOnly(
      id: Question.Id,
      wording: Wording,
      statement: Option[Statement]
    ) extends Question {
    val absence        = None
    val afterStatement = None
  }

  sealed trait ChoiceQuestion extends Question with LabelAndHints with ErrorMessaging {
    def choices: ListSet[PossibleAnswer]
    def marking: ListMap[PossibleAnswer, Mark]
  }

  sealed trait SingleChoiceQuestion extends ChoiceQuestion

  case class MultiChoiceQuestion(
      id: Question.Id,
      wording: Wording,
      statement: Option[Statement],
      afterStatement: Option[Statement] = None,
      label: Option[Question.Label] = None,
      hintText: Option[NonBulletStatementFragment] = None,
      marking: ListMap[PossibleAnswer, Mark],
      absence: Option[(String, Mark)] = None,
      errorInfo: Option[ErrorInfo] = None
    ) extends ChoiceQuestion {
    lazy val choices: ListSet[PossibleAnswer] = ListSet(marking.keys.toList: _*)
  }

  case class ChooseOneOfQuestion(
      id: Question.Id,
      wording: Wording,
      statement: Option[Statement],
      afterStatement: Option[Statement] = None,
      label: Option[Question.Label] = None,
      hintText: Option[NonBulletStatementFragment] = None,
      marking: ListMap[PossibleAnswer, Mark],
      absence: Option[(String, Mark)] = None,
      errorInfo: Option[ErrorInfo] = None
    ) extends SingleChoiceQuestion {
    lazy val choices: ListSet[PossibleAnswer] = ListSet(marking.keys.toList: _*)
  }

  case class YesNoQuestion(
      id: Question.Id,
      wording: Wording,
      statement: Option[Statement],
      afterStatement: Option[Statement] = None,
      label: Option[Question.Label] = None,
      hintText: Option[NonBulletStatementFragment] = None,
      yesMarking: Mark,
      noMarking: Mark,
      absence: Option[(String, Mark)] = None,
      errorInfo: Option[ErrorInfo] = None
    ) extends SingleChoiceQuestion {

    val YES = PossibleAnswer("Yes")
    val NO  = PossibleAnswer("No")

    lazy val marking: ListMap[PossibleAnswer, Mark] = ListMap(YES -> yesMarking, NO -> noMarking)
    lazy val choices                                = ListSet(YES, NO)
  }

  import play.api.libs.json._
  import uk.gov.hmrc.play.json.Union

  implicit val jsonFormatWording: Format[Wording] = Json.valueFormat[Wording]

  implicit val markWrites: Writes[Mark] = Writes {
    case Mark.Fail => JsString("fail")
    case Mark.Warn => JsString("warn")
    case Mark.Pass => JsString("pass")
  }

  implicit val markReads: Reads[Mark] = Reads {
    case JsString("fail") => JsSuccess(Mark.Fail)
    case JsString("warn") => JsSuccess(Mark.Warn)
    case JsString("pass") => JsSuccess(Mark.Pass)
    case _                => JsError("Failed to parse Mark value")
  }

  implicit val keyReadsQuestionId: KeyReads[Question.Id]   = KeyReads(key => JsSuccess(Question.Id(key)))
  implicit val keyWritesQuestionId: KeyWrites[Question.Id] = KeyWrites(_.value)

  implicit val keyReadsPossibleAnswer: KeyReads[PossibleAnswer]   = KeyReads(key => JsSuccess(PossibleAnswer(key)))
  implicit val keyWritesPossibleAnswer: KeyWrites[PossibleAnswer] = KeyWrites(_.value)

  implicit val jsonListMapKV: Reads[ListMap[PossibleAnswer, Mark]] = listMapReads[PossibleAnswer, Mark]

  import Statement._

  implicit val jsonFormatPossibleAnswer: Format[PossibleAnswer]    = Json.valueFormat[PossibleAnswer]
  implicit val jsonFormatTextQuestion: OFormat[TextQuestion]       = Json.format[TextQuestion]
  implicit val jsonFormatYesNoQuestion: OFormat[YesNoQuestion]     = Json.format[YesNoQuestion]
  implicit val jsonFormatDateQuestion: OFormat[DateQuestion]       = Json.format[DateQuestion]
  implicit val jsonFormatAddressQuestion: OFormat[AddressQuestion] = Json.format[AddressQuestion]

  implicit val jsonFormatChooseOneOfQuestion: OFormat[ChooseOneOfQuestion] = Json.format[ChooseOneOfQuestion]
  implicit val jsonFormatMultiChoiceQuestion: OFormat[MultiChoiceQuestion] = Json.format[MultiChoiceQuestion]
  implicit val jsonFormatAcknowledgementOnly: OFormat[AcknowledgementOnly] = Json.format[AcknowledgementOnly]

  implicit val jsonFormatQuestion: Format[Question] = Union.from[Question]("questionType")
    .and[MultiChoiceQuestion]("multi")
    .and[YesNoQuestion]("yesNo")
    .and[ChooseOneOfQuestion]("choose")
    .and[DateQuestion]("date")
    .and[AddressQuestion]("address")
    .and[TextQuestion]("text")
    .and[AcknowledgementOnly]("acknowledgement")
    .format
}
