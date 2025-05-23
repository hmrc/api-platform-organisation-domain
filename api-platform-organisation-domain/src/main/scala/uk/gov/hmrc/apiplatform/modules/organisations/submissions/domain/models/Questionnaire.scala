/*
 * Copyright 2024 HM Revenue & Customs
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

import cats.data.NonEmptyList

import play.api.libs.json.{Format, Json}
import uk.gov.hmrc.apiplatform.modules.common.domain.services.NonEmptyListFormatters

sealed trait AskWhen

object AskWhen {
  case class AskWhenContext(contextKey: String, expectedValue: String)                              extends AskWhen
  case class AskWhenAnswer(questionId: Question.Id, expectedValue: ActualAnswer.SingleChoiceAnswer) extends AskWhen
  case object AlwaysAsk                                                                             extends AskWhen

  object AskWhenAnswer {

    def apply(question: Question.SingleChoiceQuestion, expectedValue: String): AskWhen = {
      require(question.choices.find(qc => qc.value == expectedValue).isDefined)
      AskWhenAnswer(question.id, ActualAnswer.SingleChoiceAnswer(expectedValue))
    }
  }

  type Context = Map[String, String]

  object Context {

    object Keys {
      val VAT_OR_ITSA             = "VAT_OR_ITSA"
      val IN_HOUSE_SOFTWARE       = "IN_HOUSE_SOFTWARE"       // Stored on Application
      val NEW_TERMS_OF_USE_UPLIFT = "NEW_TERMS_OF_USE_UPLIFT" // Application already in production, rather than a production credentials request
    }
  }

  def shouldAsk(context: Context, answersToQuestions: Submission.AnswersToQuestions)(askWhen: AskWhen): Boolean = {
    askWhen match {
      case AlwaysAsk                                 => true
      case AskWhenContext(contextKey, expectedValue) => context.get(contextKey).map(_.equalsIgnoreCase(expectedValue)).getOrElse(false)
      case AskWhenAnswer(questionId, expectedAnswer) => answersToQuestions.get(questionId).map(_ == expectedAnswer).getOrElse(false)
    }
  }

  import play.api.libs.json._
  import uk.gov.hmrc.play.json.Union

  implicit val jsonFormatAskWhenContext: OFormat[AskWhenContext] = Json.format[AskWhenContext]
  implicit val jsonFormatAskWhenAnswer: OFormat[AskWhenAnswer]   = Json.format[AskWhenAnswer]

  implicit val jsonFormatCondition: Format[AskWhen] = Union.from[AskWhen]("askWhen")
    .and[AskWhenContext]("askWhenContext")
    .and[AskWhenAnswer]("askWhenAnswer")
    .andType("alwaysAsk", () => AlwaysAsk)
    .format
}

case class QuestionItem(question: Question, askWhen: AskWhen)

object QuestionItem extends NonEmptyListFormatters {
  def apply(question: Question): QuestionItem                   = QuestionItem(question, AskWhen.AlwaysAsk)
  def apply(question: Question, askWhen: AskWhen): QuestionItem = new QuestionItem(question, askWhen)

  import play.api.libs.json._
  import AskWhen._

  implicit val jsonFormatQuestionItem: OFormat[QuestionItem] = Json.format[QuestionItem]
}

object Questionnaire {
  case class Label(value: String) extends AnyVal
  case class Id(value: String)    extends AnyVal

  object Label {
    implicit val format: Format[Label] = Json.valueFormat[Label]
  }

  object Id {
    def random = Questionnaire.Id(java.util.UUID.randomUUID.toString)

    implicit val format: Format[Id] = Json.valueFormat[Id]
  }

  import play.api.libs.json._
  import QuestionItem._

  implicit val jsonFormatquestionnaire: OFormat[Questionnaire] = Json.format[Questionnaire]
}

case class Questionnaire(
    id: Questionnaire.Id,
    label: Questionnaire.Label,
    questions: NonEmptyList[QuestionItem]
  ) {
  def hasQuestion(qid: Question.Id): Boolean       = question(qid).isDefined
  def question(qid: Question.Id): Option[Question] = questions.find(_.question.id == qid).map(_.question)
}
