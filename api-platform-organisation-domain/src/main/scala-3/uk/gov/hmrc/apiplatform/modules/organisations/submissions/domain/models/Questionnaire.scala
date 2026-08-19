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
  case class AskWhenContext(contextKey: String, expectedValue: String)                                              extends AskWhen
  case class AskWhenAnswer(questionId: Question.Id, expectedValue: ActualAnswer.SingleChoiceAnswer)                 extends AskWhen
  case class AskWhenAnswers(questionId: Question.Id, expectedValues: NonEmptyList[ActualAnswer.SingleChoiceAnswer]) extends AskWhen
  case object AlwaysAsk                                                                                             extends AskWhen

  object AskWhenAnswer {

    def apply(question: Question.SingleChoiceQuestion, expectedValue: String): AskWhen = {
      require(question.choices.find(qc => qc.value == expectedValue).isDefined)
      AskWhenAnswer(question.id, ActualAnswer.SingleChoiceAnswer(expectedValue))
    }
  }

  object AskWhenAnswers {

    def apply(question: Question.SingleChoiceQuestion, expectedValues: NonEmptyList[String]): AskWhen = {
      require(!expectedValues.map(ev => question.choices.find(qc => qc.value == ev).isDefined).exists(is => is == false))
      AskWhenAnswers(question.id, expectedValues.map(ev => ActualAnswer.SingleChoiceAnswer(ev)))
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

  def shouldAsk(context: Context, answersToQuestions: Submission.AnswersToQuestions)(askWhen: NonEmptyList[AskWhen]): Boolean = {
    // Assume that all AskWhen's must be true for the overall one to be true - i.e. AND not OR
    !askWhen.map(shouldAskWhen(context, answersToQuestions)(_)).exists(r => r == false)
  }

  private def shouldAskWhen(context: Context, answersToQuestions: Submission.AnswersToQuestions)(askWhen: AskWhen): Boolean = {
    askWhen match {
      case AlwaysAsk                                   => true
      case AskWhenContext(contextKey, expectedValue)   => context.get(contextKey).map(_.equalsIgnoreCase(expectedValue)).getOrElse(false)
      case AskWhenAnswer(questionId, expectedAnswer)   => answersToQuestions.get(questionId).map(_ == expectedAnswer).getOrElse(false)
      case AskWhenAnswers(questionId, expectedAnswers) => answersToQuestions.get(questionId).map(aa => expectedAnswers.exists(ea => ea == aa)).getOrElse(false)
    }
  }

  import play.api.libs.json.*
  import uk.gov.hmrc.play.json.Union
  import NonEmptyListFormatters.given

  given OFormat[AskWhenContext] = Json.format[AskWhenContext]
  given OFormat[AskWhenAnswer]  = Json.format[AskWhenAnswer]
  given OFormat[AskWhenAnswers] = Json.format[AskWhenAnswers]

  given Format[AskWhen] = Union.from[AskWhen]("askWhen")
    .and[AskWhenContext]("askWhenContext")
    .and[AskWhenAnswer]("askWhenAnswer")
    .and[AskWhenAnswers]("askWhenAnswers")
    .andType("alwaysAsk", () => AlwaysAsk)
    .format
}

case class QuestionItem(question: Question, askWhen: NonEmptyList[AskWhen])

object QuestionItem {
  def apply(question: Question): QuestionItem                                 = QuestionItem(question, NonEmptyList.of(AskWhen.AlwaysAsk))
  def apply(question: Question, askWhen: AskWhen): QuestionItem               = new QuestionItem(question, NonEmptyList.of(askWhen))
  def apply(question: Question, askWhen: NonEmptyList[AskWhen]): QuestionItem = new QuestionItem(question, askWhen)

  import play.api.libs.json.*
  import NonEmptyListFormatters.given

  given OFormat[QuestionItem] = Json.format[QuestionItem]
}

object Questionnaire {
  import NonEmptyListFormatters.given

  case class Label(value: String) extends AnyVal
  case class Id(value: String)    extends AnyVal

  object Label {
    given Format[Label] = Json.valueFormat[Label]
  }

  object Id {
    def random = Questionnaire.Id(java.util.UUID.randomUUID.toString)

    given Format[Id] = Json.valueFormat[Id]
  }

  import play.api.libs.json.*
  import QuestionItem.given

  given OFormat[Questionnaire] = Json.format[Questionnaire]
}

case class Questionnaire(
    id: Questionnaire.Id,
    label: Questionnaire.Label,
    questions: NonEmptyList[QuestionItem]
  ) {
  def hasQuestion(qid: Question.Id): Boolean       = question(qid).isDefined
  def question(qid: Question.Id): Option[Question] = questions.find(_.question.id == qid).map(_.question)
}
