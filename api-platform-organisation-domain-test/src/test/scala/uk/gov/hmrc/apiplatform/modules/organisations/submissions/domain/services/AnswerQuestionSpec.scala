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

import org.scalatest.Inside

import uk.gov.hmrc.apiplatform.modules.common.utils.HmrcSpec

import uk.gov.hmrc.apiplatform.modules.organisations.submissions.domain.models._
import uk.gov.hmrc.apiplatform.modules.organisations.submissions.utils.{AsIdsHelpers, QuestionBuilder, SubmissionsTestData}

class AnswerQuestionSpec extends HmrcSpec with Inside with QuestionBuilder with AsIdsHelpers {

  trait Setup extends SubmissionsTestData
  val blankContext: AskWhen.Context = Map.empty

  val YesAnswer = Map(Question.answerKey -> Seq("Yes"))
  val NoAnswer  = Map(Question.answerKey -> Seq("No"))

  "AnswerQuestion" when {
    "answer is called" should {
      "return updated submission" in new Setup {
        val after = AnswerQuestion.recordAnswer(aSubmission, questionId, YesAnswer)

        inside(after.value) {
          case ExtendedSubmission(submission, _) =>
            submission.id shouldBe submission.id
            submission.organisationId shouldBe submission.organisationId
            submission.startedOn shouldBe submission.startedOn
            submission.groups shouldBe submission.groups
            submission.latestInstance.answersToQuestions.get(questionId).value shouldBe ActualAnswer.SingleChoiceAnswer("Yes")
        }
      }

      "return updated submission after overwriting answer" in new Setup {
        val s1 = AnswerQuestion.recordAnswer(aSubmission, questionId, YesAnswer)
        val s2 = AnswerQuestion.recordAnswer(s1.value.submission, questionId, NoAnswer)

        inside(s2.value) {
          case ExtendedSubmission(submission, _) =>
            submission.latestInstance.answersToQuestions.get(questionId).value shouldBe ActualAnswer.SingleChoiceAnswer("No")
        }
      }

      "return updated submission does not loose other answers in same questionnaire" in new Setup {
        val s1 = AnswerQuestion.recordAnswer(aSubmission, questionId, NoAnswer)
        val s2 = AnswerQuestion.recordAnswer(s1.value.submission, question2Id, YesAnswer)

        inside(s2.value) {
          case ExtendedSubmission(submission, _) =>
            submission.latestInstance.answersToQuestions.get(questionId).value shouldBe ActualAnswer.SingleChoiceAnswer("No")
            submission.latestInstance.answersToQuestions.get(question2Id).value shouldBe ActualAnswer.TextAnswer("Yes")
        }
      }

      "return left when question is not part of the questionnaire" in new Setup {
        val after = AnswerQuestion.recordAnswer(aSubmission, Question.Id.random, YesAnswer)

        after.left.value
      }

      "return left when answer is not valid" in new Setup {
        val after = AnswerQuestion.recordAnswer(aSubmission, ResponsibleIndividualDetails.question1.id, Map(Question.answerKey -> Seq("Bob")))

        after.left.value
      }
    }

    "deriveProgressOfQuestionnaire" should {
      val emptyAnswers = Map.empty[Question.Id, ActualAnswer]

      "return not started, with answerable questions when nothing answered" in new Setup {
        val context = simpleContext
        val answers = emptyAnswers
        val res     = AnswerQuestion.deriveProgressOfQuestionnaire(ResponsibleIndividualDetails.questionnaire, context, answers)

        res.state shouldBe QuestionnaireState.NotStarted
      }

      "return in progress, with answerable questions when a question is answered" in new Setup {
        val context = soldContext
        val answers = Map(ResponsibleIndividualDetails.question1.id -> ActualAnswer.SingleChoiceAnswer("Yes"))
        val res     = AnswerQuestion.deriveProgressOfQuestionnaire(ResponsibleIndividualDetails.questionnaire, context, answers)

        res.state shouldBe QuestionnaireState.InProgress
      }

      "return completed, with answerable questions when all questions are answered" in new Setup {
        val context = soldContext
        val answers = Map(
          ResponsibleIndividualDetails.question1.id -> ActualAnswer.SingleChoiceAnswer("Yes"),
          ResponsibleIndividualDetails.question3.id -> ActualAnswer.TextAnswer("Manager"),
          ResponsibleIndividualDetails.question5.id -> ActualAnswer.TextAnswer("01234 567890"),
          ResponsibleIndividualDetails.question6.id -> ActualAnswer.TextAnswer("https://example.com")
        )
        val res     = AnswerQuestion.deriveProgressOfQuestionnaire(ResponsibleIndividualDetails.questionnaire, context, answers)

        res.state shouldBe QuestionnaireState.Completed
      }
    }
  }
}
