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

package uk.gov.hmrc.apiplatform.modules.organisations.submissions.domain.services

import uk.gov.hmrc.apiplatform.modules.organisations.submissions.domain.models._

object MarkAnswer {
  //
  // Assume answer is valid for question as it is only called for validated completed submissions
  //

  protected def markSingleChoiceAnswer(question: Question.SingleChoiceQuestion, answer: ActualAnswer.SingleChoiceAnswer): Mark =
    question.marking.get(PossibleAnswer(answer.value)).get

  protected def markMultiChoiceAnswer(question: Question.MultiChoiceQuestion, answer: ActualAnswer.MultipleChoiceAnswer): Mark = {
    import cats.Monoid
    import Mark._

    Monoid.combineAll(
      answer.values
        .map(PossibleAnswer)
        .map(question.marking.get(_).get)
    )
  }

  protected def markQuestion(question: Question, answer: ActualAnswer): Mark = {
    (question, answer) match {
      case (_, ActualAnswer.NoAnswer)                                              => question.absenceMark.getOrElse(throw new RuntimeException(s"Failed with $answer for $question"))
      case (q: Question.TextQuestion, a: ActualAnswer.TextAnswer)                  => Mark.Pass
      case (q: Question.MultiChoiceQuestion, a: ActualAnswer.MultipleChoiceAnswer) => markMultiChoiceAnswer(q, a)
      case (q: Question.SingleChoiceQuestion, a: ActualAnswer.SingleChoiceAnswer)  => markSingleChoiceAnswer(q, a)
      case (q: Question.AcknowledgementOnly, ActualAnswer.AcknowledgedAnswer)      => Mark.Pass
      case _                                                                       => throw new IllegalArgumentException(s"Unexpectely the answer is not valid - ${question.wording.value}")
    }
  }

  private def markInstanceInternal(submission: Submission, instance: Submission.Instance): Map[Question.Id, Mark] = {
    // All questions should/must exist for these questionIds.
    def unsafeGetQuestion(id: Question.Id): Question = submission.findQuestion(id).get

    instance.answersToQuestions.map {
      case (id: Question.Id, answer: ActualAnswer) => (id -> markQuestion(unsafeGetQuestion(id), answer))
    }
  }

  def markInstance(submission: Submission, index: Int): Map[Question.Id, Mark] = {
    // All answers must be valid to have got here
    require(submission.status.canBeMarked)

    submission.instances.find(_.index == index)
      .fold(Map.empty[Question.Id, Mark])(i => markInstanceInternal(submission, i))
  }

  def markSubmission(submission: Submission): Map[Question.Id, Mark] = {
    // All answers must be valid to have got here
    require(submission.status.canBeMarked)

    markInstanceInternal(submission, submission.latestInstance)
  }
}
