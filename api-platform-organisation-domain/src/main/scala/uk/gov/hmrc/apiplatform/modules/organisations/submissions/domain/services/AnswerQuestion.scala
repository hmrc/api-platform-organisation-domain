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

import java.time.Instant
import java.time.temporal.ChronoUnit

import cats.data.NonEmptyList

import uk.gov.hmrc.apiplatform.modules.organisations.submissions.domain.models._

object AnswerQuestion {
  import Submission.AnswersToQuestions

  private def fromOption[A](opt: Option[A], msg: String): Either[ValidationErrors, A] =
    opt.fold[Either[ValidationErrors, A]](Left(ValidationErrors(ValidationError(message = msg))))(v => Right(v))

  private def cond[A](cond: => Boolean, ok: A, msg: String): Either[ValidationErrors, A] = if (cond) Right(ok) else Left(ValidationErrors(ValidationError(message = msg)))

  def questionsToAsk(questionnaire: Questionnaire, context: AskWhen.Context, answersToQuestions: AnswersToQuestions): List[Question.Id] = {
    questionnaire.questions.collect {
      case (qi) if AskWhen.shouldAsk(context, answersToQuestions)(qi.askWhen) => qi.question.id
    }
  }

  def recordAnswer(submission: Submission, questionId: Question.Id, rawAnswers: Map[String, Seq[String]]): Either[ValidationErrors, ExtendedSubmission] = {
    for {
      question         <- fromOption(submission.findQuestion(questionId), "Not valid for this submission")
      context           = submission.context
      validatedAnswers <- ValidateAnswers.validate(question, rawAnswers)
      latestInstance    = submission.latestInstance

      updatedAnswersToQuestions   <- cond(
                                       latestInstance.isOpenToAnswers,
                                       latestInstance.answersToQuestions + (questionId -> validatedAnswers),
                                       "Answers cannot be recorded for a Submission that is not in progress"
                                     )
      // we assume no recursion needed for the next 3 steps - otherwise the ask when question structure must have been implemented in a complex recursive mess
      updatedQuestionnaireProgress = deriveProgressOfQuestionnaires(submission.allQuestionnaires, context, updatedAnswersToQuestions)
      areQuestionsAnswered         = updatedQuestionnaireProgress.values
                                       .map(_.state)
                                       .forall(QuestionnaireState.isCompleted)
      questionsThatShouldBeAsked   = updatedQuestionnaireProgress.flatMap(_._2.questionsToAsk).toList
      finalAnswersToQuestions      = updatedAnswersToQuestions.filter { case (qid, _) => questionsThatShouldBeAsked.contains(qid) }
      updatedSubmission            = updateSubmissionState(finalAnswersToQuestions, areQuestionsAnswered, submission)
    } yield ExtendedSubmission(updatedSubmission, updatedQuestionnaireProgress)
  }

  def updateSubmissionState(answers: Submission.AnswersToQuestions, areQuestionsAnswered: Boolean, submission: Submission): Submission = {
    import Submission._

    val addAnsweringStatus = addStatusHistory(Submission.Status.Answering(Instant.now().truncatedTo(ChronoUnit.MILLIS), areQuestionsAnswered))
    (addAnsweringStatus andThen updateLatestAnswersTo(answers))(submission)
  }

  def deriveProgressOfQuestionnaire(questionnaire: Questionnaire, context: AskWhen.Context, answersToQuestions: AnswersToQuestions): QuestionnaireProgress = {
    val questionsToAsk                           = AnswerQuestion.questionsToAsk(questionnaire, context, answersToQuestions)
    val (answeredQuestions, unansweredQuestions) = questionsToAsk.partition(answersToQuestions.contains)
    val state                                    = (unansweredQuestions.headOption, answeredQuestions.nonEmpty) match {
      case (None, true)  => QuestionnaireState.Completed
      case (None, false) => QuestionnaireState.NotApplicable
      case (_, true)     => QuestionnaireState.InProgress
      case (_, false)    => QuestionnaireState.NotStarted
    }

    QuestionnaireProgress(state, questionsToAsk)
  }

  def deriveProgressOfQuestionnaires(
      questionnaires: NonEmptyList[Questionnaire],
      context: AskWhen.Context,
      answersToQuestions: AnswersToQuestions
    ): Map[Questionnaire.Id, QuestionnaireProgress] = {
    questionnaires.toList.map(q => (q.id -> deriveProgressOfQuestionnaire(q, context, answersToQuestions))).toMap
  }
}
