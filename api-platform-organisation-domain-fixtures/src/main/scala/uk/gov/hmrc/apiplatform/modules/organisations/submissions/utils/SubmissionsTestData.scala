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

package uk.gov.hmrc.apiplatform.modules.organisations.submissions.utils

import scala.util.Random

import cats.data.NonEmptyList

import uk.gov.hmrc.apiplatform.modules.common.domain.models.UserId
import uk.gov.hmrc.apiplatform.modules.common.utils.FixedClock

import uk.gov.hmrc.apiplatform.modules.organisations.domain.models.OrganisationId
import uk.gov.hmrc.apiplatform.modules.organisations.submissions.domain.models.AskWhen.Context.Keys
import uk.gov.hmrc.apiplatform.modules.organisations.submissions.domain.models._

trait StatusTestDataHelper {
  self: FixedClock =>

  implicit class StatusHistorySyntax(submission: Submission) {

    def hasCompletelyAnsweredWith(answers: Submission.AnswersToQuestions): Submission = {
      (
        Submission.addStatusHistory(Submission.Status.Answering(instant, true)) andThen
          Submission.updateLatestAnswersTo(answers)
      )(submission)
    }

    def hasCompletelyAnswered: Submission = {
      Submission.addStatusHistory(Submission.Status.Answering(instant, true))(submission)
    }

    def answeringWith(answers: Submission.AnswersToQuestions): Submission = {
      (
        Submission.addStatusHistory(Submission.Status.Answering(instant, false)) andThen
          Submission.updateLatestAnswersTo(answers)
      )(submission)
    }

    def answering: Submission = {
      Submission.addStatusHistory(Submission.Status.Answering(instant, false))(submission)
    }

    def submitted: Submission = {
      Submission.submit(instant, "bob@example.com")(submission)
    }
  }
}

trait ProgressTestDataHelper extends FixedClock {

  implicit class ProgressSyntax(submission: Submission) {
    private val allQuestionnaireIds: NonEmptyList[Questionnaire.Id] = submission.allQuestionnaires.map(_.id)
    private val allQuestionIds                                      = submission.allQuestions.map(_.id)
    private def questionnaire(qId: Questionnaire.Id): Questionnaire = submission.allQuestionnaires.find(q => q.id == qId).get
    private def allQuestionIds(qId: Questionnaire.Id)               = questionnaire(qId).questions.map(_.question).map(_.id).toList

    private def incompleteQuestionnaireProgress(qId: Questionnaire.Id): QuestionnaireProgress    = QuestionnaireProgress(QuestionnaireState.InProgress, allQuestionIds(qId))
    private def completedQuestionnaireProgress(qId: Questionnaire.Id): QuestionnaireProgress     = QuestionnaireProgress(QuestionnaireState.Completed, allQuestionIds.toList)
    private def notStartedQuestionnaireProgress(qId: Questionnaire.Id): QuestionnaireProgress    = QuestionnaireProgress(QuestionnaireState.NotStarted, allQuestionIds.toList)
    private def notApplicableQuestionnaireProgress(qId: Questionnaire.Id): QuestionnaireProgress = QuestionnaireProgress(QuestionnaireState.NotApplicable, allQuestionIds.toList)

    def withIncompleteProgress(): ExtendedSubmission =
      ExtendedSubmission(submission, allQuestionnaireIds.map(i => (i -> incompleteQuestionnaireProgress(i))).toList.toMap)

    def withCompletedProgress(): ExtendedSubmission =
      ExtendedSubmission(submission, allQuestionnaireIds.map(i => (i -> completedQuestionnaireProgress(i))).toList.toMap)

    def withNotStartedProgress(): ExtendedSubmission =
      ExtendedSubmission(submission, allQuestionnaireIds.map(i => (i -> notStartedQuestionnaireProgress(i))).toList.toMap)

    def withNotApplicableProgress(): ExtendedSubmission =
      ExtendedSubmission(submission, allQuestionnaireIds.map(i => (i -> notApplicableQuestionnaireProgress(i))).toList.toMap)

    def withSubmittedProgress(): ExtendedSubmission =
      ExtendedSubmission(Submission.submit(instant, "bob@example.com")(submission), allQuestionnaireIds.map(i => (i -> completedQuestionnaireProgress(i))).toList.toMap)

  }
}

trait SubmissionsTestData extends QuestionBuilder with QuestionnaireTestData with ProgressTestDataHelper with StatusTestDataHelper with FixedClock {

  val organisationId = OrganisationId.random
  val submissionId   = SubmissionId.random
  val userId         = UserId.random

  val standardContext: AskWhen.Context = Map(
    AskWhen.Context.Keys.IN_HOUSE_SOFTWARE       -> "No",
    AskWhen.Context.Keys.VAT_OR_ITSA             -> "No",
    AskWhen.Context.Keys.NEW_TERMS_OF_USE_UPLIFT -> "No"
  )
  val aSubmission                      = Submission.create("bob@example.com", submissionId, Some(organisationId), instant, userId, testGroups, testQuestionIdsOfInterest, standardContext)

  val altSubmissionId = SubmissionId.random
  require(altSubmissionId != submissionId)

  val altSubmission =
    Submission.create("bob@example.com", altSubmissionId, Some(organisationId), instant.plusSeconds(100), userId, testGroups, testQuestionIdsOfInterest, standardContext)

  val completedSubmissionId = SubmissionId.random
  require(completedSubmissionId != submissionId)

  val completelyAnswerExtendedSubmission =
    aSubmission.copy(id = completedSubmissionId)
      .hasCompletelyAnsweredWith(answersToQuestions)
      .withCompletedProgress()

  val gatekeeperUserName = "gatekeeperUserName"
  val reasons            = "some reasons"
  val warnings           = "this is a warning"

  val createdSubmission             = aSubmission
  val answeringSubmission           = createdSubmission.answeringWith(answersToQuestions)
  val answeredSubmission            = createdSubmission.hasCompletelyAnsweredWith(AnsweringQuestionsHelper.answersForGroups(Mark.Pass)(answeringSubmission.groups))
  val submittedSubmission           = Submission.submit(instant, "bob@example.com")(answeredSubmission)
  val declinedSubmission            = Submission.decline(instant, gatekeeperUserName, reasons)(submittedSubmission)
  val grantedSubmission             = Submission.grant(instant, gatekeeperUserName, None, None)(submittedSubmission)
  val grantedWithWarningsSubmission = Submission.grantWithWarnings(instant, gatekeeperUserName, "Warnings", None)(submittedSubmission)
  val pendingRISubmission           = Submission.pendingResponsibleIndividual(instant, "bob@example.com")(submittedSubmission)
  val warningsSubmission            = Submission.warnings(instant, "bob@example.com")(submittedSubmission)
  val failSubmission                = Submission.fail(instant, "bob@example.com")(submittedSubmission)

  val partialQuestionnaireProgress        = Map(
    ResponsibleIndividualDetails.questionnaire.id            -> QuestionnaireProgress(
      QuestionnaireState.NotStarted,
      List(ResponsibleIndividualDetails.question1.id, ResponsibleIndividualDetails.question2.id, ResponsibleIndividualDetails.question3.id)
    ),
    OrganisationDetails.questionnaire.id                     -> QuestionnaireProgress(
      QuestionnaireState.InProgress,
      List(OrganisationDetails.questionOrgType.id, OrganisationDetails.questionLtdOrgName.id)
    ),
    Questionnaire.Id("1e4a1369-8e28-447c-bd47-efbabec1a43b") -> QuestionnaireProgress(QuestionnaireState.NotApplicable, List.empty)
  )
  val partiallyAnsweredExtendedSubmission = ExtendedSubmission(answeringSubmission, partialQuestionnaireProgress)

  def buildSubmissionWithQuestions(): Submission = {
    val subId = SubmissionId.random
    val orgId = OrganisationId.random
    val usrId = UserId.random

    val question1          = chooseOneOfQuestion(1, "a", "b", "c", "d", "e", "f", "g")
    val question2          = chooseOneOfQuestion(2, "ga", "gb", "gc", "gd", "ge")
    val questionName1      = textQuestion(3)
    val questionName2      = textQuestion(4)
    val questionName3      = textQuestion(5)
    val questionName4      = textQuestion(6)
    val questionName5      = textQuestion(7)
    val questionName6      = textQuestion(8)
    val questionName7      = textQuestion(9)
    val questionName8      = textQuestion(10)
    val questionName9      = dateQuestion(11)
    val questionName10     = addressQuestion(12)
    val questionName11     = textQuestion(13)
    val questionPrivacyUrl = textQuestion(14)
    val questionTermsUrl   = textQuestion(15)
    val questionWeb        = textQuestion(16)
    val questionAck        = acknowledgementOnly(17)
    val questionMulti      = multichoiceQuestion(18, "a1", "b", "c")

    val questionnaire1 = Questionnaire(
      id = Questionnaire.Id.random,
      label = Questionnaire.Label("Questionnaire 1"),
      questions = NonEmptyList.of(
        QuestionItem(question1),
        QuestionItem(question2),
        QuestionItem(questionName1, AskWhen.AskWhenAnswer(question1, "a")),
        QuestionItem(questionName2, AskWhen.AskWhenAnswer(question1, "b")),
        QuestionItem(questionName3, AskWhen.AskWhenAnswer(question1, "c")),
        QuestionItem(questionName4, AskWhen.AskWhenAnswer(question1, "d")),
        QuestionItem(questionName5, AskWhen.AskWhenAnswer(question1, "e")),
        QuestionItem(questionName6, AskWhen.AskWhenAnswer(question1, "f")),
        QuestionItem(questionName7, AskWhen.AskWhenAnswer(question2, "ga")),
        QuestionItem(questionName8, AskWhen.AskWhenAnswer(question2, "gb")),
        QuestionItem(questionName9, AskWhen.AskWhenAnswer(question2, "gc")),
        QuestionItem(questionName10, AskWhen.AskWhenAnswer(question2, "gd")),
        QuestionItem(questionName11, AskWhen.AskWhenAnswer(question2, "ge")),
        QuestionItem(questionPrivacyUrl),
        QuestionItem(questionTermsUrl),
        QuestionItem(questionWeb),
        QuestionItem(questionAck),
        QuestionItem(questionMulti)
      )
    )

    val questionnaireGroups = NonEmptyList.of(
      GroupOfQuestionnaires(
        heading = "Group 1",
        links = NonEmptyList.of(
          questionnaire1
        )
      )
    )

    Submission.create(
      "bob@example.com",
      subId,
      Some(orgId),
      instant,
      usrId,
      questionnaireGroups,
      QuestionIdsOfInterest(
        question1.id,
        question2.id,
        questionName1.id,
        questionName2.id,
        questionName3.id,
        questionName4.id,
        questionName5.id,
        questionName6.id,
        questionName7.id,
        questionName8.id,
        questionName9.id,
        questionName10.id,
        questionName11.id
      ),
      standardContext
    )
  }

  private def buildAnsweredSubmission(fullyAnswered: Boolean)(submission: Submission): Submission = {
    val address = RegisteredOfficeAddress(Some("1 main st"), None, None, None, Some("AB1 2CD"))

    def passAnswer(question: Question): ActualAnswer = {
      question match {
        case Question.TextQuestion(id, wording, statement, _, _, _, _, absence, _)                      => ActualAnswer.TextAnswer("some random text")
        case Question.ChooseOneOfQuestion(id, wording, statement, _, _, _, marking, absence, _)         => ActualAnswer.SingleChoiceAnswer(marking.filter {
            case (pa, Mark.Pass) => true; case _ => false
          }.head._1.value)
        case Question.MultiChoiceQuestion(id, wording, statement, _, _, _, marking, absence, _)         => ActualAnswer.MultipleChoiceAnswer(Set(marking.filter {
            case (pa, Mark.Pass) => true; case _ => false
          }.head._1.value))
        case Question.AcknowledgementOnly(id, wording, statement)                                       => ActualAnswer.AcknowledgedAnswer
        case Question.DateQuestion(_, _, _, _, _, _, _, _)                                              => ActualAnswer.DateAnswer(now.toLocalDate)
        case Question.AddressQuestion(_, _, _, _, _, _, _, _)                                           => ActualAnswer.AddressAnswer(address)
        case Question.YesNoQuestion(id, wording, statement, _, _, _, yesMarking, noMarking, absence, _) =>
          if (yesMarking == Mark.Pass) ActualAnswer.SingleChoiceAnswer("Yes") else ActualAnswer.SingleChoiceAnswer("No")
      }
    }

    val answerQuestions = submission.allQuestions.toList.drop(if (fullyAnswered) 0 else 1)
    val answers         = answerQuestions.map(q => (q.id -> passAnswer(q))).toMap

    if (fullyAnswered) {
      submission.hasCompletelyAnsweredWith(answers)
    } else {
      submission.answeringWith(answers)
    }
  }

  def buildPartiallyAnsweredSubmission(submission: Submission = buildSubmissionWithQuestions()): Submission =
    buildAnsweredSubmission(false)(submission)

  def buildFullyAnsweredSubmission(submission: Submission = buildSubmissionWithQuestions()): Submission =
    buildAnsweredSubmission(true)(submission)

  def allFirstQuestions(questionnaires: NonEmptyList[Questionnaire]): Map[Questionnaire.Id, Question.Id] =
    questionnaires.map { qn =>
      (qn.id, qn.questions.head.question.id)
    }
      .toList
      .toMap

  val simpleContext = Map(Keys.IN_HOUSE_SOFTWARE -> "Yes", Keys.VAT_OR_ITSA -> "No")
  val soldContext   = Map(Keys.IN_HOUSE_SOFTWARE -> "No", Keys.VAT_OR_ITSA -> "No")
  val vatContext    = Map(Keys.IN_HOUSE_SOFTWARE -> "Yes", Keys.VAT_OR_ITSA -> "Yes")
}

trait AnsweringQuestionsHelper extends FixedClock {
  val address = RegisteredOfficeAddress(Some("1 main st"), None, None, None, Some("AB1 2CD"))

  def answerForQuestion(desiredMark: Mark)(question: Question): Map[Question.Id, Option[ActualAnswer]] = {
    val answers: List[Option[ActualAnswer]] = question match {

      case Question.YesNoQuestion(id, _, _, _, _, _, yesMarking, noMarking, absence, _) =>
        (if (yesMarking == desiredMark) Some(ActualAnswer.SingleChoiceAnswer("Yes")) else None) ::
          (if (noMarking == desiredMark) Some(ActualAnswer.SingleChoiceAnswer("No")) else None) ::
          (absence.flatMap(a => if (a._2 == desiredMark) Some(ActualAnswer.NoAnswer) else None)) ::
          List.empty[Option[ActualAnswer]]

      case Question.ChooseOneOfQuestion(id, _, _, _, _, _, marking, absence, _) => {
        marking.map {
          case (pa, mark) => Some(ActualAnswer.SingleChoiceAnswer(pa.value))
          case _          => None
        }
          .toList ++
          List(absence.flatMap(a => if (a._2 == desiredMark) Some(ActualAnswer.NoAnswer) else None))
      }

      case Question.TextQuestion(id, _, _, _, _, _, _, absence, _) =>
        if (desiredMark == Mark.Pass)
          Some(ActualAnswer.TextAnswer(Random.nextString(Random.nextInt(25) + 1))) ::
            absence.flatMap(a => if (a._2 == desiredMark) Some(ActualAnswer.NoAnswer) else None) ::
            List.empty[Option[ActualAnswer]]
        else
          List(Some(ActualAnswer.NoAnswer)) // Cos we can't do anything else

      case Question.AcknowledgementOnly(id, _, _)           => List(Some(ActualAnswer.AcknowledgedAnswer))
      case Question.DateQuestion(_, _, _, _, _, _, _, _)    => List(Some(ActualAnswer.DateAnswer(now.toLocalDate)))
      case Question.AddressQuestion(_, _, _, _, _, _, _, _) => List(Some(ActualAnswer.AddressAnswer(address)))

      case Question.MultiChoiceQuestion(id, _, _, _, _, _, marking, absence, _) =>
        marking.map {
          case (pa, mark) if (mark == desiredMark) => Some(ActualAnswer.MultipleChoiceAnswer(Set(pa.value)))
          case _                                   => None
        }
          .toList ++
          List(absence.flatMap(a => if (a._2 == desiredMark) Some(ActualAnswer.NoAnswer) else None))
    }

    Map(question.id -> Random.shuffle(
      answers.collect {
        case Some(a) => a
      }
    ).headOption)
  }

  def answersForQuestionnaire(desiredMark: Mark)(questionnaire: Questionnaire): Map[Question.Id, ActualAnswer] = {
    questionnaire.questions
      .toList
      .map(qi => qi.question)
      .flatMap(x => answerForQuestion(desiredMark)(x))
      .collect {
        case (id, Some(a)) => id -> a
      }
      .toMap
  }

  def answersForGroups(desiredMark: Mark)(groups: NonEmptyList[GroupOfQuestionnaires]): Map[Question.Id, ActualAnswer] = {
    groups
      .flatMap(g => g.links)
      .toList
      .flatMap(qn => answersForQuestionnaire(desiredMark)(qn))
      .toMap
  }
}

object AnsweringQuestionsHelper extends AnsweringQuestionsHelper

trait MarkedSubmissionsTestData extends SubmissionsTestData with AnsweringQuestionsHelper {

  val markedAnswers: Map[Question.Id, Mark] = Map(
    (OrganisationDetails.questionOrgType.id       -> Mark.Pass),
    (OrganisationDetails.questionCompanyNumber.id -> Mark.Pass),
    (OrganisationDetails.questionLtdOrgName.id    -> Mark.Pass),
    (ResponsibleIndividualDetails.question3.id    -> Mark.Pass),
    (ResponsibleIndividualDetails.question4.id    -> Mark.Pass),
    (ResponsibleIndividualDetails.question6.id    -> Mark.Fail)
  )

  val markedSubmission = MarkedSubmission(submittedSubmission, markedAnswers)

  def markAsPass(requestedBy: String = "bob@example.com")(submission: Submission): MarkedSubmission = {
    val answers = answersForGroups(Mark.Pass)(submission.groups)
    val marks   = answers.map { case (q, a) => q -> Mark.Pass }

    MarkedSubmission(submission.hasCompletelyAnsweredWith(answers), marks)
  }
}
