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

import scala.io.Source

import play.api.libs.json._
import uk.gov.hmrc.apiplatform.modules.common.domain.models.UserId
import uk.gov.hmrc.apiplatform.modules.common.utils.BaseJsonFormattersSpec

import uk.gov.hmrc.apiplatform.modules.organisations.domain.models.OrganisationId
import uk.gov.hmrc.apiplatform.modules.organisations.submissions.domain.models.SubmissionId
import uk.gov.hmrc.apiplatform.modules.organisations.submissions.utils.SubmissionsTestData

class SubmissionSpec extends BaseJsonFormattersSpec with SubmissionsTestData {

  import Submission.extendedSubmissionFormat

  val submission         = partiallyAnsweredExtendedSubmission.submission.copy(
    id = SubmissionId.unsafeApply("123a1dd3-09cf-46a0-bc73-350a385de678"),
    organisationId = Some(OrganisationId.unsafeApply("426a1df9-09cf-46a0-bc73-350a385ea461")),
    startedBy = UserId.unsafeApply("193316a2-e3b4-4aed-9001-a318e841cbb3")
  )
  val extendedSubmission = partiallyAnsweredExtendedSubmission.copy(submission = submission)

  "submission questionIdsOfInterest org name" in {
    Submission.updateLatestAnswersTo(samplePassAnswersToQuestions)(aSubmission).latestInstance.answersToQuestions(
      aSubmission.questionIdsOfInterest.organisationTypeId
    ) shouldBe ActualAnswer.SingleChoiceAnswer("UK limited company")
  }

  "submission instance state history" in {
    aSubmission.latestInstance.statusHistory.head.isOpenToAnswers shouldBe true
    aSubmission.latestInstance.isOpenToAnswers shouldBe true
    aSubmission.status.isOpenToAnswers shouldBe true
  }

  "submission instance is in progress" in {
    aSubmission.latestInstance.isOpenToAnswers shouldBe true
  }

  "submission is in progress" in {
    aSubmission.status.isOpenToAnswers shouldBe true
  }

  "submission is read only" in {
    aSubmission.status.isReadOnly shouldBe false
    submittedSubmission.status.isReadOnly shouldBe true
    grantedSubmission.status.isReadOnly shouldBe true
    grantedWithWarningsSubmission.status.isReadOnly shouldBe true
    declinedSubmission.instances.tail.head.status.isReadOnly shouldBe true
    failSubmission.status.isReadOnly shouldBe true
    warningsSubmission.status.isReadOnly shouldBe true
    pendingRISubmission.status.isReadOnly shouldBe true
  }

  "submission findQuestionnaireContaining" in {
    aSubmission.findQuestionnaireContaining(aSubmission.questionIdsOfInterest.organisationTypeId) shouldBe Some(OrganisationDetails.questionnaire)
  }

  "submission setLatestAnswers" in {
    val newAnswersToQuestions = Map(
      (OrganisationDetails.questionOrgType.id -> ActualAnswer.TextAnswer("new web site"))
    )

    Submission.updateLatestAnswersTo(newAnswersToQuestions)(aSubmission).latestInstance.answersToQuestions(OrganisationDetails.questionOrgType.id) shouldBe ActualAnswer.TextAnswer(
      "new web site"
    )
  }

  "submission automaticallyMark pass" in {
    val answering1       = Submission.addStatusHistory(Submission.Status.Answering(instant, false))(aSubmission)
    val answering2       = Submission.updateLatestAnswersTo(samplePassAnswersToQuestions)(answering1)
    val answered         = Submission.addStatusHistory(Submission.Status.Answering(instant, true))(answering2)
    val submitted        = Submission.submit(instant, "bob@example.com")(answered)
    val markedSubmission = Submission.automaticallyMark(instant, "bob@example.com")(submitted)
    markedSubmission.latestInstance.isGranted shouldBe true
    markedSubmission.latestInstance.status shouldBe Submission.Status.Granted(instant, "bob@example.com", Some("Automatically passed"), None)
  }

  "submission automaticallyMark fail" in {
    val answering1       = Submission.addStatusHistory(Submission.Status.Answering(instant, false))(aSubmission)
    val answering2       = Submission.updateLatestAnswersTo(sampleFailAnswersToQuestions)(answering1)
    val answered         = Submission.addStatusHistory(Submission.Status.Answering(instant, true))(answering2)
    val submitted        = Submission.submit(instant, "bob@example.com")(answered)
    val markedSubmission = Submission.automaticallyMark(instant, "bob@example.com")(submitted)
    markedSubmission.latestInstance.isFailed shouldBe true
    markedSubmission.latestInstance.status shouldBe Submission.Status.Failed(instant, "bob@example.com")
  }

  "submission automaticallyMark warning" in {
    val answering1       = Submission.addStatusHistory(Submission.Status.Answering(instant, false))(aSubmission)
    val answering2       = Submission.updateLatestAnswersTo(sampleWarningsAnswersToQuestions)(answering1)
    val answered         = Submission.addStatusHistory(Submission.Status.Answering(instant, true))(answering2)
    val submitted        = Submission.submit(instant, "bob@example.com")(answered)
    val markedSubmission = Submission.automaticallyMark(instant, "bob@example.com")(submitted)
    markedSubmission.latestInstance.isWarnings shouldBe true
    markedSubmission.latestInstance.status shouldBe Submission.Status.Warnings(instant, "bob@example.com")
  }

  "questionnaire state description" in {
    QuestionnaireState.describe(QuestionnaireState.NotStarted) shouldBe "Not Started"
    QuestionnaireState.describe(QuestionnaireState.InProgress) shouldBe "In Progress"
    QuestionnaireState.describe(QuestionnaireState.NotApplicable) shouldBe "Not Applicable"
    QuestionnaireState.describe(QuestionnaireState.Completed) shouldBe "Completed"
  }

  "questionnaire state is completed" in {
    QuestionnaireState.isCompleted(QuestionnaireState.NotStarted) shouldBe false
    QuestionnaireState.isCompleted(QuestionnaireState.InProgress) shouldBe false
    QuestionnaireState.isCompleted(QuestionnaireState.Completed) shouldBe true
  }

  "shouldAsk" in {
    AskWhen.shouldAsk(standardContext, answersToQuestions)(OrganisationDetails.questionnaire.questions.head.askWhen) shouldBe true
    AskWhen.shouldAsk(standardContext, answersToQuestions)(OrganisationDetails.questionnaire.questions.tail.head.askWhen) shouldBe true
    AskWhen.shouldAsk(standardContext, answersToQuestions)(ResponsibleIndividualDetails.questionnaire.questions.tail.tail.head.askWhen) shouldBe true
  }

  "submission status isOpenToAnswers" in {
    Submission.Status.Answering(instant, false).isOpenToAnswers shouldBe true
  }

  "submission status isCreated" in {
    Submission.Status.Answering(instant, false).isCreated shouldBe false
  }

  "submission status isGrantedWithOrWithoutWarnings" in {
    Submission.Status.Answering(instant, false).isGrantedWithOrWithoutWarnings shouldBe false
    Submission.Status.Granted(instant, "bob@example.com", None, None).isGrantedWithOrWithoutWarnings shouldBe true
    Submission.Status.GrantedWithWarnings(instant, "bob@example.com", "warnings", None).isGrantedWithOrWithoutWarnings shouldBe true
  }

  "submission status isGrantedWithWarnings" in {
    Submission.Status.GrantedWithWarnings(instant, "bob@example.com", "warnings", None).isGrantedWithWarnings shouldBe true
  }

  "submission status isDeclined" in {
    Submission.Status.Declined(instant, "bob@example.com", "reasons").isDeclined shouldBe true
  }

  "submission status isPendingResponsibleIndividual" in {
    Submission.Status.PendingResponsibleIndividual(instant, "bob@example.com").isPendingResponsibleIndividual shouldBe true
  }

  "submission status isLegalTransition" in {
    Submission.Status.isLegalTransition(
      Submission.Status.PendingResponsibleIndividual(instant, "bob@example.com"),
      Submission.Status.Failed(instant, "bob@example.com")
    ) shouldBe true
    Submission.Status.isLegalTransition(
      Submission.Status.PendingResponsibleIndividual(instant, "bob@example.com"),
      Submission.Status.Warnings(instant, "bob@example.com")
    ) shouldBe true
    Submission.Status.isLegalTransition(
      Submission.Status.PendingResponsibleIndividual(instant, "bob@example.com"),
      Submission.Status.Granted(instant, "bob@example.com", Some("comments"), None)
    ) shouldBe true
    Submission.Status.isLegalTransition(
      Submission.Status.PendingResponsibleIndividual(instant, "bob@example.com"),
      Submission.Status.Declined(instant, "bob@example.com", "reasons")
    ) shouldBe true
    Submission.Status.isLegalTransition(
      Submission.Status.Failed(instant, "bob@example.com"),
      Submission.Status.Granted(instant, "bob@example.com", Some("comments"), None)
    ) shouldBe true
    Submission.Status.isLegalTransition(
      Submission.Status.Failed(instant, "bob@example.com"),
      Submission.Status.Declined(instant, "bob@example.com", "reasons")
    ) shouldBe true
    Submission.Status.isLegalTransition(
      Submission.Status.Warnings(instant, "bob@example.com"),
      Submission.Status.Granted(instant, "bob@example.com", Some("comments"), None)
    ) shouldBe true
    Submission.Status.isLegalTransition(
      Submission.Status.Warnings(instant, "bob@example.com"),
      Submission.Status.GrantedWithWarnings(instant, "bob@example.com", "warnings", None)
    ) shouldBe true
    Submission.Status.isLegalTransition(
      Submission.Status.GrantedWithWarnings(instant, "bob@example.com", "warnings", None),
      Submission.Status.Declined(instant, "bob@example.com", "reasons")
    ) shouldBe true
    Submission.Status.isLegalTransition(
      Submission.Status.GrantedWithWarnings(instant, "bob@example.com", "warnings", None),
      Submission.Status.Granted(instant, "bob@example.com", None, None)
    ) shouldBe true
    Submission.Status.isLegalTransition(
      Submission.Status.Granted(instant, "bob@example.com", Some("comments"), None),
      Submission.Status.Declined(instant, "bob@example.com", "reasons")
    ) shouldBe true
    Submission.Status.isLegalTransition(
      Submission.Status.Answering(instant, false),
      Submission.Status.Granted(instant, "bob@example.com", None, None)
    ) shouldBe false
  }

  "toJson for extended submission" in {
    val jsonExtendedSubmission = Source.fromResource(s"./submissions/extended-submission-valid.json").mkString
    Json.prettyPrint(Json.toJson(extendedSubmission)) shouldBe jsonExtendedSubmission
  }

  "read extended submssion from json" in {
    val jsonExtendedSubmission = Source.fromResource(s"./submissions/extended-submission-valid.json").mkString
    testFromJson[ExtendedSubmission](jsonExtendedSubmission)(extendedSubmission)
  }

  "read invalid extended submission questionnaire state from json" in {
    val jsonInvalidExtendedSubmission = Source.fromResource(s"./submissions/extended-submission-invalid.json").mkString
    intercept[Exception] {
      testFromJson[ExtendedSubmission](jsonInvalidExtendedSubmission)(extendedSubmission)
    }
  }
}
