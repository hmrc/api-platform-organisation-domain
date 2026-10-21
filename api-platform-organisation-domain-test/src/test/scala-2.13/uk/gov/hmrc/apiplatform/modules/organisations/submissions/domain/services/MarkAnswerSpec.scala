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

import uk.gov.hmrc.apiplatform.modules.common.utils.HmrcSpec

import uk.gov.hmrc.apiplatform.modules.organisations.submissions.domain.models._
import uk.gov.hmrc.apiplatform.modules.organisations.submissions.utils.{MarkedSubmissionsTestData, SubmissionsTestData}

class MarkAnswerSpec extends HmrcSpec {

  trait Setup extends SubmissionsTestData

  "markSubmission" should {
    "pass a conditional text answer with Yes" in new Setup {
      val questionWeb = ResponsibleIndividualDetails.question6
      val answers     = Map(questionWeb.id -> ActualAnswer.ConditionalTextAnswer(ConditionalText(Some("Yes"), Some("https://example.com"))))
      val submission  = Submission.updateLatestAnswersTo(answers)(submittedSubmission)

      MarkAnswer.markSubmission(submission) shouldBe Map(questionWeb.id -> Mark.Pass)
    }

    "pass a conditional text answer with No" in new Setup {
      val questionWeb = ResponsibleIndividualDetails.question6
      val answers     = Map(questionWeb.id -> ActualAnswer.ConditionalTextAnswer(ConditionalText(Some("No"), None)))
      val submission  = Submission.updateLatestAnswersTo(answers)(submittedSubmission)

      MarkAnswer.markSubmission(submission) shouldBe Map(questionWeb.id -> Mark.Pass)
    }

    "keep the failing marked-submission fixture consistent with its answers" in new MarkedSubmissionsTestData {
      val questionIds = Set(OrganisationDetails.questionLtdOrgName.id, ResponsibleIndividualDetails.question6.id)
      val answers     = markedSubmission.submission.latestInstance.answersToQuestions.filter { case (id, _) => questionIds.contains(id) }
      val submission  = Submission.updateLatestAnswersTo(answers)(markedSubmission.submission)
      val marks       = MarkAnswer.markSubmission(submission)

      markedSubmission.isFail shouldBe true
      marks(OrganisationDetails.questionLtdOrgName.id) shouldBe Mark.Fail
      marks(ResponsibleIndividualDetails.question6.id) shouldBe Mark.Pass
    }
  }
}
