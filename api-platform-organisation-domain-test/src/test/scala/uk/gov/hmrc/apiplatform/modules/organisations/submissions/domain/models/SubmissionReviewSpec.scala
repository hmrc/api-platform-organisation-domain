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

import play.api.libs.json.Json
import uk.gov.hmrc.apiplatform.modules.common.utils.{BaseJsonFormattersSpec, FixedClock}

import uk.gov.hmrc.apiplatform.modules.organisations.domain.models.OrganisationName

class SubmissionReviewSpec extends BaseJsonFormattersSpec with FixedClock {

  def jsonSubmissionReview(submissionId: SubmissionId, instanceIndex: Int, organisationName: OrganisationName) = {
    s"""{
       |  "submissionId" : "${submissionId.value.toString()}",
       |  "instanceIndex" : $instanceIndex,
       |  "organisationName" : "${organisationName.value}",
       |  "lastUpdate" : "2020-01-02T03:04:05.006Z",
       |  "requestedBy" : "bob@example.com",
       |  "requestedOn" : "2020-01-02T03:04:05.006Z",
       |  "state" : "Submitted",
       |  "events" : [ {
       |    "description" : "Submitted",
       |    "name" : "bob@example.com",
       |    "timestamp" : "2020-01-02T03:04:05.006Z"
       |  } ]
       |}""".stripMargin
  }

  val submissionId  = SubmissionId.random
  val instanceIndex = 0
  val orgName       = OrganisationName("My org")

  val submissionReview = SubmissionReview(
    submissionId,
    instanceIndex,
    orgName,
    instant,
    "bob@example.com",
    instant,
    SubmissionReview.State.Submitted,
    List(SubmissionReview.Event("Submitted", "bob@example.com", instant, None))
  )

  "SubmissionReview" should {
    "convert to json" in {
      Json.prettyPrint(Json.toJson[SubmissionReview](submissionReview)) shouldBe jsonSubmissionReview(submissionId, instanceIndex, orgName)
    }

    "read from json" in {
      testFromJson[SubmissionReview](jsonSubmissionReview(submissionId, instanceIndex, orgName))(submissionReview)
    }
  }
}
