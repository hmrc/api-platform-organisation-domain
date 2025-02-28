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

package uk.gov.hmrc.apiplatform.modules.organisations.domain.models

import play.api.libs.json.Json
import uk.gov.hmrc.apiplatform.modules.common.domain.models.{LaxEmailAddress, UserId}
import uk.gov.hmrc.apiplatform.modules.common.utils.BaseJsonFormattersSpec

class OrganisationSpec extends BaseJsonFormattersSpec {

  def jsonOrganisation(organisationId: OrganisationId, organisationName: OrganisationName, userId: UserId, email: LaxEmailAddress) = {
    s"""{
       |  "id" : "${organisationId.value.toString()}",
       |  "organisationName" : "${organisationName.value}",
       |  "members" : [ {
       |    "userId" : "${userId.value.toString()}",
       |    "emailAddress" : "${email.text}"
       |  } ]
       |}""".stripMargin
  }

  val userId  = UserId.random
  val email   = LaxEmailAddress("my-email@example.com")
  val orgId   = OrganisationId.random
  val orgName = OrganisationName("My org")

  "Organisation" should {
    "convert to json" in {
      Json.prettyPrint(Json.toJson[Organisation](Organisation(orgId, orgName, Set(Member(userId, email))))) shouldBe jsonOrganisation(orgId, orgName, userId, email)
    }

    "read from json" in {
      testFromJson[Organisation](jsonOrganisation(orgId, orgName, userId, email))(Organisation(orgId, orgName, Set(Member(userId, email))))
    }
  }
}
