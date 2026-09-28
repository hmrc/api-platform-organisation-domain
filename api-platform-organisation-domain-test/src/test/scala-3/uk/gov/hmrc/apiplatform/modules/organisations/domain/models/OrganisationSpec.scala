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

import java.time.Instant

import play.api.libs.json.Json
import uk.gov.hmrc.apiplatform.modules.common.domain.models.{OrganisationId, UserId}
import uk.gov.hmrc.apiplatform.modules.common.utils.{BaseJsonFormattersSpec, FixedClock}

import uk.gov.hmrc.apiplatform.modules.organisations.domain.models.Collaborator.{Role, Roles}
import uk.gov.hmrc.apiplatform.modules.organisations.domain.models.Collaborators.Member

class OrganisationSpec extends BaseJsonFormattersSpec with FixedClock {

  def jsonOrganisation(
      organisationId: OrganisationId,
      organisationName: OrganisationName,
      organisationType: Organisation.OrganisationType,
      createdDateTime: Instant,
      role: Role,
      userId: UserId
    ) = {
    s"""{
       |  "id" : "${organisationId.value.toString()}",
       |  "organisationName" : "${organisationName.value}",
       |  "organisationType" : "${organisationType.toString}",
       |  "createdDateTime" : "${createdDateTime.toString()}",
       |  "collaborators" : [ {
       |    "userId" : "${userId.value.toString()}",
       |    "role" : "${role.toString()}"
       |  } ]
       |}""".stripMargin
  }

  def jsonOrganisationWithExtraData(
      organisationId: OrganisationId,
      organisationName: OrganisationName,
      organisationType: Organisation.OrganisationType,
      createdDateTime: Instant,
      role: Role,
      userId: UserId
    ) = {
    s"""{
       |  "id" : "${organisationId.value.toString()}",
       |  "organisationName" : "${organisationName.value}",
       |  "organisationType" : "${organisationType.toString}",
       |  "createdDateTime" : "${createdDateTime.toString()}",
       |  "collaborators" : [ {
       |    "userId" : "${userId.value.toString()}",
       |    "role" : "${role.toString()}"
       |  } ],
       |  "companyNumber" : "12345678",
       |  "corporationTaxUtr" : "1234567890",
       |  "websiteUrl" : "https://www.bobsburgers.com",
       |  "address" : {
       |    "addressLineOne" : "1 main st",
       |    "addressLineTwo" : "Kings Cross",
       |    "addressLineThree" : "Zone 1",
       |    "careOf" : "Bob Roberts",
       |    "country" : "United Kingdom",
       |    "locality" : "London",
       |    "poBox" : "PO Box 123",
       |    "postalCode" : "AB1 2CD",
       |    "premises" : "Unit 1",
       |    "region" : "Greater London"
       |  }
       |}""".stripMargin
  }

  val userId          = UserId.random
  val orgId           = OrganisationId.random
  val orgName         = OrganisationName("My org")
  val orgType         = Organisation.OrganisationType.UkLimitedCompany
  val createdDateTime = instant

  val orgAddress = OrganisationAddress(
    addressLineOne = Some("1 main st"),
    addressLineTwo = Some("Kings Cross"),
    addressLineThree = Some("Zone 1"),
    careOf = Some("Bob Roberts"),
    country = Some("United Kingdom"),
    locality = Some("London"),
    poBox = Some("PO Box 123"),
    postalCode = Some("AB1 2CD"),
    premises = Some("Unit 1"),
    region = Some("Greater London")
  )

  "Organisation" should {
    "convert to json" in {
      Json.prettyPrint(Json.toJson[Organisation](Organisation(orgId, orgName, orgType, createdDateTime, Set(Member(userId))))) shouldBe jsonOrganisation(
        orgId,
        orgName,
        orgType,
        createdDateTime,
        Roles.Member,
        userId
      )
    }

    "read from json" in {
      testFromJson[Organisation](jsonOrganisation(orgId, orgName, orgType, createdDateTime, Roles.Member, userId))(Organisation(
        orgId,
        orgName,
        orgType,
        createdDateTime,
        Set(Member(userId))
      ))
    }

    "convert to json with extra organisation data" in {
      Json.prettyPrint(Json.toJson[Organisation](Organisation(
        orgId,
        orgName,
        orgType,
        createdDateTime,
        Set(Member(userId)),
        companyNumber = Some("12345678"),
        corporationTaxUtr = Some("1234567890"),
        websiteUrl = Some("https://www.bobsburgers.com"),
        address = Some(orgAddress)
      ))) shouldBe jsonOrganisationWithExtraData(orgId, orgName, orgType, createdDateTime, Roles.Member, userId)
    }

    "read from json with extra organisation data" in {
      testFromJson[Organisation](jsonOrganisationWithExtraData(orgId, orgName, orgType, createdDateTime, Roles.Member, userId))(Organisation(
        orgId,
        orgName,
        orgType,
        createdDateTime,
        Set(Member(userId)),
        companyNumber = Some("12345678"),
        corporationTaxUtr = Some("1234567890"),
        websiteUrl = Some("https://www.bobsburgers.com"),
        address = Some(orgAddress)
      ))
    }
  }
}
