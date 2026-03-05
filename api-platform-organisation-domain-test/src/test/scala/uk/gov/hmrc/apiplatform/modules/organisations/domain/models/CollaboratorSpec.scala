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
import uk.gov.hmrc.apiplatform.modules.common.domain.models.UserId
import uk.gov.hmrc.apiplatform.modules.common.utils.BaseJsonFormattersSpec

import uk.gov.hmrc.apiplatform.modules.organisations.domain.models.Collaborator.{Role, Roles}
import uk.gov.hmrc.apiplatform.modules.organisations.domain.models.Collaborators.{Administrator, Member, ResponsibleIndividual}

class CollaboratorSpec extends BaseJsonFormattersSpec {

  def jsonCollaborator(role: Role, userId: UserId) = {
    s"""{
       |  "userId" : "${userId.value.toString()}",
       |  "role" : "${role.toString()}"
       |}""".stripMargin
  }

  val userId = UserId.random

  "Collaborator" should {
    "convert to json" in {
      Json.prettyPrint(Json.toJson[Collaborator](Member(userId))) shouldBe jsonCollaborator(Roles.Member, userId)
      Json.prettyPrint(Json.toJson[Collaborator](Administrator(userId))) shouldBe jsonCollaborator(Roles.Administrator, userId)
      Json.prettyPrint(Json.toJson[Collaborator](ResponsibleIndividual(userId))) shouldBe jsonCollaborator(Roles.ResponsibleIndividual, userId)
    }

    "read from json" in {
      testFromJson[Collaborator](jsonCollaborator(Roles.Member, userId))(Member(userId))
      testFromJson[Collaborator](jsonCollaborator(Roles.Administrator, userId))(Administrator(userId))
      testFromJson[Collaborator](jsonCollaborator(Roles.ResponsibleIndividual, userId))(ResponsibleIndividual(userId))
    }

    "apply" in {
      Role.apply("Administrator") shouldBe Some(Roles.Administrator)
      Role.apply("ResponsibleIndividual") shouldBe Some(Roles.ResponsibleIndividual)
      Role.apply("Member") shouldBe Some(Roles.Member)
      Role.apply("MEMBER") shouldBe Some(Roles.Member)
      Role.apply("random") shouldBe None
    }
  }
}
