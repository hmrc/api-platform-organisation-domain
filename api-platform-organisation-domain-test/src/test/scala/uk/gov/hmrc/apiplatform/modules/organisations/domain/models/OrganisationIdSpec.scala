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

package uk.gov.hmrc.apiplatform.modules.organisations.domain.models

import play.api.libs.json.{JsString, Json}
import uk.gov.hmrc.apiplatform.modules.common.utils.BaseJsonFormattersSpec

class OrganisationIdSpec extends BaseJsonFormattersSpec {

  "OrganisationId" should {
    "toString" in {
      val orgId = OrganisationId.random
      orgId.toString shouldBe orgId.value.toString()
    }

    "convert to json" in {
      val org = OrganisationId.random
      Json.toJson[OrganisationId](org) shouldBe JsString(org.toString())
    }

    "read from json" in {
      val org = OrganisationId.random
      testFromJson[OrganisationId](JsString(org.toString()).toString())(org)
    }
  }

}
