/*
 * Copyright 2026 HM Revenue & Customs
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

import play.api.libs.json.{JsObject, Json}
import uk.gov.hmrc.apiplatform.modules.common.utils.HmrcSpec

class CountrySpec extends HmrcSpec {

  "Country" should {
    "load all 196 countries in file order" in {
      Country.countries should have size 196
      Country.countries.head shouldBe Country("AF", "Afghanistan")
      Country.countries.last shouldBe Country("ZW", "Zimbabwe")
      Country.countries.map(country => country.code -> country.name) shouldBe
        Json.parse(getClass.getResourceAsStream("/countries.json")).as[Seq[JsObject]].map(country =>
          (country \ "code").as[String] -> (country \ "name").as[String]
        )
    }
  }
}
