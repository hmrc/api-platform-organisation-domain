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

import org.scalatest.Inside

import uk.gov.hmrc.apiplatform.modules.common.utils.HmrcSpec

import uk.gov.hmrc.apiplatform.modules.organisations.submissions.domain.models.*
import uk.gov.hmrc.apiplatform.modules.organisations.submissions.utils.{AsIdsHelpers, QuestionBuilder, SubmissionsTestData}

class ActualAnswersAsTextSpec extends HmrcSpec with Inside with QuestionBuilder with AsIdsHelpers {

  trait Setup extends SubmissionsTestData

  "ActualAnswersAsText" when {
    "address answer" should {
      "return address" in new Setup {
        val addLineOne   = "1 main st"
        val addLineTwo   = "line two"
        val locality     = "city"
        val region       = "region"
        val postcode     = "A12 3BC"
        val actualAnswer = ActualAnswer.AddressAnswer(RegisteredOfficeAddress(Some(addLineOne), Some(addLineTwo), Some(locality), Some(region), Some(postcode)))

        ActualAnswersAsText(actualAnswer) shouldBe s"$addLineOne, $addLineTwo, $locality, $region, $postcode"
      }
    }

    "international address answer" should {
      "return address" in new Setup {
        val addLineOne   = "1 Cour Victor Hugo"
        val addLineTwo   = "line two"
        val addLineThree = "line three"
        val locality     = "St Etienne"
        val region       = "Auvergne"
        val postcode     = "12345"
        val country      = "France"
        val actualAnswer =
          ActualAnswer.InternationalAddressAnswer(InternationalAddress(
            Some(addLineOne),
            Some(addLineTwo),
            Some(addLineThree),
            Some(locality),
            Some(region),
            Some(postcode),
            Some(country)
          ))

        ActualAnswersAsText(actualAnswer) shouldBe s"$addLineOne, $addLineTwo, $addLineThree, $locality, $region, $postcode, $country"
      }
    }

    "name answer" should {
      "return name" in new Setup {
        val firstName    = "Bob"
        val lastName     = "Fleming"
        val actualAnswer = ActualAnswer.NameAnswer(FullName(Some("yes"), Some(firstName), Some(lastName)))

        ActualAnswersAsText(actualAnswer) shouldBe s"$firstName $lastName"
      }
    }
  }
}
