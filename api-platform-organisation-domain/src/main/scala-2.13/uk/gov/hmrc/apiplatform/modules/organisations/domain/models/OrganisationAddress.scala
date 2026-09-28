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

import play.api.libs.json.{Json, OFormat}

case class OrganisationAddress(
    addressLineOne: Option[String] = None,
    addressLineTwo: Option[String] = None,
    addressLineThree: Option[String] = None,
    careOf: Option[String] = None,
    country: Option[String] = None,
    locality: Option[String] = None,
    poBox: Option[String] = None,
    postalCode: Option[String] = None,
    premises: Option[String] = None,
    region: Option[String] = None
  )

object OrganisationAddress {
  implicit val orgAddressFormat: OFormat[OrganisationAddress] = Json.format[OrganisationAddress]
}
