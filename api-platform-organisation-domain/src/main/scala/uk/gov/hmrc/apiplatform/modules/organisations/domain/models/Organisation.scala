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

import scala.collection.immutable.ListSet

import play.api.libs.json.{Json, OFormat}

object Organisation {

  sealed trait OrganisationType {
    val isUkLimitedCompany: Boolean                   = this == OrganisationType.UkLimitedCompany
    val isSoleTrader: Boolean                         = this == OrganisationType.SoleTrader
    val isRegisteredSociety: Boolean                  = this == OrganisationType.RegisteredSociety
    val isCharitableIncorporatedOrganisation: Boolean = this == OrganisationType.CharitableIncorporatedOrganisation
    val isNonUkWithPlaceOfBusinessInUk: Boolean       = this == OrganisationType.NonUkWithPlaceOfBusinessInUk
    val isNonUkWithoutPlaceOfBusinessInUk: Boolean    = this == OrganisationType.NonUkWithoutPlaceOfBusinessInUk
    val isGeneralPartnership: Boolean                 = this == OrganisationType.GeneralPartnership
    val isLimitedLiabilityPartnership: Boolean        = this == OrganisationType.LimitedLiabilityPartnership
    val isLimitedPartnership: Boolean                 = this == OrganisationType.LimitedPartnership
    val isScottishPartnership: Boolean                = this == OrganisationType.ScottishPartnership
    val isScottishLimitedPartnership: Boolean         = this == OrganisationType.ScottishLimitedPartnership

    val isNonUk: Boolean = this == OrganisationType.NonUkWithPlaceOfBusinessInUk ||
      this == OrganisationType.NonUkWithoutPlaceOfBusinessInUk

    val isPartnership: Boolean = this == OrganisationType.GeneralPartnership ||
      this == OrganisationType.LimitedLiabilityPartnership ||
      this == OrganisationType.LimitedPartnership ||
      this == OrganisationType.ScottishPartnership ||
      this == OrganisationType.ScottishLimitedPartnership
  }

  object OrganisationType {
    case object UkLimitedCompany                   extends OrganisationType
    case object SoleTrader                         extends OrganisationType
    case object RegisteredSociety                  extends OrganisationType
    case object CharitableIncorporatedOrganisation extends OrganisationType
    case object NonUkWithPlaceOfBusinessInUk       extends OrganisationType
    case object NonUkWithoutPlaceOfBusinessInUk    extends OrganisationType
    case object GeneralPartnership                 extends OrganisationType
    case object LimitedLiabilityPartnership        extends OrganisationType
    case object LimitedPartnership                 extends OrganisationType
    case object ScottishPartnership                extends OrganisationType
    case object ScottishLimitedPartnership         extends OrganisationType

    val values = ListSet(
      UkLimitedCompany,
      SoleTrader,
      RegisteredSociety,
      CharitableIncorporatedOrganisation,
      NonUkWithPlaceOfBusinessInUk,
      NonUkWithoutPlaceOfBusinessInUk,
      GeneralPartnership,
      LimitedLiabilityPartnership,
      LimitedPartnership,
      ScottishPartnership,
      ScottishLimitedPartnership
    )

    def apply(text: String): Option[OrganisationType] = OrganisationType.values.find(_.toString.toUpperCase == text.toUpperCase())

    def unsafeApply(text: String): OrganisationType = apply(text).getOrElse(throw new RuntimeException(s"$text is not a valid OrganisationType"))

    import play.api.libs.json.Format
    import uk.gov.hmrc.apiplatform.modules.common.domain.services.SealedTraitJsonFormatting
    implicit val format: Format[OrganisationType] = SealedTraitJsonFormatting.createFormatFor[OrganisationType]("OrganisationType", apply)
  }

  implicit val orgFormat: OFormat[Organisation] = Json.format[Organisation]
}

case class Organisation(id: OrganisationId, organisationName: OrganisationName, organisationType: Organisation.OrganisationType, members: Set[Member])
