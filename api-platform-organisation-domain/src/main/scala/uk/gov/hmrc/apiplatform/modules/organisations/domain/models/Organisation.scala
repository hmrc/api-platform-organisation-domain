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
import scala.collection.immutable.ListSet

import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.apiplatform.modules.common.domain.models.OrganisationId

object Organisation {

  enum OrganisationType:
    case UkLimitedCompany
    case SoleTrader
    case RegisteredSociety
    case CharitableIncorporatedOrganisation
    case NonUkWithPlaceOfBusinessInUk
    case NonUkWithoutPlaceOfBusinessInUk
    case GeneralPartnership
    case LimitedLiabilityPartnership
    case LimitedPartnership
    case ScottishPartnership
    case ScottishLimitedPartnership

  object OrganisationType {

    extension (ot: OrganisationType) {
      def isUkLimitedCompany: Boolean                   = ot == OrganisationType.UkLimitedCompany
      def isSoleTrader: Boolean                         = ot == OrganisationType.SoleTrader
      def isRegisteredSociety: Boolean                  = ot == OrganisationType.RegisteredSociety
      def isCharitableIncorporatedOrganisation: Boolean = ot == OrganisationType.CharitableIncorporatedOrganisation
      def isNonUkWithPlaceOfBusinessInUk: Boolean       = ot == OrganisationType.NonUkWithPlaceOfBusinessInUk
      def isNonUkWithoutPlaceOfBusinessInUk: Boolean    = ot == OrganisationType.NonUkWithoutPlaceOfBusinessInUk
      def isGeneralPartnership: Boolean                 = ot == OrganisationType.GeneralPartnership
      def isLimitedLiabilityPartnership: Boolean        = ot == OrganisationType.LimitedLiabilityPartnership
      def isLimitedPartnership: Boolean                 = ot == OrganisationType.LimitedPartnership
      def isScottishPartnership: Boolean                = ot == OrganisationType.ScottishPartnership
      def isScottishLimitedPartnership: Boolean         = ot == OrganisationType.ScottishLimitedPartnership

      def isNonUk: Boolean = ot == OrganisationType.NonUkWithPlaceOfBusinessInUk ||
        ot == OrganisationType.NonUkWithoutPlaceOfBusinessInUk

      def isPartnership: Boolean = ot == OrganisationType.GeneralPartnership ||
        ot == OrganisationType.LimitedLiabilityPartnership ||
        ot == OrganisationType.LimitedPartnership ||
        ot == OrganisationType.ScottishPartnership ||
        ot == OrganisationType.ScottishLimitedPartnership
    }

    def apply(text: String): Option[OrganisationType] = OrganisationType.values.find(_.toString.toUpperCase == text.toUpperCase())

    def unsafeApply(text: String): OrganisationType = apply(text).getOrElse(throw new RuntimeException(s"$text is not a valid OrganisationType"))

    import play.api.libs.json.Format
    import uk.gov.hmrc.apiplatform.modules.common.domain.services.SimpleEnumJsonFormatting
    given Format[OrganisationType] = SimpleEnumJsonFormatting.createStringFormatFor[OrganisationType]("OrganisationType", apply)
  }

  given OFormat[Organisation] = Json.format[Organisation]
}

case class Organisation(
    id: OrganisationId,
    organisationName: OrganisationName,
    organisationType: Organisation.OrganisationType,
    createdDateTime: Instant,
    collaborators: Set[Collaborator]
  )
