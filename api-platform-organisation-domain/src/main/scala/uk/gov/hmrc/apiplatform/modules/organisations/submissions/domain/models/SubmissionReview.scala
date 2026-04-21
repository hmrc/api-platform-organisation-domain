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

import java.time.Instant
import scala.collection.immutable.ListSet

import play.api.libs.json.{Json, OFormat}

import uk.gov.hmrc.apiplatform.modules.organisations.domain.models.OrganisationName
import uk.gov.hmrc.apiplatform.modules.organisations.submissions.domain.models.SubmissionId

object SubmissionReview {

  sealed trait State {
    val isSubmitted: Boolean   = this == State.Submitted
    val isInProgress: Boolean  = this == State.InProgress
    val isApproved: Boolean    = this == State.Approved
    val isDeclined: Boolean    = this == State.Declined
    val isReSubmitted: Boolean = this == State.ReSubmitted
  }

  object State {
    case object Submitted   extends State
    case object InProgress  extends State
    case object Approved    extends State
    case object Declined    extends State
    case object ReSubmitted extends State

    val values = ListSet(Submitted, InProgress, Approved, Declined, ReSubmitted)

    def apply(text: String): Option[State] = State.values.find(_.toString.toUpperCase == text.toUpperCase())

    def unsafeApply(text: String): State = apply(text).getOrElse(throw new RuntimeException(s"$text is not a valid State"))

    import play.api.libs.json.Format
    import uk.gov.hmrc.apiplatform.modules.common.domain.services.SealedTraitJsonFormatting
    implicit val format: Format[State] = SealedTraitJsonFormatting.createFormatFor[State]("State", apply)
  }

  case class Event(
      description: String,
      name: String,
      timestamp: Instant,
      comment: Option[String]
    )

  object Event {
    implicit val eventFormat: OFormat[Event] = Json.format[Event]
  }

  implicit val submissionReviewFormat: OFormat[SubmissionReview] = Json.format[SubmissionReview]
}

case class SubmissionReview(
    submissionId: SubmissionId,
    organisationName: OrganisationName,
    lastUpdate: Instant,
    requestedBy: String,
    requestedOn: Instant,
    state: SubmissionReview.State,
    events: List[SubmissionReview.Event]
  )
