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

  enum State:
    case Submitted, InProgress, Approved, Declined, ReSubmitted

  object State {

    extension (state: State) {
      def isSubmitted: Boolean   = state == State.Submitted
      def isInProgress: Boolean  = state == State.InProgress
      def isApproved: Boolean    = state == State.Approved
      def isDeclined: Boolean    = state == State.Declined
      def isReSubmitted: Boolean = state == State.ReSubmitted
    }

    def apply(text: String): Option[State] = State.values.find(_.toString.toUpperCase == text.toUpperCase())

    def unsafeApply(text: String): State = apply(text).getOrElse(throw new RuntimeException(s"$text is not a valid State"))

    import play.api.libs.json.Format
    import uk.gov.hmrc.apiplatform.modules.common.domain.services.SimpleEnumJsonFormatting
    given Format[State] = SimpleEnumJsonFormatting.createEnumFormatFor[State]("State", apply)
  }

  case class Event(
      description: String,
      name: String,
      timestamp: Instant,
      comment: Option[String]
    )

  object Event {
    given OFormat[Event] = Json.format[Event]
  }

  given OFormat[SubmissionReview] = Json.format[SubmissionReview]
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
