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

import java.time.LocalDate
sealed trait ActualAnswer

case class RegisteredOfficeAddress(
    addressLineOne: Option[String],
    addressLineTwo: Option[String],
    locality: Option[String],
    region: Option[String],
    postalCode: Option[String]
  )

case class InternationalAddress(
    addressLineOne: Option[String],
    addressLineTwo: Option[String],
    addressLineThree: Option[String],
    locality: Option[String],
    postalCode: Option[String],
    country: Option[String]
  )

case class FullName(
    isThisYourName: Option[String],
    firstName: Option[String],
    lastName: Option[String]
  )

case class Attachment(
    fileRef: Option[String],
    fileName: Option[String]
  )

object ActualAnswer {

  case class MultipleChoiceAnswer(values: Set[String])               extends ActualAnswer
  case class SingleChoiceAnswer(value: String)                       extends ActualAnswer
  case class TextAnswer(value: String)                               extends ActualAnswer
  case class DateAnswer(value: LocalDate)                            extends ActualAnswer
  case class AddressAnswer(value: RegisteredOfficeAddress)           extends ActualAnswer
  case class InternationalAddressAnswer(value: InternationalAddress) extends ActualAnswer
  case class NameAnswer(value: FullName)                             extends ActualAnswer
  case class CompanyNumberAnswer(value: String)                      extends ActualAnswer
  case class AttachmentAnswer(value: Attachment)                     extends ActualAnswer
  case object AcknowledgedAnswer                                     extends ActualAnswer
  case object NoAnswer                                               extends ActualAnswer

  import play.api.libs.json._
  import uk.gov.hmrc.play.json.Union

  implicit val jfTextAnswer: OFormat[TextAnswer]                                 = Json.format[TextAnswer]
  implicit val jfDateAnswer: OFormat[DateAnswer]                                 = Json.format[DateAnswer]
  implicit val jfAddress: OFormat[RegisteredOfficeAddress]                       = Json.format[RegisteredOfficeAddress]
  implicit val jfAddressAnswer: OFormat[AddressAnswer]                           = Json.format[AddressAnswer]
  implicit val jfInternationalAddress: OFormat[InternationalAddress]             = Json.format[InternationalAddress]
  implicit val jfInternationalAddressAnswer: OFormat[InternationalAddressAnswer] = Json.format[InternationalAddressAnswer]
  implicit val jfFullName: OFormat[FullName]                                     = Json.format[FullName]
  implicit val jfNameAnswer: OFormat[NameAnswer]                                 = Json.format[NameAnswer]
  implicit val jfCompanyNumberAnswer: OFormat[CompanyNumberAnswer]               = Json.format[CompanyNumberAnswer]
  implicit val jfAttachment: OFormat[Attachment]                                 = Json.format[Attachment]
  implicit val jfAttachmentAnswer: OFormat[AttachmentAnswer]                     = Json.format[AttachmentAnswer]
  implicit val jfSingleChoiceAnswer: OFormat[SingleChoiceAnswer]                 = Json.format[SingleChoiceAnswer]
  implicit val jfMultipleChoiceAnswer: OFormat[MultipleChoiceAnswer]             = Json.format[MultipleChoiceAnswer]

  implicit val jfActualAnswer: OFormat[ActualAnswer] = Union.from[ActualAnswer]("answerType")
    .and[MultipleChoiceAnswer]("multipleChoice")
    .and[SingleChoiceAnswer]("singleChoice")
    .and[DateAnswer]("date")
    .and[AddressAnswer]("address")
    .and[InternationalAddressAnswer]("internationalAddress")
    .and[NameAnswer]("name")
    .and[CompanyNumberAnswer]("companyNumber")
    .and[AttachmentAnswer]("attachment")
    .and[TextAnswer]("text")
    .andType("acknowledged", () => AcknowledgedAnswer)
    .andType("noAnswer", () => NoAnswer)
    .format

}
