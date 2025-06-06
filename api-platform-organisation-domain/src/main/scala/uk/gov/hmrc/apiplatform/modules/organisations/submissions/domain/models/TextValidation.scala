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

import scala.util.{Success, Try}

import org.apache.commons.validator.routines.EmailValidator

import play.api.libs.json.{Json, OFormat}

import uk.gov.hmrc.apiplatform.modules.organisations.domain.models.ValidatedOrganisationName

sealed trait TextValidation {
  def isValid(text: String): Boolean = this.validate(text).isRight

  def validate(text: String): Either[String, String] = this match {
    case TextValidation.Url =>
      Try(new java.net.URL(text)) match {
        case Success(_) => Right(text)
        case _          => Left(s"$text is not a valid Url")
      }

    case TextValidation.Email =>
      if (TextValidation.emailValidator.isValid(text)) {
        Right(text)
      } else {
        Left(s"$text is not a valid email")
      }

    case TextValidation.MatchRegex(regex) => {
      val matcher = regex.r
      text match {
        case matcher(_*) => Right(text)
        case _           => Left(s"$text does not match expected pattern")
      }
    }

    case TextValidation.OrganisationName => {
      if (ValidatedOrganisationName.validate(text).isValid) {
        Right(text)
      } else {
        Left(s"$text is not a valid organisation name")
      }
    }

    case TextValidation.OrganisationNumber => {
      if (ValidatedOrganisationNumber.validate(text).isValid) {
        Right(text)
      } else {
        Left(s"$text is not a valid organisation number")
      }
    }

  }
}

object TextValidation {
  val emailValidator = EmailValidator.getInstance()

  case object Url                      extends TextValidation
  case class MatchRegex(regex: String) extends TextValidation
  case object Email                    extends TextValidation
  case object OrganisationName         extends TextValidation
  case object OrganisationNumber       extends TextValidation

  import uk.gov.hmrc.play.json.Union

  implicit val formatMatchRegex: OFormat[MatchRegex] = Json.format[MatchRegex]

  implicit val formatTextValidation: OFormat[TextValidation] = Union.from[TextValidation]("validationType")
    .andType("url", () => Url)
    .and[MatchRegex]("regex")
    .andType("email", () => Email)
    .andType("organisationName", () => OrganisationName)
    .andType("organisationNumber", () => OrganisationNumber)
    .format
}
