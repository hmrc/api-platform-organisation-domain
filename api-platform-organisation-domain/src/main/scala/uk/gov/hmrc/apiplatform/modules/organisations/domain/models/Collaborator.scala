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

import play.api.libs.json._
import uk.gov.hmrc.apiplatform.modules.common.domain.models.UserId

import uk.gov.hmrc.apiplatform.modules.organisations.domain.models.Collaborator.Roles._

sealed trait Collaborator {
  def userId: UserId

  def isAdministrator: Boolean
  def isResponsibleIndividual: Boolean

  final def describeRole: String = Collaborator.describeRole(this)

  final def role: Collaborator.Role = Collaborator.role(this)
}

object Collaborator {

  sealed trait Role {
    def isAdministrator: Boolean
    def isResponsibleIndividual: Boolean

    def displayText: String = Role.displayText(this)
  }

  object Role {

    val displayText: Role => String = {
      case Collaborator.Roles.ADMINISTRATOR          => "Administrator"
      case Collaborator.Roles.RESPONSIBLE_INDIVIDUAL => "Responsible individual"
      case Collaborator.Roles.MEMBER                 => "Member"
    }

    def apply(text: String): Option[Collaborator.Role] = text.toUpperCase() match {
      case "ADMINISTRATOR"          => Some(Collaborator.Roles.ADMINISTRATOR)
      case "RESPONSIBLE_INDIVIDUAL" => Some(Collaborator.Roles.RESPONSIBLE_INDIVIDUAL)
      case "MEMBER"                 => Some(Collaborator.Roles.MEMBER)
      case _                        => None
    }

    private val convert: String => JsResult[Role] = (s) => Role(s).fold[JsResult[Role]](JsError(s"$s is not a role"))(role => JsSuccess(role))

    implicit val reads: Reads[Role] = (JsPath.read[String]).flatMapResult(convert(_))

    implicit val writes: Writes[Role] = Writes[Role](role => JsString(role.toString))

    implicit val format: Format[Role] = Format(reads, writes)
  }

  object Roles {

    case object ADMINISTRATOR extends Role {
      val isAdministrator         = true
      val isResponsibleIndividual = false
    }

    case object RESPONSIBLE_INDIVIDUAL extends Role {
      val isAdministrator         = true
      val isResponsibleIndividual = true
    }

    case object MEMBER extends Role {
      val isAdministrator         = false
      val isResponsibleIndividual = false
    }

  }

  def apply(role: Role, userId: UserId): Collaborator = {
    role match {
      case ADMINISTRATOR          => Collaborators.Administrator(userId)
      case RESPONSIBLE_INDIVIDUAL => Collaborators.ResponsibleIndividual(userId)
      case MEMBER                 => Collaborators.Member(userId)
    }
  }

  def role(me: Collaborator): Collaborator.Role = me match {
    case a: Collaborators.Administrator         => Collaborator.Roles.ADMINISTRATOR
    case r: Collaborators.ResponsibleIndividual => Collaborator.Roles.RESPONSIBLE_INDIVIDUAL
    case m: Collaborators.Member                => Collaborator.Roles.MEMBER
  }

  def describeRole(me: Collaborator): String = me match {
    case a: Collaborators.Administrator         => Roles.ADMINISTRATOR.toString
    case r: Collaborators.ResponsibleIndividual => Roles.RESPONSIBLE_INDIVIDUAL.toString
    case m: Collaborators.Member                => Roles.MEMBER.toString
  }

  import play.api.libs.json.Json
  import play.api.libs.json.OFormat
  import uk.gov.hmrc.play.json.Union

  implicit val formatAdministrator: OFormat[Collaborators.Administrator]                 = Json.format[Collaborators.Administrator]
  implicit val formatResponsibleIndividual: OFormat[Collaborators.ResponsibleIndividual] = Json.format[Collaborators.ResponsibleIndividual]
  implicit val formatMember: OFormat[Collaborators.Member]                               = Json.format[Collaborators.Member]

  implicit val memberJf: OFormat[Collaborator] = Union.from[Collaborator]("role")
    .and[Collaborators.Administrator](Roles.ADMINISTRATOR.toString)
    .and[Collaborators.ResponsibleIndividual](Roles.RESPONSIBLE_INDIVIDUAL.toString)
    .and[Collaborators.Member](Roles.MEMBER.toString)
    .format
}

object Collaborators {

  case class Administrator(userId: UserId) extends Collaborator {
    val isAdministrator         = true
    val isResponsibleIndividual = false
  }

  case class ResponsibleIndividual(userId: UserId) extends Collaborator {
    val isAdministrator         = true
    val isResponsibleIndividual = true
  }

  case class Member(userId: UserId) extends Collaborator {
    val isAdministrator         = false
    val isResponsibleIndividual = false
  }
}
