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
      case Collaborator.Roles.Administrator         => "Administrator"
      case Collaborator.Roles.ResponsibleIndividual => "Responsible individual"
      case Collaborator.Roles.Member                => "Member"
    }

    def apply(text: String): Option[Collaborator.Role] = text.toUpperCase() match {
      case "ADMINISTRATOR"         => Some(Collaborator.Roles.Administrator)
      case "RESPONSIBLEINDIVIDUAL" => Some(Collaborator.Roles.ResponsibleIndividual)
      case "MEMBER"                => Some(Collaborator.Roles.Member)
      case _                       => None
    }

    private val convert: String => JsResult[Role] = (s) => Role(s).fold[JsResult[Role]](JsError(s"$s is not a role"))(role => JsSuccess(role))

    implicit val reads: Reads[Role] = (JsPath.read[String]).flatMapResult(convert(_))

    implicit val writes: Writes[Role] = Writes[Role](role => JsString(role.toString))

    implicit val format: Format[Role] = Format(reads, writes)
  }

  object Roles {

    case object Administrator extends Role {
      val isAdministrator         = true
      val isResponsibleIndividual = false
    }

    case object ResponsibleIndividual extends Role {
      val isAdministrator         = true
      val isResponsibleIndividual = true
    }

    case object Member extends Role {
      val isAdministrator         = false
      val isResponsibleIndividual = false
    }

  }

  def apply(role: Role, userId: UserId): Collaborator = {
    role match {
      case Administrator         => Collaborators.Administrator(userId)
      case ResponsibleIndividual => Collaborators.ResponsibleIndividual(userId)
      case Member                => Collaborators.Member(userId)
    }
  }

  def role(me: Collaborator): Collaborator.Role = me match {
    case a: Collaborators.Administrator         => Collaborator.Roles.Administrator
    case r: Collaborators.ResponsibleIndividual => Collaborator.Roles.ResponsibleIndividual
    case m: Collaborators.Member                => Collaborator.Roles.Member
  }

  def describeRole(me: Collaborator): String = me match {
    case a: Collaborators.Administrator         => Roles.Administrator.toString
    case r: Collaborators.ResponsibleIndividual => Roles.ResponsibleIndividual.toString
    case m: Collaborators.Member                => Roles.Member.toString
  }

  import play.api.libs.json.Json
  import play.api.libs.json.OFormat
  import uk.gov.hmrc.play.json.Union

  implicit val formatAdministrator: OFormat[Collaborators.Administrator]                 = Json.format[Collaborators.Administrator]
  implicit val formatResponsibleIndividual: OFormat[Collaborators.ResponsibleIndividual] = Json.format[Collaborators.ResponsibleIndividual]
  implicit val formatMember: OFormat[Collaborators.Member]                               = Json.format[Collaborators.Member]

  implicit val collaboratorJf: OFormat[Collaborator] = Union.from[Collaborator]("role")
    .and[Collaborators.Administrator](Roles.Administrator.toString)
    .and[Collaborators.ResponsibleIndividual](Roles.ResponsibleIndividual.toString)
    .and[Collaborators.Member](Roles.Member.toString)
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
