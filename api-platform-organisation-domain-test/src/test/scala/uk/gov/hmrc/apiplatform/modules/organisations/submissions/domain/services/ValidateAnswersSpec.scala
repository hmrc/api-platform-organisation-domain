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

import uk.gov.hmrc.apiplatform.modules.common.utils.{FixedClock, HmrcSpec}

import uk.gov.hmrc.apiplatform.modules.organisations.submissions.domain.models._
import uk.gov.hmrc.apiplatform.modules.organisations.submissions.utils.{AsIdsHelpers, QuestionBuilder}

class ValidateAnswersSpec extends HmrcSpec with Inside with QuestionBuilder with AsIdsHelpers with FixedClock {

  import org.scalatest.prop.TableDrivenPropertyChecks._

  def answerOf(text: String*)            = Map(Question.answerKey -> text.toList)
  def noAnswer: Map[String, Seq[String]] = Map.empty

  "ValidateAnswers" should {

    "for acknowledgement questions" in {
      val question = acknowledgementOnly(1)
      type AnswerMatching = Either[Unit, ActualAnswer]
      val aFailure: AnswerMatching    = Left(())
      val validAnswer: AnswerMatching = Right(ActualAnswer.AcknowledgedAnswer)

      val passes = Table(
        ("description", "question", Question.answerKey, "expects"),
        ("valid answer", question, noAnswer, validAnswer),
        ("too many answers", question, answerOf("Yes"), aFailure)
      )

      forAll(passes) { (_: String, question: Question, answers: Map[String, Seq[String]], expects: AnswerMatching) =>
        expects match {
          case Right(answer) =>
            ValidateAnswers.validate(question, answers) shouldBe Right(answer)
          case Left(())      =>
            ValidateAnswers.validate(question, answers).left.value
        }
      }
    }

    "for single choice questions" in {
      val question         = yesNoQuestion(1)
      val optionalQuestion = question.makeOptionalPass
      type AnswerMatching = Either[Unit, ActualAnswer]
      val aFailure: AnswerMatching         = Left(())
      val validAnswer: AnswerMatching      = Right(ActualAnswer.SingleChoiceAnswer("Yes"))
      val validEmptyAnswer: AnswerMatching = Right(ActualAnswer.NoAnswer)

      val passes = Table(
        ("description", "question", Question.answerKey, "expects"),
        ("valid answer", question, answerOf("Yes"), validAnswer),
        ("too many answers", question, answerOf("Yes", "Bob"), aFailure),
        ("invalid answer", question, answerOf("Bob"), aFailure),
        ("valid answer on optional", optionalQuestion, answerOf("Yes"), validAnswer),
        ("invalid answer on optional", optionalQuestion, answerOf("Bob"), aFailure),
        ("empty answer is invalid on non optional", question, noAnswer, aFailure),
        ("empty answer list is invalid on non optional", question, answerOf(), aFailure),
        ("empty answer is valid on optional", optionalQuestion, noAnswer, validEmptyAnswer),
        ("empty answer list is valid on optional", optionalQuestion, answerOf(), validEmptyAnswer)
      )

      forAll(passes) { (_: String, question: Question, answers: Map[String, Seq[String]], expects: AnswerMatching) =>
        expects match {
          case Right(answer) =>
            ValidateAnswers.validate(question, answers) shouldBe Right(answer)
          case Left(())      =>
            ValidateAnswers.validate(question, answers).left.value
        }
      }
    }

    "for date questions" in {
      val question = dateQuestion(1)
      type AnswerMatching = Either[Unit, ActualAnswer]
      val aFailure: AnswerMatching    = Left(())
      val validAnswer: AnswerMatching = Right(ActualAnswer.DateAnswer(now.toLocalDate))
      val validRawAnswers             = Map(
        "day"   -> Seq(now.getDayOfMonth.toString),
        "month" -> Seq(now.getMonthValue.toString),
        "year"  -> Seq(now.getYear.toString)
      )

      val passes = Table(
        ("description", "question", Question.answerKey, "expects"),
        ("valid answer", question, validRawAnswers, validAnswer),
        ("too many answers", question, validRawAnswers + ("day" -> Seq("1", "2")), aFailure),
        ("invalid date", question, validRawAnswers + ("day"     -> Seq("120")), aFailure),
        ("invalid answer", question, answerOf("Bob"), aFailure)
      )

      forAll(passes) { (_: String, question: Question, answers: Map[String, Seq[String]], expects: AnswerMatching) =>
        expects match {
          case Right(answer) =>
            ValidateAnswers.validate(question, answers) shouldBe Right(answer)
          case Left(())      =>
            ValidateAnswers.validate(question, answers).left.value
        }
      }
    }

    "for address questions" in {
      val question = addressQuestion(1)
      type AnswerMatching = Either[Unit, ActualAnswer]
      val aFailure: AnswerMatching    = Left(())
      val addLineOne                  = "1 main st"
      val addLineTwo                  = "line two"
      val locality                    = "city"
      val region                      = "region"
      val postcode                    = "A12 3BC"
      val validAnswer: AnswerMatching = Right(ActualAnswer.AddressAnswer(RegisteredOfficeAddress(Some(addLineOne), Some(addLineTwo), Some(locality), Some(region), Some(postcode))))
      val validRawAnswers             = Map(
        "addressLineOne" -> Seq(addLineOne),
        "addressLineTwo" -> Seq(addLineTwo),
        "locality"       -> Seq(locality),
        "region"         -> Seq(region),
        "postcode"       -> Seq(postcode)
      )

      val passes = Table(
        ("description", "question", Question.answerKey, "expects"),
        ("valid answer", question, validRawAnswers, validAnswer),
        ("too many answers", question, validRawAnswers + ("addressLineOne" -> Seq("1", "2")), aFailure),
        ("invalid answer", question, answerOf("Bob"), aFailure)
      )

      forAll(passes) { (_: String, question: Question, answers: Map[String, Seq[String]], expects: AnswerMatching) =>
        expects match {
          case Right(answer) =>
            ValidateAnswers.validate(question, answers) shouldBe Right(answer)
          case Left(())      =>
            ValidateAnswers.validate(question, answers).left.value
        }
      }
    }

    "for multi choice questions" in {
      val question         = multichoiceQuestion(1, "One", "Two", "Three")
      val optionalQuestion = question.makeOptionalPass
      type AnswerMatching = Either[Unit, ActualAnswer]
      val aFailure: AnswerMatching          = Left(())
      val validSingleAnswer: AnswerMatching = Right(ActualAnswer.MultipleChoiceAnswer(Set("One")))
      val validMultiAnswer: AnswerMatching  = Right(ActualAnswer.MultipleChoiceAnswer(Set("One", "Two")))
      val validEmptyAnswer: AnswerMatching  = Right(ActualAnswer.NoAnswer)

      val passes = Table(
        ("description", "question", Question.answerKey, "expects"),
        ("valid answer", question, answerOf("One"), validSingleAnswer),
        ("valid answers", question, answerOf("One", "Two"), validMultiAnswer),
        ("invalid answer", question, answerOf("Zero"), aFailure),
        ("invalid answers", question, answerOf("Zero", "Fred"), aFailure),
        ("mixed validity answers", question, answerOf("Zero", "One"), aFailure),
        ("mixed validity answers", question, answerOf("One", "Zero"), aFailure),
        ("valid answer on optional", optionalQuestion, answerOf("One"), validSingleAnswer),
        ("valid answers on optional", optionalQuestion, answerOf("One", "Two"), validMultiAnswer),
        ("invalid answer on optional", optionalQuestion, answerOf("Zero"), aFailure),
        ("invalid answers on optional", optionalQuestion, answerOf("Zero", "Fred"), aFailure),
        ("mixes answers on optional", optionalQuestion, answerOf("Zero", "One"), aFailure),
        ("mixes answers on optional", optionalQuestion, answerOf("One", "Zero"), aFailure),
        ("empty answer is invalid on non optional", question, noAnswer, aFailure),
        ("empty answer is valid on optional", optionalQuestion, noAnswer, validEmptyAnswer)
      )

      forAll(passes) { (_: String, question: Question, answers: Map[String, Seq[String]], expects: AnswerMatching) =>
        expects match {
          case Right(answer) =>
            ValidateAnswers.validate(question, answers) shouldBe Right(answer)
          case Left(())      =>
            ValidateAnswers.validate(question, answers).left.value
        }
      }
    }

    "for text questions without validation" in {
      val question         = textQuestion(1)
      val optionalQuestion = question.makeOptionalPass
      type AnswerMatching = Either[Unit, ActualAnswer]
      val aFailure: AnswerMatching         = Left(())
      val validAnswer: AnswerMatching      = Right(ActualAnswer.TextAnswer("Bobby"))
      val validEmptyAnswer: AnswerMatching = Right(ActualAnswer.NoAnswer)

      val passes = Table(
        ("description", "question", Question.answerKey, "expects"),
        ("valid answer", question, answerOf("Bobby"), validAnswer),
        ("too many answers", question, answerOf("Bobby", "Fred"), aFailure),
        ("valid answer on optional", optionalQuestion, answerOf("Bobby"), validAnswer),
        ("empty answer is invalid on non optional", question, noAnswer, aFailure),
        ("empty answer is valid on optional", optionalQuestion, noAnswer, validEmptyAnswer)
      )

      forAll(passes) { (_: String, question: Question, answers: Map[String, Seq[String]], expects: AnswerMatching) =>
        expects match {
          case Right(answer) =>
            ValidateAnswers.validate(question, answers) shouldBe Right(answer)
          case Left(())      =>
            ValidateAnswers.validate(question, answers).left.value
        }
      }
    }

    "for text questions with validation" in {
      val question         = textQuestion(1).copy(validation = Some(TextValidation.MatchRegex("[0-9]+")))
      val optionalQuestion = question.makeOptionalPass
      type AnswerMatching = Either[Unit, ActualAnswer]
      val aFailure: AnswerMatching         = Left(())
      val validAnswer: AnswerMatching      = Right(ActualAnswer.TextAnswer("123"))
      val validEmptyAnswer: AnswerMatching = Right(ActualAnswer.NoAnswer)

      val passes = Table(
        ("description", "question", Question.answerKey, "expects"),
        ("valid answer", question, answerOf("123"), validAnswer),
        ("invalid answer", question, answerOf("abc"), aFailure),
        ("too many answers", question, answerOf("123", "456"), aFailure),
        ("valid answer on optional", optionalQuestion, answerOf("123"), validAnswer),
        ("empty answer is invalid on non optional", question, noAnswer, aFailure),
        ("empty answer is valid on optional", optionalQuestion, noAnswer, validEmptyAnswer)
      )

      forAll(passes) { (_: String, question: Question, answers: Map[String, Seq[String]], expects: AnswerMatching) =>
        expects match {
          case Right(answer) =>
            ValidateAnswers.validate(question, answers) shouldBe Right(answer)
          case Left(())      =>
            ValidateAnswers.validate(question, answers).left.value
        }
      }
    }
  }
}
