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

package uk.gov.hmrc.apiplatform.modules.organisations.submissions.utils

import uk.gov.hmrc.apiplatform.modules.common.utils.HmrcSpec

import uk.gov.hmrc.apiplatform.modules.organisations.submissions.domain.models._

class AnsweringQuestionsHelperSpec extends HmrcSpec with QuestionBuilder {

  "answerForQuestion" should {
    "provide a conditional text answer for Pass" in {
      val question = conditionalTextQuestion(1)

      AnsweringQuestionsHelper.answerForQuestion(Mark.Pass)(question) shouldBe Map(
        question.id -> Some(ActualAnswer.ConditionalTextAnswer(AnsweringQuestionsHelper.conditionalText))
      )
    }

    "provide no conditional text answer for Warn" in {
      val question = conditionalTextQuestion(1)

      AnsweringQuestionsHelper.answerForQuestion(Mark.Warn)(question) shouldBe Map(question.id -> None)
    }

    "provide no conditional text answer for Fail" in {
      val question = conditionalTextQuestion(1)

      AnsweringQuestionsHelper.answerForQuestion(Mark.Fail)(question) shouldBe Map(question.id -> None)
    }
  }
}
