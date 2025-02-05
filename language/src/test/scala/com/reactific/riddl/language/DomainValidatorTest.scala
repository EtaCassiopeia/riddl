/*
 * Copyright 2019 Ossum, Inc.
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package com.reactific.riddl.language

import com.reactific.riddl.language.AST.*
import com.reactific.riddl.language.Messages.*
import com.reactific.riddl.language.parsing.RiddlParserInput

/** Unit Tests For ValidatorTest */
class DomainValidatorTest extends ValidatingTest {

  "DomainValidator" should {
    "identify duplicate domain definitions" in {
      val rpi = RiddlParserInput.empty
      val result = Validation.validate(
        RootContainer(
          Seq(
            Domain((1, 1, rpi), Identifier((1, 7, rpi), "foo")),
            Domain((2, 2, rpi), Identifier((2, 8, rpi), "foo"))
          ),
          Seq(rpi)
        ),
        CommonOptions()
      )
      val theErrors: Messages = result.messages
      theErrors must not be empty
      val messages = theErrors.map(_.format)
      val notOccur =
        "Style: empty(2:2): Domain 'foo' overloads Domain 'foo' at empty(1:1)"
      assert(messages.exists(_.startsWith(notOccur)))
    }

    "allow author information" in {
      val input = """domain foo by author Reid is {
                    |  author Reid is {
                    |    name: "Reid Spencer"
                    |    email: "reid@reactific.com"
                    |    organization: "Reactific Software Inc."
                    |    title: "President"
                    |  } described as "identifying"
                    |} described as "example"
                    |""".stripMargin
      parseAndValidateDomain(input) {
        (domain: Domain, rpi: RiddlParserInput, messages: Messages) =>
          domain mustNot be(empty)
          domain.contents mustNot be(empty)
          val expectedAuthor = Author(
            (2, 3, rpi),
            Identifier((2, 10, rpi), "Reid"),
            LiteralString((3, 11, rpi), "Reid Spencer"),
            LiteralString((4, 12, rpi), "reid@reactific.com"),
            Some(LiteralString((5, 19, rpi), "Reactific Software Inc.")),
            Some(LiteralString((6, 12, rpi), "President")),
            None,
            None,
            Some(BlockDescription(
              (7, 18, rpi),
              Seq(LiteralString((7, 18, rpi), "identifying"))
            ))
          )
          domain.authorDefs mustNot be(empty)
          domain.authorDefs.head must be(expectedAuthor)
          val expectedAuthorRef = AuthorRef((1,12,rpi),
            PathIdentifier((1,22,rpi), Seq("Reid")))
          domain.authors mustNot be(empty)
          domain.authors.head must be(expectedAuthorRef)
          messages mustBe empty
      }
    }
  }
}
