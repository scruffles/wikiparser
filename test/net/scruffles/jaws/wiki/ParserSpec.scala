package net.scruffles.jaws.wiki

import org.scalatest._
import org.scalatest.matchers._
import scala.collection.mutable.Map

class ParserSpec extends Spec with ShouldMatchers {
    def testEverything(): Unit = {
        (new ParserSpec()).execute()
    }

    describe("A WikiParser") {
        it("should honor plain text") {
            new WikiParser("This text\t has no markup 123")
                    .parse() should equal("This text\t has no markup 123")
        }
        it("should change line breaks into paragraph breaks") {
            new WikiParser("Paragraph A\nParagraph B")
                    .parse() should equal("Paragraph A</p>Paragraph B")
        }
        it("should bold any text surrounded by '' ") {
            new WikiParser("one ''two'' three")
                    .parse() should equal("one <b>two</b> three")
        }
        it("should italicize any text surrounded by // ") {
            new WikiParser("one //two// three")
                    .parse() should equal("one <i>two</i> three")
        }
        it("should underline any text surrounded by __ ") {
            new WikiParser("one __two__ three")
                    .parse() should equal("one <u>two</u> three")
        }
        it("should superscript any text surrounded by ^^ ") {
            new WikiParser("one^^2^^ three")
                    .parse() should equal("one<sup>2</sup> three")
        }
        it("should subscript any text surrounded by ~~ ") {
            new WikiParser("one~~2~~ three")
                    .parse() should equal("one<sub>2</sub> three")
        }
        it("should ignore markup between {{ and }} ") {
            new WikiParser("one {{two ''three'' four}} five")
                    .parse() should equal("one two ''three'' four five")
        }
        it("should create a heading level 1 with any line begining with ! ") {
            new WikiParser("!Line 1\nLine2")
                    .parse() should equal("<h1>Line 1</h1></p>Line2")
        }
        it("should create a heading level 2 with any line begining with !! ") {
            new WikiParser("!!Line 1\nLine2")
                    .parse() should equal("<h2>Line 1</h2></p>Line2")
        }
        it("should create a heading level 3 with any line begining with !!! ") {
            new WikiParser("!!!Line 1\nLine2")
                    .parse() should equal("<h3>Line 1</h3></p>Line2")
        }
        it("should create a heading level 4 with any line begining with !!!! ") {
            new WikiParser("!!!!Line 1\nLine2")
                    .parse() should equal("<h4>Line 1</h4></p>Line2")
        }
        it("should create a heading level 5 with any line begining with !!!!! ") {
            new WikiParser("!!!!!Line 1\nLine2")
                    .parse() should equal("<h5>Line 1</h5></p>Line2")
        }
        it("should create a heading level 6 with any line begining with !!!!!! ") {
            new WikiParser("!!!!!!Line 1\nLine2")
                    .parse() should equal("<h6>Line 1</h6></p>Line2")
        }
//        it("should create a bulleted list when lines begin with '* ' ") {
//            new WikiParser("line1\n* line2\n* line3\nline4")
//                    .parse() should equal("Line1<ul><li>Line2</li><li>Line3</li></ul>Line1")
//        }
    }
}