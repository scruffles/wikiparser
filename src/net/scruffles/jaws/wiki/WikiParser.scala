package net.scruffles.jaws.wiki

import scala.util.parsing.combinator._
import java.util.regex._
import scala.collection.mutable.Map

class WikiParser(val text: String) {

    def replaceBold = replacePair(_ :String, """''""", "<b>", "</b>")
    def replaceItalic = replacePair(_ :String, """//""", "<i>", "</i>")
    def replaceUnderline = replacePair(_ :String, """__""", "<u>", "</u>")
    def replaceStrike = replacePair(_ :String, """--""", "<s>", "</s>")
    def replaceSuperscript = replacePair(_ :String, """\^\^""", "<sup>", "</sup>")
    def replaceSubscript = replacePair(_ :String, """~~""", "<sub>", "</sub>")
    def replaceH1 = replaceHeader(_:String, "!", "h1")
    def replaceH2 = replaceHeader(_:String, "!!", "h2")
    def replaceH3 = replaceHeader(_:String, "!!!", "h3")
    def replaceH4 = replaceHeader(_:String, "!!!!", "h4")
    def replaceH5 = replaceHeader(_:String, "!!!!!", "h5")
    def replaceH6 = replaceHeader(_:String, "!!!!!!", "h6")


    def removeEscapedText(input: String): (String, Map[String, String]) = {
        var text = input
        val map = Map[String, String]()
        val matcher = Pattern.compile("""\{\{.*?\}\}""").matcher(text)
        while (matcher.find) {
            val id = generateRandomId
            val escapedText = matcher.group.substring(2, matcher.group.length - 2)
            text = text.replaceFirst("\\{\\{" + escapedText + "\\}\\}", "-" + id + "-")
            map.put(id, escapedText)
        }
        return (text, map)
    }

    def replaceEscapedText(input: String, escapedTextByKey: Map[String, String]): String = {
        var text = input
        for (id <- escapedTextByKey.keys) {
            text = text.replaceAll("-" + id + "-", escapedTextByKey(id))
        }
        return text
    }

    def parse(): String = {
        var results = (for{row <- text.split("\n")} yield parseLine(row)).mkString("</p>")
        return results
        return parseLists(results)
    }

    def parseLists(input:String): String = {       
        var currentListState = Array.apply[String]()
        (
            for {row <- input.split("</p>")} yield {
                val result = parseRowForListItems(row, currentListState)
                currentListState = result._2
                return result._1
            }
        ).mkString("</p>")
    }

    def parseRowForListItems(input: String, currentListState: Array[String]): (String, Array[String]) = {
        return (input, currentListState)
        // if current line is a list item
        //     if it matches the last in the stack
        //        do nothing
        //     else if it matches any prev item in the stack
        //        close each last ordered/unordered list tag until you reach the same level
        //     else
        //        create an ordered/unordered list tag (what if it's multiple levels deep, but there are no items in the other level?)
        // else
        //     close each last ordered/unordered list tag until you clear the stack
    }

    def parseLine(input: String): String = {
        val resultOfEscapedTextRemoval = removeEscapedText(input)
        val replaceEscapedTextWithMap = replaceEscapedText(_: String, resultOfEscapedTextRemoval._2)

        chainStringManipulators(
            resultOfEscapedTextRemoval._1,
            replaceBold ::
            replaceItalic ::
            replaceSuperscript ::
            replaceUnderline ::
            replaceStrike ::
            replaceSubscript ::
            replaceH6 ::
            replaceH5 ::         
            replaceH4 ::
            replaceH3 ::
            replaceH2 ::
            replaceH1 ::
            replaceEscapedTextWithMap ::
            Nil
            )
    }

    //                                                  
    // Plumming:
    //

    // todo not random enough.. could cycle through two in the same milli (according to a lame clock)
    def generateRandomId(): String = {
        System.currentTimeMillis.toString
    }


    def chainStringManipulators(text: String, functions: List[(String) => String]): String = {
        var result = text
        functions.foreach(function => {
            result = function(result)
        })
        return result
    }

    def replaceHeader(input: String, wikiTag: String, htmlTag: String): String = {
        if (input.startsWith(wikiTag))
            "<" + htmlTag + ">" + input.substring(wikiTag.length) + "</" + htmlTag + ">"
        else input
    }

    def replacePair(inputString: String, stringToReplace: String, afterPrefix: String, afterPostfix: String): String = {
        val replacement = inputString.replaceFirst(stringToReplace, afterPrefix)
        if (inputString != replacement) {
            var result = replacement.replaceFirst(stringToReplace, afterPostfix)
            return replacePair(result, stringToReplace, afterPrefix, afterPostfix)
        } else {
            return inputString
        }
    }
}