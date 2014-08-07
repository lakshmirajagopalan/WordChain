/**
 * Created by lakshmi on 6/8/14.
 */

import org.scalatest.{FunSuite, ShouldMatchers, FlatSpec}

import scala.collection.immutable.Iterable


case class Question(start:String,end:String)

class WordChainTest extends FunSuite with ShouldMatchers {

  def startWord(question:Question) ={
    question match{
      case Question(start,end) if start.startsWith("a") => end
    }
  }
//
  test("should return adjacency map for a dictionary") {
    val adj = WordChain.createAdjacency(List("cat", "cot", "bot"))

    adj("cat") should be(List("cot"))
    adj("cot") should be(List("cat", "bot"))
    adj("bot") should be(List("cot"))

  }
//
  test("compute charDiff correctly") {
    WordChain.isDifferingByOne("cat", "cot") should be (true)
    WordChain.isDifferingByOne("cot", "bot") should be (true)
    WordChain.isDifferingByOne("bot", "cat") should be(false)
    WordChain.isDifferingByOne("bot", "dry") should be(false)
  }
//
  test("constrauct Null word chain") {
    val defaultDict = List("cat", "cot", "bot")
    val wordChain = WordChain(defaultDict, Map("cat" -> "bot"))
    wordChain.getWordChain("cat", "cat", Nil) should be(Some(List("cat")))

  }
//

  test("should construct one word chain ") {
    val defaultDict = List("cat", "cot", "bot")
    val wordChain = WordChain(defaultDict, Map("cat" -> "bot"))
    wordChain.getWordChain("cat", "cot", Nil) should be(Some(List("cot")))

  }
//
  test("should construct multiple word chain ") {
    val defaultDict = List("cat", "cot", "bot")
    val wordChain = WordChain(defaultDict, Map("cat" -> "bot"))
    wordChain.getWordChain("cat", "bot", Nil) should be(Some(List("cot", "bot")))

  }

  test("should return Null for word not found in the Dict") {
    val defaultDict = List("cat", "cot", "bot")
    val wordChain = WordChain(defaultDict, Map("cat" -> "bat"))
    wordChain.getWordChain("cat", "bot", Nil) should be(Some(List("cot", "bot")))

  }


}
