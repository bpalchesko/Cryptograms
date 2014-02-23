package cryptograms

import org.scalatest._
import scala.util.Random
import java.io.File
import scala.io.Source, scala.io.Source._

class WordSearchTest extends FunSuite {

  test("Removes punctuation from sentence") {
    val sentence = WordSearch.removePunctuation("ABC DEF'G HIJK... LMN; OP - QRS?")
    assert(sentence == "ABC DEF'G HIJK LMN OP  QRS")
  }

  test("Removes extra spaces from sentence") {
    val sentence = WordSearch.removeExtraSpaces("THIS   SENTENCE HAS EXTRA   SPACES")
    println(sentence)
    assert(sentence == "THIS SENTENCE HAS EXTRA SPACES")
  }

  test("Creates a list of words from the cryptogram") {
    val firstSentence = WordSearch.makeSentenceList("This is a six word sentence.")
    val secondSentence = WordSearch.makeSentenceList("This sentence is five words.")
    val thirdSentence = WordSearch.makeSentenceList("This - isn't a   sentence... is it??")
    for (i <- 0 to thirdSentence.length - 1) { println(thirdSentence(i)) }
    println(thirdSentence(2))
    assert(firstSentence.length == 6 && firstSentence(0) == "THIS" &&
      firstSentence(5) == "SENTENCE")
    assert(secondSentence.length == 5 && secondSentence(1) == "SENTENCE")
    assert(thirdSentence(0) == "THIS" && thirdSentence(1) == "ISN'T" &&
      thirdSentence(2) == "A" && thirdSentence(3) == "SENTENCE" &&
      thirdSentence(4) == "IS" && thirdSentence(5) == "IT")
  }

  test("Decoder Array returns indices correctly") {
    var decoderArray = new Array[Char](26)
    var result = WordSearch.updateDecoderArray("are", "ert", decoderArray)
    assert(result(0) == 'E')
    assert(result.length == 26)
    result = WordSearch.updateDecoderArray("don't", "bif'd", result)
    for (i <- 0 to 25) {
      print(result(i))
    }
    assert(result(0) == 'E')
    assert(result.length == 26)
    assert(result(3) == 'B')
  }

  test("Final code string test") {
    var tDecoderArray = new Array[Char](26)
    println(WordSearch.updateDecoderArray("antelope",
      "rbaqsvdq", tDecoderArray).toList)
    var result = WordSearch.createFinalCodeString(WordSearch.updateDecoderArray("antelope",
      "rbaqsvdq", tDecoderArray))
    println("Result:" + result)
    assert(result.length == 26)
    assert(result(0) == 'R')
    assert(result(WordSearch.alphabet.indexOf('N')) == 'B')
    assert(WordSearch.makeDuplicateLetterString(result) == 
      "012345678910111213141516171819202122232425")
  }

  test("Initialize Decoded Sentence") {
    var sentence = "the quick brown fox jumps over the lazy dog"
    var sentenceList = sentence.split(" ").toList
    var decSent = WordSearch.initializeDecodedSentence(sentenceList)
    var decSent2 = WordSearch.initializeDecodedSentence("It's an apostrophe".split(" ").toList)
    assert(decSent.length == 9)
    assert(decSent.contains("___"))
    assert(decSent(0).getClass.getSimpleName != "Char")
    assert(decSent2(0) == "__'_" && decSent2.length == 3)
  }

  test("Letter properly decoded") {
    var codedString = "qwertyuiopasdfghjklzxcvbnm"
    codedString = codedString.toUpperCase
    var codeArray = codedString.toCharArray
    assert(WordSearch.decodedLetter(codeArray, 'Q') == 'A')
    assert(WordSearch.decodedLetter(codeArray, 'W') == 'B')
    assert(WordSearch.decodedLetter(codeArray, ''') == ''')
  }

  test("update decode string") {
    var decoderArray = new Array[Char](26)
    var sentence = "the quick brown fox jumps over the lazy dog"
    var codedString = "qwertyuiopasdfghjklzxcvbnm"
    var encodedSent = WordSearch.makeSentenceList(Dave.encode(sentence, codedString))
    var decodedSentenceArray = WordSearch.initializeDecodedSentence(sentence.split(" ").toList)
    decoderArray = WordSearch.updateDecoderArray("quick", "JXOEA", decoderArray)
    var decodedString = WordSearch.updateDecodedSentence(encodedSent, decodedSentenceArray,
      decoderArray)
    for (letter <- decoderArray) { print(letter) }
    println(decodedString(1))
    assert(decodedString.length == 9)
    assert(decodedString(1) == "QUICK")
  }

  test("Update Decoded Sentence Array correctly") {
    val codedSentence = Dave.encode("this is a sentence", "qwertyuiopasdfghjklzxcvbnm")
    val codedSentenceList = WordSearch.makeSentenceList(codedSentence)
    val blankDecoderArray = new Array[Char](26)
    val decodedSentenceArray = WordSearch.initializeDecodedSentence(codedSentenceList)
    val decodedArray: Array[Char] = WordSearch.updateDecoderArray("then", "ZIOL", blankDecoderArray)
    val updatedDecodedSentence = WordSearch.updateDecodedSentence(codedSentenceList,
      decodedSentenceArray, decodedArray)
    assert(updatedDecodedSentence.mkString(" ").length == codedSentence.length)
    assert(updatedDecodedSentence(0) == "THEN")
    assert(updatedDecodedSentence(1) == "EN")
    assert(updatedDecodedSentence(3) == "N__T____")
  }

  test("Check match word for conflicts with current decoder array") {
    var decoderArray = new Array[Char](26)
    decoderArray = WordSearch.updateDecoderArray("two", "abc", decoderArray)
    assert(WordSearch.checkForNoCodeConficts("WORDS", "BCEFG", decoderArray) == true)
    assert(WordSearch.checkForNoCodeConficts("WOODS", "BCEFG", decoderArray) == false)
    assert(WordSearch.checkForNoCodeConficts("WOO'S", "BCEFG", decoderArray) == false)
    assert(WordSearch.checkForNoCodeConficts("WORDS", "BCE'G", decoderArray) == false)
    assert(WordSearch.checkForNoCodeConficts("WOR'S", "BCE'G", decoderArray) == true)
  }

  test("Check duplicate letter string") {
    assert(WordSearch.makeDuplicateLetterString("AAA") == "000")
    assert(WordSearch.makeDuplicateLetterString("ELEPHANT") == "01034567")
    assert(WordSearch.makeDuplicateLetterString("NOODLE") == "011345")
    assert(WordSearch.makeDuplicateLetterString("ISN'T") == "01234")
  }

  test("Checks if letter would be encoded to itself") {
    val cryptogramWord = "QWERT"
    val cryptogramWord2 = "QW'ERT"
    assert(WordSearch.isValidMatchSameLetter(cryptogramWord, "QUICK") == false)
    assert(WordSearch.isValidMatchSameLetter(cryptogramWord, "SWING") == false)
    assert(WordSearch.isValidMatchSameLetter(cryptogramWord, "SMART") == false)
    assert(WordSearch.isValidMatchSameLetter(cryptogramWord, "SUPER") == true)
    assert(WordSearch.isValidMatchSameLetter(cryptogramWord2, "SU'PER") == true)
  }

  test("Narrowing word list, then evaluating code conflicts") {
    var wordList = List("WOODS", "WALLS", "WORDS", "WORFS", "WOR'S", "WARDS")
    val sentence = "Abc, bcefg."
    val sentenceList = WordSearch.makeSentenceList(sentence)
    val sentenceApostrophe = "Abc, defg'a?"
    val sentenceApostropheList = WordSearch.makeSentenceList(sentenceApostrophe)
    var decoderArray = new Array[Char](26)
    decoderArray = WordSearch.updateDecoderArray("two", "abc", decoderArray)
    val narrowedList = WordSearch.createListOfPotentialMatches(sentenceList(1), wordList)
    assert(narrowedList == List("WORDS", "WARDS"))
    assert(WordSearch.checkForNoCodeConficts(narrowedList(0), sentenceList(1),
      decoderArray))
    assert(!WordSearch.checkForNoCodeConficts(narrowedList(1), sentenceList(1),
      decoderArray))
    val narrowedList2 = WordSearch.createListOfPotentialMatches(sentenceApostropheList(1),
      List("AREN'T", "ARRN'T"))
    assert(narrowedList2 == List("AREN'T"))
    assert(WordSearch.checkForNoCodeConficts(narrowedList2(0), sentenceApostropheList(1),
      decoderArray))
  }
  
  test("Test createPotentialMatchCombinations") {
    var listToCombine = List(List("ABC", "DEF"), List("GH"), List("IJKL", "MNOP"))
    var result = WordSearch.createPotentialMatchCombinations(listToCombine)
    println(listToCombine.length)
    println(result)
    assert(result.length == 4)
  }
}