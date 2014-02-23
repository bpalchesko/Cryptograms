package cryptograms

import org.scalatest._
import scala.util.Random

class CryptogramsEncoderTest extends FunSuite {
  
  //createShuffledList test
  test("Shuffled List is fully encoded") {
    for(test <- 1 to 1000) {
	  var testList = Dave.createShuffledList(Random.shuffle(Dave.alphabet))
	  var validEncodedLetters = 0
	  for (i <- 0 to testList.length-1) {
	    if(Dave.alphabet(i)!=testList(i)) validEncodedLetters += 1
	    else println(testList)
	  }
	  assert(validEncodedLetters == 26)
    }
  }
  
  //createShuffledListString test
  test ("Shuffled List String") {
    for(test <- 1 to 1000) {
      var testList = Dave.createShuffledList(Random.shuffle(Dave.alphabet))
      var testListString = Dave.createShuffledListString(testList)
      var stringtoList = List[Char]()
      for (i <- testListString) {
        stringtoList = stringtoList :+ i
      }
      assert(testList == stringtoList)
    }
  }
  
  //encoder testing
    test("Failing test") {
    for(test <- 1 to 1000) {
      var failingList = Random.shuffle(Dave.alphabet)
      failingList = failingList.patch(25, Nil, 1) 
	  failingList = failingList :+ 'Z'
	  var validEncodedLetters = 0
	  for (i <- 0 to failingList.length-1) {
	    if(Dave.alphabet(i)!=failingList(i)) validEncodedLetters += 1
	  }
	  assert(validEncodedLetters <= 25)
    }
  }
    
   //capital letters test/decoder test
    test("Check for different cases, numbers and punctuation/decoded properly") {
      for (test <- 1 to 100) {
        var statement = "hello--- this is ...pretty 543awesome"
        var g = Dave.encode(statement, "qwertyuiopasdfgHjklzxcvbnm")
        assert(Dave.decode(g, "qwertyuiopASDfgHjklzxcvbnm") == statement.toUpperCase)
      } 
    }
    
    test("Test searchForMatch") {
      var decoderArray = new Array[Char](26)
      val wordLists =  List(List("THIS", "THAT", "WHAT"), List("SHOUT", "THING", "SMART"), 
          List("AT", "IN", "IS"), List("COOL", "WOOL", "PEEL"))
      val cryptogram = List("ABCD", "ABCEF", "CD", "GHHI")
      assert(Dave.searchForMatch(cryptogram(0), wordLists(0), decoderArray) == Some("THIS"))
      decoderArray = WordSearch.updateDecoderArray("THIS", "ABCD", decoderArray)
      assert(Dave.searchForMatch(cryptogram(1), wordLists(1), decoderArray) == Some("THING"))
      decoderArray = WordSearch.updateDecoderArray("THING", "ABCED", decoderArray)
      assert(Dave.searchForMatch(cryptogram(2), wordLists(2), decoderArray) == Some("IS"))
      decoderArray = WordSearch.updateDecoderArray("IS", "CD", decoderArray)
      assert(Dave.searchForMatch(cryptogram(3), wordLists(3), decoderArray) == Some("COOL"))
    }
    
    test("Test updateDecoderWithMatch") {
      var decoderArray = new Array[Char](26)
      decoderArray = Dave.updateDecoderWithMatch(Some("THIS"), "ABCD", decoderArray)
      decoderArray = Dave.updateDecoderWithMatch(Some("THING"), "ABCEF", decoderArray)
      decoderArray = Dave.updateDecoderWithMatch(Some("IS"), "CD", decoderArray)
      decoderArray = Dave.updateDecoderWithMatch(Some("COOL"), "GHHI", decoderArray)
      println(decoderArray.toList)
      assert(decoderArray(19) == 'A' && decoderArray(18) == 'D' && decoderArray(2) == 'G')
    }
    
    test("Test counting English words") {
      assert(Dave.countEnglishWords(List("HERE", "IS", "A", "SENTENCE"), 
      List("HERE", "IS", "A", "BIG", "LIST", "OF", "ENGLISH", "WORDS", "SENTENCE")) == 4)
    }

}