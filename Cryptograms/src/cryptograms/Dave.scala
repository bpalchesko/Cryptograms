package cryptograms

import scala.util.Random

object Dave {

  val alphabet = List('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 
    'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z')
  var shuffledAlphabet = Random.shuffle(alphabet)

  /**
   * Helper method to create a shuffled code list that prevents letters from 
   * being encoded to themselves
   */
  def createShuffledList(listOfLetters: List[Char]): List[Char] = {
    var shuffledList = listOfLetters
    while (shuffledList(shuffledList.length - 1) == 'Z') {
      shuffledList = Random.shuffle(shuffledList)
    }
    for (i <- 0 to alphabet.length - 1) {
      if (alphabet(i) == shuffledList(i)) {
        var swapLetter = shuffledList(i)
        shuffledList = shuffledList.patch(i, Nil, 1)
        shuffledList = shuffledList :+ swapLetter
      }
    }
    shuffledList
  }
  
  /**
   * Creates string from a shuffled code list, which can used as a
   * code for the encode method
   */
  def createShuffledListString(shuffledList: List[Char]): String = {
    var code = ""
    for (i <- 0 to shuffledList.length - 1) {
      code += shuffledList(i)
    }
    code
  }

  /**
   * Encode sentence using a code string
   */
  def encode(plainText: String, code: String): String = {
    var sentence = plainText.toUpperCase
    var scrambledSentence = ""
    for (i <- 0 to sentence.length - 1) {
      if (alphabet.contains(sentence(i))) {
        var letterToReplace = alphabet.indexOf(sentence(i))
        var letterToAdd = code(letterToReplace)
        scrambledSentence += letterToAdd
      } else scrambledSentence += sentence(i)
    }
    scrambledSentence.toUpperCase
  }

  /**
   * Decodes a coded sentence according to the code
   */
  def decode(encodedText: String, code: String): String = {
    var decodedString = ""
    var upperCaseCode = code.toUpperCase
    for (i <- 0 to encodedText.length - 1) {
      if (upperCaseCode.contains(encodedText(i))) {
        var letterIndexinCode = upperCaseCode.indexOf(encodedText(i))
        var letterInAlphabet = alphabet(letterIndexinCode)
        decodedString = decodedString + letterInAlphabet
      } else decodedString = decodedString + encodedText(i)
    }
    decodedString
  }

  /**
   * Given a cryptogram, returns the code that was used to encode it
   */
 def discoverObject(message: String): String = {
    var cryptogramWordList = WordSearch.makeSentenceList(message)
    cryptogramWordList = cryptogramWordList.sortWith((x: String, y: String) => x.length <= y.length)
    val englishWordList = WordSearch.createTopWordList
    var listOfWordPossibilities = WordSearch.createListOfPotentialMatchLists(cryptogramWordList, englishWordList)
    var wordCombinationsToTest = WordSearch.createPotentialMatchCombinations(listOfWordPossibilities)
    //var cryptogramSolved = false
    var decoderArray = new Array[Char](26)
    var bestDecoderArray = new Array[Char](26)
    var decodedSentence = WordSearch.initializeDecodedSentence(cryptogramWordList)
    var bestDecodedSentence = WordSearch.updateDecodedSentence(cryptogramWordList, decodedSentence, bestDecoderArray)
    var decodedSentenceList = decodedSentence.toList
    var totalAttempts = 0

    for (attempt <- wordCombinationsToTest) {
      decoderArray = new Array[Char](26)
      decodedSentence = WordSearch.initializeDecodedSentence(cryptogramWordList)
      for (i <- 0 until cryptogramWordList.length) {
        var cryptogramWord = cryptogramWordList(i)
        if (WordSearch.checkForNoCodeConficts(attempt(i), cryptogramWord, decoderArray)) {
          decoderArray = WordSearch.updateDecoderArray(attempt(i), cryptogramWord, decoderArray) 
        }
      }
      totalAttempts += 1
      
      decodedSentenceList = WordSearch.updateDecodedSentence(cryptogramWordList, decodedSentence, decoderArray)
      bestDecodedSentence = WordSearch.updateDecodedSentence(cryptogramWordList, decodedSentence, bestDecoderArray)
      if (countEnglishWords(decodedSentenceList, englishWordList) > countEnglishWords(bestDecodedSentence, englishWordList)) {
        bestDecoderArray = decoderArray
      }

    }
    println(bestDecoderArray.mkString(""))
    println(s"Attempts: $totalAttempts")
    WordSearch.createFinalCodeString(bestDecoderArray)
    
    /*for (i <- 1 until cryptogramWordList.length)
    for (word <- listOfWordPossibilities(0); 
         if cryptogramSolved == false && attempt <30 ) {
      decoderArray = new Array[Char](26)
      decoderArray = WordSearch.updateDecoderArray(word, cryptogramWordList(0), decoderArray)
      println(word + " New array " + decoderArray.toList)
      decodedSentence = WordSearch.initializeDecodedSentence(cryptogramWordList)
      for (i <- 1 until cryptogramWordList.length if cryptogramSolved == false) {
        var cryptogramWord = cryptogramWordList(i)
        var searchAttempt = searchForMatch(cryptogramWord, listOfWordPossibilities(i), decoderArray)
        println(searchAttempt)
        decoderArray = updateDecoderWithMatch(searchAttempt, cryptogramWord, decoderArray)  
        decodedSentenceList = WordSearch.updateDecodedSentence(cryptogramWordList, decodedSentence, decoderArray)
      }
      println(decoderArray.toList)
      println(decodedSentence)
      println("English words: " + countEnglishWords(decodedSentenceList, englishWordList))
      attempt += 1
      bestDecodedSentence = WordSearch.updateDecodedSentence(cryptogramWordList, decodedSentence, bestDecoderArray)
      if (countEnglishWords(decodedSentenceList, englishWordList) > countEnglishWords(bestDecodedSentence, englishWordList)) {
        bestDecoderArray = decoderArray
      }
    }
    println(bestDecoderArray.mkString(""))
    println(s"Attempts: $attempt")
    WordSearch.createFinalCodeString(bestDecoderArray)*/
  }

  def searchForMatch(cryptogramWord: String, wordList: List[String], 
                    decoderArray: Array[Char]): Option[String] = {
    var matchFound: Option[String] = None
    for(word <- wordList if matchFound == None) {
      if (WordSearch.checkForNoCodeConficts(word, cryptogramWord, decoderArray)) matchFound = Some(word)
    }
    matchFound
  }
  
  def updateDecoderWithMatch(matchFound: Option[String], cryptogramWord: String, 
                             decoderArray: Array[Char]): Array[Char] = {
    var resultingDecoderArray = decoderArray
    matchFound match {
      case Some(word) => resultingDecoderArray = WordSearch.updateDecoderArray(word, cryptogramWord, decoderArray)
      case None =>
    }
    resultingDecoderArray  
  }
  
 def underscoreCheck(decodedSentence: Array[String]) = {
    var decodedLetterExists = false
    for (i <- 0 to decodedSentence.length - 1) {
      for (j <- decodedSentence(i)) {
        if (j == '_') {
          decodedLetterExists = true
        }
      }
    }
    decodedLetterExists
  }
  
  def calculateNumberOfDecodedLetters (decoderArray: Array[Char]): Int = {
    var lettersDecoded = 0 
    for (i <- decoderArray) {
      if(alphabet.contains(i)) lettersDecoded +=1
    }
    lettersDecoded
  }

  def countEnglishWords(decodedSentence: List[String], englishWordList: List[String]): Int = {
    var numberOfWords = 0
    for (word <- decodedSentence) {
      if (englishWordList.contains(word)) numberOfWords +=1
    }
    numberOfWords
  }

} 
      