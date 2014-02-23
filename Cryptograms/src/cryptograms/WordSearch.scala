package cryptograms

import java.io.File
import scala.io.Source, scala.io.Source._, scala.swing.FileChooser

object WordSearch {

  val alphabet = List('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
    'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z')

  /**
   * Turns user-selected text file into a list of common English words
   */
  def createTopWordList: List[String] = {
    val chooser = new FileChooser
    val response = chooser.showOpenDialog(null)
    var wordList: List[String] = List()

    if (response == FileChooser.Result.Approve) {
      for (line <- Source.fromFile(chooser.selectedFile).getLines) {
        wordList = wordList :+ line.toUpperCase.toString
      }
    }
    wordList
  }

  /**
   * Removes punctuation (besides apostrophes) from the encoded sentence
   */
  def removePunctuation(sentence: String): String = {
    var sentenceMinusPunctuation = ""
    for (i <- 0 to sentence.length - 1) {
      if (alphabet.contains(sentence(i)) || sentence(i) == ' ' ||
        sentence(i) == ''') sentenceMinusPunctuation += sentence(i)
    }
    sentenceMinusPunctuation
  }

  /**
   * Removes consecutive spaces. Note: this was necessary because trim
   * whitespace removes apostrophes (in Eclipse at least)
   */
  def removeExtraSpaces(sentence: String): String = {
    var sentenceMinusExtraSpaces = ""
    for (i <- 0 to sentence.length - 1) {
      var lastIndex = i - 1
      if (lastIndex < 0) lastIndex = 0
      if (alphabet.contains(sentence(i)) || (sentence(i) == ' ' &&
        sentence(lastIndex) != ' ') || sentence(i) == ''') {
        sentenceMinusExtraSpaces += sentence(i)
      }
    }
    sentenceMinusExtraSpaces
  }

  /**
   * Turns the formatted, encoded sentence into a list of encoded words
   */
  def makeSentenceList(sentence: String): List[String] = {
    var sentenceToSplit = removeExtraSpaces(removePunctuation(sentence.toUpperCase.trim))
    var sentenceList = sentenceToSplit.split(' ').toList
    sentenceList
  }

  /**
   * Finds apostrophes in words, so that an encrypted word with apostrophes can
   * be matches with an English word with an apostrophe in the same place
   */
  def findApostrophe(word: String): Int = {
    var apostropheIndex = 0
    for (i <- word) {
      if (i == ''') {
        apostropheIndex = i
      }
    }
    apostropheIndex
  }

  /**
   * True if:
   * a) there are no apostrophes present in the encoded word and the potential match
   * b) if there are apostrophes, they are in the same place
   */
  def apostrophePositionMatches(cryptogramWord: String, testWord: String): Boolean = {
    var apostropheMatches = true
    if (testWord.contains("'")) {
      //find index of '
      apostropheMatches = false
      apostropheMatches = findApostrophe(cryptogramWord) == findApostrophe(testWord)
    }
    apostropheMatches
  }

  /**
   * Compares the cryptogram word with the potential match to check that the match wouldn't
   * require a letter to be encoded to itself
   */
  def isValidMatchSameLetter(cryptogramWord: String, matchWord: String): Boolean = {
    var invalidMatch = 0
    for (i <- 0 to cryptogramWord.length - 1) {
      if (cryptogramWord(i) == matchWord(i) && alphabet.contains(matchWord(i))) invalidMatch = 1
    }
    invalidMatch == 0
  }

  /**
   * Returns a string of numbers showing the locations of repeated letters
   * Ex: "SALSA" would return "01201"
   * These strings are later compared to check the validity of a potential match
   */
  def makeDuplicateLetterString(word: String): String = {
    var duplicateLetterString = ""
    for (i <- 0 to word.length - 1) {
      if (word.indexOf(word(i)) == i) duplicateLetterString += i
      else duplicateLetterString += word.indexOf(word(i))
    }
    duplicateLetterString
  }

  /**
   * Checks multiple conditions to return whether an English word is a potential
   * match with the cryptogram word: they are same length, have same apostrophe
   * locations (if applicable), a letter wouldn't be encoded to itself, and
   * patterns of repeated letters match
   */
  def isWordPotentialMatch(cryptogramWord: String, testWord: String): Boolean = {
    cryptogramWord.length == testWord.length &&
      apostrophePositionMatches(cryptogramWord, testWord) &&
      isValidMatchSameLetter(cryptogramWord, testWord) &&
      makeDuplicateLetterString(cryptogramWord) == makeDuplicateLetterString(testWord)
  }

  /**
   * Creates a list of all English words from the word list that fit the conditions of
   * a potential match for a specific cryptogram word
   */
  def createListOfPotentialMatches(cryptogramWord: String, wordList: List[String]): List[String] = {
    var listOfPotentialMatches: List[String] = List()
    for (word <- wordList) {
      if (isWordPotentialMatch(cryptogramWord, word)) {
        listOfPotentialMatches = listOfPotentialMatches :+ word
      }
    }
    listOfPotentialMatches
  }

  /**
   * Creates a list containing the lists of potential matches of each word in the cryptogram
   */
  def createListOfPotentialMatchLists(sentenceList: List[String],
    wordList: List[String]): List[List[String]] = {
    var listOfWordLists: List[List[String]] = List()
    for (i <- 0 to sentenceList.length - 1) {
      listOfWordLists = listOfWordLists :+ createListOfPotentialMatches(sentenceList(i),
        wordList)
    }
    listOfWordLists
  }
  
  def createPotentialMatchCombinations(listsToCombine: List[List[String]]): List[List[String]] = {
    var lists = listsToCombine
    var combinationLists: List[List[String]] = List()
    if (listsToCombine.length < 20) {
      for(i <- 1 to 20-lists.length) lists = lists :+ List("")
    }
      /*for(a <- lists(0); b <- lists(1); c <- lists(2); d <- lists(3)) {
        combinationLists = combinationLists :+ List(a, b, c, d)
      }*/
    (for(a <- lists(0); b <- lists(1); c <- lists(2); d <- lists(3); e <- lists(4);
    f <- lists(5);g <- lists(6);h <- lists(7);i <- lists(8);j <- lists(9);
    k <- lists(10);l <- lists(11);m <- lists(12);n <- lists(13);o <- lists(14);
    p <- lists(15);q <- lists(16);r <- lists(17);s <- lists(18);t <- lists(19); if 
    combinationLists.length < 10)  yield List(a, b, c, d, e, f, g, h, i, j, k,
      l, m, n, o, p, q, r, s, t)).toList
    
    /* for(a <- lists(0); b <- lists(1); c <- lists(2); d <- lists(3); e <- lists(4);
    f <- lists(5);g <- lists(6);h <- lists(7);i <- lists(8);j <- lists(9);
    k <- lists(10);l <- lists(11);m <- lists(12);n <- lists(13);o <- lists(14);
    p <- lists(15);q <- lists(16);r <- lists(17);s <- lists(18);t <- lists(19); if 
    combinationLists.length < 1500) {
      combinationLists = combinationLists :+ List(a, b, c, d, e, f, g, h, i, j, k,
      l, m, n, o, p, q, r, s, t)
    }*/
    //combinationLists
  }

  /**
   * Takes matched word from list & encoded word and outputs updated decoder array
   */
  def updateDecoderArray(matchWord: String, testWord: String,
    decoderArray: Array[Char]): Array[Char] = {
    var testWordCaps = testWord.toUpperCase
    var matchWordCaps = matchWord.toUpperCase

    for (i <- 0 to testWordCaps.length - 1) {
      if (alphabet.contains(testWordCaps(i))) { //ignore apostrophes
        decoderArray(alphabet.indexOf(matchWordCaps(i))) = testWordCaps(i)
      }
    }
    decoderArray
  }

   /**
   * Turns the decoder array into a string of letters
   */
  def createFinalCodeString(decoderArray: Array[Char]): String = {
    var remainingLetters = alphabet.filterNot((ch: Char) => decoderArray.contains(ch))
    remainingLetters = remainingLetters.reverse
    var finalCodeString = ""
    for (i <- 0 to 25) {
      if (alphabet.contains(decoderArray(i))) {
      finalCodeString = finalCodeString + decoderArray(i)
      }
      else {
        remainingLetters.find((letter: Char) => letter != alphabet(i)  && 
        !finalCodeString.contains(letter)) match {
          case Some(letter) => finalCodeString = finalCodeString + letter
          case None =>
        }
      }
    }
    finalCodeString.toUpperCase
  }

  /**
   * Creates an array where words will be decoded according to the decoder array
   */
  def initializeDecodedSentence(cryptogramWordList: List[String]): List[String] = {
    var codedSentenceCaps = cryptogramWordList.mkString(" ").toUpperCase
    var decodedSentence = ""
    for (i <- codedSentenceCaps) {
      if (alphabet.contains(i)) {
        decodedSentence = decodedSentence + "_"
      } else {
        decodedSentence = decodedSentence + i
      }
    }
    decodedSentence.split(" ").toList
  }

  /**
   * Given a coded character, finds the corresponding decoded letter
   */
  def decodedLetter(decoderArray: Array[Char], char: Char): Char = {
    var charAny: Any = char
    var decodedLetter = charAny match {
      case x: Int => x.toChar
      case ''' => '''
      //if doesn't find it then it will return a -1
      case x: Char => alphabetCheck(x, decoderArray)
      case _ => '_'
    }
    decodedLetter
  }

  /**
   * Checks to see if decoder array contains char
   */
  def alphabetCheck(char: Char, decoderArray: Array[Char]): Char = {
    var result = '_'
    if (decoderArray.contains(char)) {
      result = alphabet(decoderArray.indexOf(char))
    }
    result
  }

  /**
   * Updates the array of words according to changes in the decoder array
   */
  def updateDecodedSentence(codedSentenceList: List[String],
    decodedSentenceArray: List[String], decoderArray: Array[Char]): List[String] = {
    var decodedSentenceArrayResult = decodedSentenceArray.toArray
    //var codedSentenceArray = codedSentence.split(" ")
    var letterToReplace = ' '
    //iterating through each term in array
    for (term <- 0 to decodedSentenceArray.length - 1)
      //iterating through each letter of each term
      for (letter <- 0 to decodedSentenceArray(term).length - 1) {
        //finding letter to replace
        letterToReplace = decodedLetter(decoderArray, codedSentenceList(term)(letter))
        decodedSentenceArrayResult(term) = decodedSentenceArrayResult(term).updated(letter, letterToReplace)
      }
    decodedSentenceArrayResult.toList
  }

  /**
   * Checks that a potential match for a cryptogram word from the English word list has no
   * conflicts with the latest code array
   */
  def checkForNoCodeConficts(matchWord: String, cryptogramWord: String,
    decoderArray: Array[Char]): Boolean = {
    var validLetters = 0
    for (i <- 0 to matchWord.length - 1) {
      if (alphabet.contains(matchWord(i)) && alphabet.contains(cryptogramWord(i))) {
        var testLetterIndex = alphabet.indexOf(matchWord(i))
        var letterAlreadyDecoded = alphabet.contains(decoderArray(testLetterIndex))
        if (!letterAlreadyDecoded && !decoderArray.contains(cryptogramWord(i))) validLetters += 1
        else if (letterAlreadyDecoded && decoderArray(testLetterIndex)
          == cryptogramWord(i)) validLetters += 1
      } else if (matchWord(i) == ''' && cryptogramWord(i) == ''') validLetters += 1
    }
    matchWord.length == validLetters
  }
  
  
}