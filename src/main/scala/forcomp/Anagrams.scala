package forcomp

import forcomp.Anagrams.Occurrences

import scala.io.{Codec, Source}

object Anagrams extends AnagramsInterface:

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = Dictionary.loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences = w.toLowerCase().groupBy((c: Char) => c).view.mapValues(_.length).toList.sortBy(_._1)

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.foldRight("")(_ ++ _))

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.map((word: String) => (wordOccurrences(word), word)).groupBy(_._1).view.mapValues( _.map(_._2)).toMap

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] =
    {
      def diffCharRepeats(occ: (Char, Int)) = List.fill(occ._2+1)(occ).zipWithIndex.map(el => el._1.copy(_2 = el._2))
      occurrences match {
        case head :: tail => {
          val localCombinations = for {
            tailComb <- combinations(tail)
            headComb <- diffCharRepeats(head)
          } yield headComb :: tailComb
          localCombinations.map(_.filter(_._2 > 0))
      }
      case nil => List(nil)
  }
    }




  /** Subtracts occurrence list `y` from occurrence list `x`.
   *Seq[Occurrences]
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
//    val parts = x.partition(case (x_el, _): y.contains((x_el._1, _))
//    val parts = x.partition(x_el => y.contains((x_el, )))
    val subOcc = for {
      xOcc <- x
      yOcc <- y if xOcc._1 == yOcc._1
    } yield xOcc.copy(_2 = xOcc._2 - yOcc._2)
    val common = y.flatMap(y_el => x.collect{case (y_el._1, y) => (y_el._1, y)})
    val difference = x.diff(common)
    val sol = subOcc ++ difference

//    val sumOcc = for {
//      xOcc <- x
//      sOcc <- subOcc
//    } yield
//    else xOcc
    sol.filter(_._2 > 0)
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def sumNumSentence(strList: Sentence): Int =
      strList.foldRight(0)((s: Word, sum: Int) => s.size + sum)

    def makeWord(occurrences: Occurrences): List[Word] =
      val chars = occurrences.foldLeft(String())((f: String, s: (Char, Int)) => f + s._1.toString*s._2)
      try {
        val wa = wordAnagrams(chars)
        wa
      } catch {
        case e: NoSuchElementException => List[Word]()

      }


    def recAnagram(occurrences: Occurrences, used: List[Occurrences] = List()): List[Sentence] =
      if occurrences.isEmpty then return List(List(""))
      val allCombinations: List[Occurrences] = combinations(occurrences)
//      sentences.map(_ + allCombinations.map(comb => makeWord(comb)))
      val genSentences = for {
        comb <- allCombinations if !used.contains(comb)
        word <- makeWord(comb)
        nextSentence <- recAnagram(subtract(occurrences, comb), used :+ comb)
      } yield {
        val x = comb
        val y = word
        val newSentence = word +: nextSentence
        newSentence
//        newSentence =: recAnagram(comb, newSentence)
      }
      genSentences

    val goalLength = sumNumSentence(sentence)

    val allOccurences: Occurrences = sentenceOccurrences(sentence)//.reduceRight(_ + _)
    //    val allAnagrams = wordAnagrams(allChars)
//  val allCombinations = combinations(allOccurences)
    val anagrams_without_length_rule = recAnagram(allOccurences).map(_.filter(_ != "")).distinct
//    anagrams_without_length_rule.filter(_.map(_.foldRight(0)){(s: String, sum: Int) => s.size + sum) == 0}


    println(anagrams_without_length_rule)
    anagrams_without_length_rule.foreach((x: Sentence) => {println(x); println(sumNumSentence(x))} )
    anagrams_without_length_rule.filter(
      (sentence: Sentence) => sumNumSentence(sentence) == goalLength
    )


//    def recAnagram(occurrences: Occurrences, sentences: List[Sentence]): List[Sentence] =
//      if occurrences.isEmpty then return List()
//      val allCombinations = combinations(occurrences)
////      sentences.map(_ + allCombinations.map(comb => makeWord(comb)))
//      val genSentences = for {
//        sentence <- sentences
//        comb <- allCombinations
//        word <- makeWord(comb)
//      } yield {
//        val newSentence = sentence :+ word
//        newSentence =: recAnagram(comb, newSentence)
//      }
//        genSentences.flatten
//
//
//      val genSentences: Seq[List[Sentence]] = for {
//        comb <- allCombinations
//        word <- makeWord(comb)
//      } yield sentences.map(_ + recAnagram(subtract(occurrences, comb), sentences.map(_ :+ word))
//      genSentences.flatten
//
//    val sentenceOccurences = sentenceOccurrences(sentence)//.reduceRight(_ + _)
////    val allAnagrams = wordAnagrams(allChars)
//    val allCombinations = combinations(allOccurences)
//    allCombinations()
//
  }

object Dictionary:
  def loadDictionary: List[String] =
    val wordstream = Option {
      getClass.getResourceAsStream(List("forcomp", "linuxwords.txt").mkString("/", "/", ""))
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try
      val s = Source.fromInputStream(wordstream)(Codec.UTF8)
      s.getLines().toList
    catch
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    finally
      wordstream.close()