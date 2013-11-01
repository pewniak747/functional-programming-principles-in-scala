package forcomp

import common._

object Anagrams {

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
  val dictionary: List[Word] = loadDictionary

  def modifyOccurrences(transform: ((Char, Int), (Char, Int)) => (Char, Int), zero: ((Char, Int)) => Occurrences)(o: Occurrences, p: (Char, Int)): Occurrences = o match {
    case (char, num) :: tail if char == p._1 => {
      val transformed = transform(p, (char, num))
      transformed match {
        case (char, num) if num <= 0 => tail
        case pair => pair :: tail
      }
    }
    case (char, num) :: tail if char > p._1 => p :: o
    case (char, num) :: tail => (char, num) :: modifyOccurrences(transform, zero)(tail, p)
    case Nil => zero(p)
  }

  def insertToOccurrences = modifyOccurrences((p1, p2) => (p1._1, p1._2 + p2._2), List(_))_

  def removeFromOccurrences = modifyOccurrences((p1, p2) => (p1._1, p2._2 - p1._2), (p) => Nil)_

  /** Converts the word into its character occurence list.
   *  
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   */
  def wordOccurrences(w: Word): Occurrences = w.groupBy(_.toLower).mapValues(_.size).toList.sortBy(_._1)

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.mkString)

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
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
    val wordsWithOccurences = for (word <- dictionary) yield (word, wordOccurrences(word))
    wordsWithOccurences.foldLeft(Map[Occurrences, List[Word]]())((map, wordWithOccurrence) => wordWithOccurrence match {
      case (word, occurrences) if map.contains(occurrences) => map.updated(occurrences, word :: map(occurrences))
      case (word, occurrences) => map.updated(occurrences, List(word))
    })
  }

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = {
    val occurrences: Occurrences = wordOccurrences(word)
    if (dictionaryByOccurrences contains occurrences)
      dictionaryByOccurrences(occurrences) 
    else
      List[Word]()
  }

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
  def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case head :: tail => {
      val c = combinations(tail) ::: combinations(tail).map(head :: _)
      head match {
        case (char, num) if num > 1 => c ::: combinations((char, num - 1) :: tail)
        case _ => c
      }
    }
    case nil => List(nil)
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   * 
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences =
    y.foldLeft(x)((mem, pair) => removeFromOccurrences(mem, pair))

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
    def occurrenceAnagrams(occurrences: Occurrences): List[Sentence] = occurrences match {
      case Nil => List(Nil)
      case _ => {
        val occurrenceCombinations: List[Occurrences] = combinations(occurrences)
        occurrenceCombinations.flatMap((combination) => {
          if (dictionaryByOccurrences.contains(combination)) {
            val recursiveSentences = occurrenceAnagrams(subtract(occurrences, combination))
            val foundWords = dictionaryByOccurrences(combination)
            for (sentence <- recursiveSentences; word <- foundWords if sentenceOccurrences(word :: sentence) == occurrences) yield {
              word :: sentence
            }
          }
          else Nil
        })
      }
    }
    
    val occurrences = sentenceOccurrences(sentence)
    occurrenceAnagrams(occurrences)
  }
}
