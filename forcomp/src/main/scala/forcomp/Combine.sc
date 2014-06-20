package forcomp

object Combine {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  type Occurrences = List[(Char, Int)]

  def generateSimples(occ: (Char, Int)): List[Occurrences] = {
    (for {
      i <- 1 to occ._2
    } yield List((occ._1, i))).toList ++ List(Nil)
  }                                               //> generateSimples: (occ: (Char, Int))List[forcomp.Combine.Occurrences]

  val listA :List[Occurrences] = generateSimples(('a', 2))
                                                  //> listA  : List[forcomp.Combine.Occurrences] = List(List((a,1)), List((a,2)), 
                                                  //| List())
  val listB = generateSimples(('b', 2))           //> listB  : List[forcomp.Combine.Occurrences] = List(List((b,1)), List((b,2)), 
                                                  //| List())

	(for {
		x <- listA
		y <- listB
	} yield (x ++ y).sorted)                  //> res0: List[List[(Char, Int)]] = List(List((a,1), (b,1)), List((a,1), (b,2)),
                                                  //|  List((a,1)), List((a,2), (b,1)), List((a,2), (b,2)), List((a,2)), List((b,1
                                                  //| )), List((b,2)), List())

  def mergeAndCombine(listA: List[Occurrences], listB: List[Occurrences]): List[Occurrences] = {
     for {
		x <- listA
		y <- listB
	} yield (x ++ y).sorted
  }                                               //> mergeAndCombine: (listA: List[forcomp.Combine.Occurrences], listB: List[forc
                                                  //| omp.Combine.Occurrences])List[forcomp.Combine.Occurrences]
                                                  
   def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case Nil => List(Nil)
    case x :: xs => {
      mergeAndCombine(generateSimples(x), combinations(xs))
    }
  }                                               //> combinations: (occurrences: forcomp.Combine.Occurrences)List[forcomp.Combine
                                                  //| .Occurrences]

  combinations(List(('a',2),('b',2))).toSet       //> res1: scala.collection.immutable.Set[forcomp.Combine.Occurrences] = Set(List
                                                  //| ((a,1)), List((b,1)), List((b,2)), List((a,1), (b,1)), List(), List((a,2), (
                                                  //| b,1)), List((a,2)), List((a,1), (b,2)), List((a,2), (b,2)))
                                                  
                                                  
 List(('a',1), ('b',2)).toMap                     //> res2: scala.collection.immutable.Map[Char,Int] = Map(a -> 1, b -> 2)
 
 type Word = String
   type Sentence = List[Word]
   
   def wordOccurrences(w: Word): Occurrences = {
    (for {
      (k, v) <- w.toLowerCase.toList.groupBy((c: Char) => c)
    } yield (k, v.length)).toList.sorted
  }                                               //> wordOccurrences: (w: forcomp.Combine.Word)forcomp.Combine.Occurrences

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.mkString(""))
                                                  //> sentenceOccurrences: (s: forcomp.Combine.Sentence)forcomp.Combine.Occurrenc
                                                  //| es
   
   val sentence = List("I", "man")                //> sentence  : List[String] = List(I, man)
   
   sentenceOccurrences(sentence)                  //> res3: forcomp.Combine.Occurrences = List((a,1), (i,1), (m,1), (n,1))
                                                  
        combinations(sentenceOccurrences(sentence))
                                                  //> res4: List[forcomp.Combine.Occurrences] = List(List((a,1), (i,1), (m,1), (n
                                                  //| ,1)), List((a,1), (i,1), (m,1)), List((a,1), (i,1), (n,1)), List((a,1), (i,
                                                  //| 1)), List((a,1), (m,1), (n,1)), List((a,1), (m,1)), List((a,1), (n,1)), Lis
                                                  //| t((a,1)), List((i,1), (m,1), (n,1)), List((i,1), (m,1)), List((i,1), (n,1))
                                                  //| , List((i,1)), List((m,1), (n,1)), List((m,1)), List((n,1)), List())
}