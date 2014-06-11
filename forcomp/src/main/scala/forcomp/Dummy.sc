package forcomp
import common._

object Dummy {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  val occurences = List(('a', 2), ('b', 2), ('c', 2))
                                                  //> occurences  : List[(Char, Int)] = List((a,2), (b,2), (c,2))
  val abc = List(('a', 2), ('b', 3), ('c', 1))    //> abc  : List[(Char, Int)] = List((a,2), (b,3), (c,1))

  def generateSimples(occurences3: List[(Char, Int)]): List[List[(Char, Int)]] = {
    (for {
      (k, occ) <- occurences3
      i <- 1 to occ
    } yield List((k, i)) ) ++ List(Nil)
  }                                               //> generateSimples: (occurences3: List[(Char, Int)])List[List[(Char, Int)]]

  generateSimples(abc)                            //> res0: List[List[(Char, Int)]] = List(List((a,1)), List((a,2)), List((b,1)), 
                                                  //| List((b,2)), List((b,3)), List((c,1)), List())
generateSimples(List(('a',2)))                    //> res1: List[List[(Char, Int)]] = List(List((a,1)), List((a,2)), List())

  def generateSimple(occurence: (Char, Int)): List[List[(Char, Int)]] = {
    (for {
      i <- (1 to occurence._2)
    } yield List((occurence._1, i))).toList ++ List(Nil)
  }                                               //> generateSimple: (occurence: (Char, Int))List[List[(Char, Int)]]

  generateSimple(('a', 2))                        //> res2: List[List[(Char, Int)]] = List(List((a,1)), List((a,2)), List())

  def combine(occurences4: List[(Char, Int)]): List[List[(Char, Int)]] = {
    for {
      (k, occ) <- occurences4
      (l, occ2) <- occurences4
      if k < l
    } yield List((k, occ), (l, occ2))
  }                                               //> combine: (occurences4: List[(Char, Int)])List[List[(Char, Int)]]

  combine(abc)                                    //> res3: List[List[(Char, Int)]] = List(List((a,2), (b,3)), List((a,2), (c,1)),
                                                  //|  List((b,3), (c,1)))

  def simpleC(occurences: List[(Char, Int)]): List[(Char, Int)] = {
    for {
      (k, occ) <- occurences
      i <- 1 to occ
    } yield (k, i)
  }                                               //> simpleC: (occurences: List[(Char, Int)])List[(Char, Int)]
  simpleC(occurences)                             //> res4: List[(Char, Int)] = List((a,1), (a,2), (b,1), (b,2), (c,1), (c,2))

  def comb(occurrences2: List[(Char, Int)]): List[List[(Char, Int)]] = {
    val zero = List(Nil)
    occurrences2 match {
      case Nil => zero
      case (x, y) :: Nil => generateSimple((x, y)) ++ zero

      case (x, y) :: xs => generateSimple((x, y)) ++ comb(xs)

    }
  }                                               //> comb: (occurrences2: List[(Char, Int)])List[List[(Char, Int)]]

  comb(abc)                                       //> res5: List[List[(Char, Int)]] = List(List((a,1)), List((a,2)), List(), List
                                                  //| ((b,1)), List((b,2)), List((b,3)), List(), List((c,1)), List(), List())

  def secondC(occurences: List[(Char, Int)]): List[List[(Char, Int)]] = {
    for {
      (k, occ) <- occurences
      (l, occ2) <- occurences
      if k < l
    } yield List((k, occ), (l, occ2))
  }                                               //> secondC: (occurences: List[(Char, Int)])List[List[(Char, Int)]]

  secondC(simpleC(occurences))                    //> res6: List[List[(Char, Int)]] = List(List((a,1), (b,1)), List((a,1), (b,2))
                                                  //| , List((a,1), (c,1)), List((a,1), (c,2)), List((a,2), (b,1)), List((a,2), (
                                                  //| b,2)), List((a,2), (c,1)), List((a,2), (c,2)), List((b,1), (c,1)), List((b,
                                                  //| 1), (c,2)), List((b,2), (c,1)), List((b,2), (c,2)))
  
  
  def merge(listA: List[List[(Char,Int)]],listB: List[List[(Char,Int)]]): List[List[(Char,Int)]] = listA match{
  	case Nil => listB ++ List(Nil)
  	case x :: Nil => listB.map(y => y ++ x) ++ listB ++ List(x) ++ List(Nil)
  	case x :: xs => listB.map(y => y ++ x) ++ merge(xs, listB)++ listB ++ List(x) ++ List(Nil)
  }                                               //> merge: (listA: List[List[(Char, Int)]], listB: List[List[(Char, Int)]])List
                                                  //| [List[(Char, Int)]]

val ab = List(List(('a',1)),List(('b',1)),List(('a',1),('b',1)))
                                                  //> ab  : List[List[(Char, Int)]] = List(List((a,1)), List((b,1)), List((a,1), 
                                                  //| (b,1)))
val c= List(List(('c',1)))                        //> c  : List[List[(Char, Int)]] = List(List((c,1)))

merge(ab,c).toSet                                 //> res7: scala.collection.immutable.Set[List[(Char, Int)]] = Set(List((c,1)), 
                                                  //| List((a,1)), List((b,1)), List((a,1), (b,1)), List(), List((c,1), (b,1)), L
                                                  //| ist((c,1), (a,1)), List((c,1), (a,1), (b,1)))


}