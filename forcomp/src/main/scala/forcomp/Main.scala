package forcomp

object Main extends App {
  import Anagrams._
  
  val sentence = List("Linux","rulez")
  
  val sentenceOcc = sentenceOccurrences(sentence)
  println(sentenceOcc)
  val comb = combinations(sentenceOcc)
  //println(comb)
  
  
  def sentAna(sentOcc: Occurrences): List[List[Object]] = sentOcc match{
    case Nil => List(Nil)
    case _ =>
      for {
          x <- combinations(sentOcc)
          word <- dictionaryByOccurrences.get(x)
          if ((dictionaryByOccurrences contains x))
        }  yield (word ++ sentAna(subtract(sentOcc,x)))
        
    }
  
  println(sentAna(sentenceOcc))
  
}

