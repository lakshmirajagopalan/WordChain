import scala.collection.mutable

/**
 * Created by lakshmi on 6/8/14.
 */
class WordChain(adjList:Map[String, List[String]], questions:Map[String, String], Visited:collection.mutable.Map[String, Boolean]) {

  def getWordChain(src: String, dest: String, visited:List[String]): Option[List[String]] = {
    if (src == dest) return Option[List[String]](dest::Nil)

    val neighbours = adjList(src)
    if (neighbours contains dest) return Option[List[String]](List(dest))

    val unvisitedNeighbours = neighbours filter (x => !(visited contains x))
    if(unvisitedNeighbours.isEmpty) return Option[List[String]](List())

    val x:Option[List[String]] =  unvisitedNeighbours collectFirst  {case n if(!getWordChain(n, dest, src::visited).isEmpty) => val x =  getWordChain(n, dest, src::visited); if(x.isEmpty) n::Nil else n::(x.get)}
    return x

  }

}
object WordChain{

  def apply(dict:List[String], questions:Map[String, String]):WordChain = {
    new WordChain(createAdjacency(dict), questions, initVisitedMap(dict))
  }


  def createAdjacency(dictionary: List[String]) = {
    def findAllWordsDifferingByOne(inputWord: String) = {
      inputWord -> dictionary.filter(word => isDifferingByOne(inputWord ,word))
    }

    dictionary.map(findAllWordsDifferingByOne).toMap
  }

  def initVisitedMap(dict: List[String]):scala.collection.mutable.Map[String, Boolean] = {
    val visited = collection.mutable.Map(dict.map(d => d -> false).toMap.toSeq:_*)
    visited
  }
  def isDifferingByOne(word1:String, word2:String) = {
    (word1 zip word2).count(k => k._1 != k._2) == 1
  }

}
