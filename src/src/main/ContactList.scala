package main
import scala.io.Source
import scala.util.matching.Regex


class ContactList(){
  def matchWithRegularExpression(line: String){

    var pattern = new Regex("(Name: [A-Z][a-z]+( [A-Z][a-z]*)?( [A-Z][a-z]*)?)|" +
                            "(email: [a-z]+@[a-z]+\\.[a-z]+)|" +
                            "(Telf: [0-9]{9})")
    if ((pattern findAllIn line).mkString(",") == line){
      println("(OK)"+line)
    }else{
      println("(KO)"+line)
    }
  }
}
object ContactList extends App {
  val ct = new ContactList()

  val filename = "/home/alba/compiladors/Scala-Regular-Expressions/src/src/docs/testContactList.txt"
  for (line <- Source.fromFile(filename).getLines) {
    ct.matchWithRegularExpression(line)
  }
}

