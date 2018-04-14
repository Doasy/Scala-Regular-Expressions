package main

import scala.io.Source
import scala.util.matching.Regex

class Calculator {
  var variables:Map[String,Float] = Map()
  val operationPatters = new Regex("Calc: [0-9]+(\\.[0-9]+)? ?" +
    "[\\+|\\*|\\-|\\/] ?" +
    "[0-9]+(\\.[0-9]+)? ?;")
  val operationPatternWithVariebleInFirstPos= new Regex("Calc: [a-z] ?" +
    "[\\+|\\*|\\-|\\/] ?" +
    "[0-9]+(\\.[0-9]+)? ?;")
  val operationPatternWithVariebleInSecondPos= new Regex("Calc: [0-9]+(\\.[0-9]+)? ?" +
    "[\\+|\\*|\\-|\\/] ?" +
    "[a-z] ?;")
  val operationPatternWithTwoVariables= new Regex("Calc: [a-z] ?" +
    "[\\+|\\*|\\-|\\/] ?" +
    "[a-z] ?;")
  val varPattern = new Regex("[a-z] ?\\= ?[0-9]+(\\.[0-9]+)? ?;")

  def matchWithRegularExpression(line: String){
    if ((operationPatters findAllIn line).mkString(",") == line) {
      println("(OK)" + line)
      var (symbol: String, n1: Float, n2: Float) = findN1N2AndSymbol(line)
      calculate(symbol, n1, n2)

    }else if((operationPatternWithVariebleInFirstPos findAllIn line).mkString(",") == line){
      println("(OK)" + line)
      val (n1: Float, symbol: String, n2: Float) = findN1AndN2AndSymbolWithVariable(line, first = true)
      calculate(symbol, n1, n2)
    }else if((operationPatternWithVariebleInSecondPos findAllIn line).mkString(",") == line){
      println("(OK)" + line)
      val (n1: Float, symbol: String, n2: Float) = findN1AndN2AndSymbolWithVariable(line, first = false)
      calculate(symbol, n1, n2)
    }else if((operationPatternWithTwoVariables findAllIn line).mkString(",") == line){
      println("(OK)" + line)
      val (symbol: String, n1: Float, n2: Float) = findN1N2AndSymbolWithTwoVariables(line)
      calculate(symbol, n1, n2)
    }else if((varPattern findAllIn line).mkString(",") == line){
      println("(OK)" + line)
      keepVariable(line)
    }else{
      println("(KO)"+line)
    }
  }


  private def keepVariable(line: String): Unit = {
    val charPattern = new Regex("[a-z]")
    var id = (charPattern findAllIn line).mkString(",")
    val numPattern = new Regex("[0-9]+(\\.[0-9]+)?")
    var value = (numPattern findAllIn line).mkString(",").toFloat
    variables += (id -> value)
  }

  private def calculate(symbol: String, n1: Float, n2: Float): Unit = {


    val (n1String: String) = stringAsIntOrFloat(n1)
    val (n2String: String) = stringAsIntOrFloat(n2)
    if (symbol == "+") {
      val result = n1 + n2
      val (resultString: String) = stringAsIntOrFloat(result)
      println("La suma de %s i %s és igual a %s".format(n1String, n2String, resultString))
    } else if (symbol == "-") {
      val result = n1 - n2
      val (resultString: String) = stringAsIntOrFloat(result)
      println("La resta de %s i %s és igual a %s".format(n1String, n2String, resultString))
    } else if (symbol == "*") {
      val result = n1 * n2
      val (resultString: String) = stringAsIntOrFloat(result)
      println("La multiplicació de %s i %s és igual a %s".format(n1String, n2String, resultString))
    } else if (symbol == "/") {
      val result = n1 / n2
      val (resultString: String) = stringAsIntOrFloat(result)
      println("La divisió de %s i %s és igual a %s".format(n1String, n2String, resultString))
    }
  }

  private def stringAsIntOrFloat(n: Float) = {
    var actualN: String = "0"
    if (n == Math.round(n)) {
      val temp = n.toInt
      actualN = temp.toString
    } else {
      actualN = n.toString
    }
    actualN
  }
  private def findN1N2AndSymbol(line: String) = {
    val n1Pattern = new Regex("Calc: [0-9]+(\\.[0-9]+)?")
    val symbolPattern: Regex = new Regex("[\\+|\\*|\\-|\\/]")
    val n2Pattern = new Regex("[0-9]+(\\.[0-9]+)? ?;")

    val n1temp = (n1Pattern findAllIn line).mkString(",")
    val symbol = (symbolPattern findAllIn line).mkString(",")
    val n2temp = (n2Pattern findAllIn line).mkString(",")

    val nPattern = new Regex("[0-9]+(\\.[0-9]+)?")

    val n1 = (nPattern findAllIn n1temp).mkString(",").toFloat
    val n2 = (nPattern findAllIn n2temp).mkString(",").toFloat
    (symbol, n1, n2)
  }

  private def findN1N2AndSymbolWithTwoVariables(line: String) = {
    val n1Pattern = new Regex(": [a-z]")
    val symbolPattern = new Regex("[\\+|\\*|\\-|\\/]")
    val n2Pattern = new Regex("[a-z] ?;")

    val n1temp = (n1Pattern findAllIn line).mkString(",")
    val symbol = (symbolPattern findAllIn line).mkString(",")
    val n2temp = (n2Pattern findAllIn line).mkString(",")
    val varPattern = new Regex("[a-z]")

    val n1Var = (varPattern findAllIn n1temp).mkString(",")
    val n2Var = (varPattern findAllIn n2temp).mkString(",")
    (symbol, variables(n1Var), variables(n2Var))
  }
  private def findN1AndN2AndSymbolWithVariable(line: String, first: Boolean) = {
    val patterWithoutCalc=  new Regex("([0-9]+(\\.[0-9]+)?|[a-z]) ?" +
      "[\\+|\\*|\\-|\\/] ?" +
      "([0-9]+(\\.[0-9]+)?|[a-z]) ?;")
    val newLine = (patterWithoutCalc findAllIn line).mkString(",")
    val n1Pattern = new Regex("[a-z]")
    val symbolPattern = new Regex("[\\+|\\*|\\-|\\/]")
    val n2Pattern = new Regex("[0-9]+(\\.[0-9]+)?")

    val varN1 = (n1Pattern findAllIn newLine).mkString(",")
    var n1:Float = 0
    var n2:Float = 0
    if(first){
      n1 = variables(varN1)
      n2 = (n2Pattern findAllIn newLine).mkString(",").toFloat
    }else{
      n2 = variables(varN1)
      n1 = (n2Pattern findAllIn newLine).mkString(",").toFloat
    }
    val symbol = (symbolPattern findAllIn newLine).mkString(",")
    (n1, symbol, n2)
  }
}
object RunCalculator extends App {
  val calculator = new Calculator()

  val filename = "/home/alba/compiladors/Scala-Regular-Expressions/src/src/docs/testCalculator.txt"
  for (line <- Source.fromFile(filename).getLines) {
    calculator.matchWithRegularExpression(line)
  }
}

