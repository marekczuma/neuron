/**
  * Created by maro on 09.06.2017.
  */
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks
class NeuronMultiInputs (var numberOfInputs :Int, val beta :Float) {
  var inputs = prepareInputs
  var biasInput = new InputNeuron(1, 0.5f)
  val learningFactor :Float = 0.6f
  val maxDefect :Float = 0.1f
  var counter :Int = 0
  var counterLine :Int = 0

  def aggregateBlock: Float = {
    var z :Float = 0
    var a :Int = 0
    for (a <- 0 until (numberOfInputs - 1)){
      z += inputs(a).input * inputs(a).weight
    }
    z + biasInput.input*biasInput.weight
  }

  // Funkcja sigmoidalna unipolarna (krzywa logistyczna)
  def activationBlock: Float = {
    val index = -1 * beta * aggregateBlock
    1.0f/(1.0f + Math.pow(Math.E, index).toFloat)
  }

  def prepareInputs: ArrayBuffer[InputNeuron]={
    var firstInputs = ArrayBuffer[InputNeuron]()
    var a :Int = 0
    for (a <- 1 to numberOfInputs){
      var tmpInput = new InputNeuron(Math.random().toFloat, Math.random().toFloat)
      firstInputs += tmpInput
    }
    firstInputs
  }


  def learn2(learningSet :scala.collection.mutable.Map[ArrayBuffer[Float], Int]): Unit = {
    var mistakes = scala.collection.mutable.Map[ArrayBuffer[Float], Float]()
    var isLearned: Boolean = false
    counter += 1
    while(!isLearned){
      for((k, v) <- learningSet) {
        var a :Int = 0
        // Wrzucamy na wejścia wejścia z setu uczącego
        for (a <- 0 until (numberOfInputs - 1)){
          inputs(a).input = k(a)
        }
        val result = activationBlock
        counterLine += 1
        var currentDefect :Float = Math.abs(v - result)
        mistakes(k) = currentDefect
        var currentAverage = calculateFinalAverage(mistakes.values.toArray)
        println("klucz: " + k.toString() + "; oczekiwana wartość: " + v +  "; Wynik: " + result + "; błąd: " + (v - result) + "; średnia: " + currentAverage + "; miejsce w zbiorze: " + counter + "; linia numer: " + counterLine)
        if(currentAverage <= maxDefect){
        // Aktualny błą∂ (v - to co powinno być, result - aktualny wynik)

          isLearned = true
        }else {
          for (a <- 0 until (numberOfInputs - 1)) {
            inputs(a).weight = inputs(a).weight + inputs(a).input * learningFactor * (v - result) * result * (1 - result)
          }
          biasInput.weight = biasInput.weight + biasInput.input * learningFactor * (v - result) * result * (1 - result)
          isLearned = false
        }
      }
    }
  }

  def learn(learningSet :scala.collection.mutable.Map[ArrayBuffer[Float], Int]): Unit = {
    for((k, v) <- learningSet) {
      // 1.Wrzuć przykład z setu uczącego
      // 2.Sprawdź czy jest dobre wyjście
      // 3.Jeśli tak, przejdź do następnego przykładu z setu uczącego (pkt 1)
      // 4.Jeśli nie, popraw wagi i zostaw obecny przykład (wróć do pkt 1)
      var isLearned: Boolean = false
      counter += 1
      while(!isLearned){
        var a :Int = 0
        for (a <- 0 until (numberOfInputs - 1)){
          inputs(a).input = k(a)
        }
        val result = activationBlock
        counterLine += 1
        println("klucz: " + k.toString() + "; oczekiwana wartość: " + v +  "; Wynik: " + result + "; błąd: " + (v - result) + "; miejsce w zbiorze: " + counter + "; linia numer: " + counterLine)
        var currentDefect :Float = Math.abs(v - result)

        // TU JEST PROBLEM (Przerywa naukę w momencie w którym ostatnie wejście z setu uczącego będzie prawidłowe)
        if( currentDefect <= maxDefect){
          isLearned = true
        }else{
          for (a <- 0 until (numberOfInputs - 1)){
            inputs(a).weight = inputs(a).weight + inputs(a).input*learningFactor*(v - result) * result * (1 - result)
          }
          biasInput.weight = biasInput.weight + biasInput.input*learningFactor*(v - result) * result * (1 - result)
        }
      }
    }
  }

  def calculateFinalAverage(ingridients :Array[Float]) :Float = {
    var average :Float = 0
    for (ingridient <- ingridients){
      average += ingridient
    }
    average/ingridients.size
  }

}
