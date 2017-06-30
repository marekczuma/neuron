/**
  * Created by maro on 05.06.2017.
  */
class LearningMechanism(learningSet :scala.collection.mutable.Map[Float, Int], var neuron: Neuron) {
  val learningFactor :Float = 0.4f
  val maxDefect :Float = 0.1f
  var counter :Int = 0
  var counterLine :Int = 0

  def learnNeuron(): Neuron = {
    for((k, v) <- learningSet) {
      counter += 1
      if (counter == 4) {
        println("4")
      }
      var isLearned: Boolean = false
      neuron.input.input = k
      while (!isLearned) {
        val result = neuron.activationBlock
        counterLine += 1
        println("klucz: " + k.toString() + "; oczekiwana wartość: " + v + "; Wynik: " + result + "; waga: " + neuron.input.weight + "; błąd: " + (v - result) + "; miejsce w zbiorze: " + counter + "; linia numer: " + counterLine)
        if (Math.abs(v - result) <= maxDefect) {
          isLearned = true
        } else {
          neuron.input.weight = neuron.input.weight + neuron.input.input * learningFactor * (v - result) * result * (1 - result)
        }
      }
    }
    neuron
  }
}
