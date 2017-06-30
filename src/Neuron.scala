/**
  * Created by maro on 30.05.2017.
  */
class Neuron(val input: InputNeuron, val beta: Float) {
  def aggregateBlock: Float = {
    return input.input * input.weight
  }

  // Funkcja sigmoidalna unipolarna (krzywa logistyczna)
  def activationBlock: Float = {
    val index = -1 * beta * aggregateBlock
    return 1.0f/(1.0f + Math.pow(Math.E, index).toFloat)
  }
}
