/**
  * Created by maro on 30.05.2017.
  */
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map

object Main extends App {
  var inputN = new InputNeuron(0,0.02f)
  var n = new Neuron(inputN, 0.2f)
//  val learningSet = prepareLearningSet
//  val learningAndSet = prepareAndLearningSet
//  var learningMechanism = new LearningMechanism(learningSet, n)
//  n = learningMechanism.learnNeuron
//  n.input.input = 0.1f
//  println((n.activationBlock))

  // Symulacja tego jak ma wyglądać użycie neuronu
  var andNeuron :NeuronMultiInputs = new NeuronMultiInputs(2, 0.5f)
  andNeuron.learn2(prepareAndLearningSet);
  andNeuron.inputs(0).input = 0
  andNeuron.inputs(1).input = 0
  println("======= TEST: 0;0. Oczekiwane: 0 ========")
  println("Wynik:" + andNeuron.activationBlock)
  andNeuron.inputs(0).input = 0
  andNeuron.inputs(1).input = 1
  println("======= TEST: 0;1. Oczekiwane: 0 ========")
  println("Wynik:" + andNeuron.activationBlock)
  andNeuron.inputs(0).input = 1
  andNeuron.inputs(1).input = 0
  println("======= TEST: 1;0. Oczekiwane: 0 ========")
  println("Wynik:" + andNeuron.activationBlock)
  andNeuron.inputs(0).input = 1
  andNeuron.inputs(1).input = 1
  println("======= TEST: 1;1. Oczekiwane: 1 ========")
  println("Wynik:" + andNeuron.activationBlock)
//  for((k, v) <- learningSet) {
//    println("klucz: " + k + "; wartość: " + v);
//  }
//  println(learningSet)

  def prepareLearningSet :scala.collection.mutable.Map[Float, Int] ={
    var set = scala.collection.mutable.Map[Float, Int]();
    var a :Float = 0;
    for (a <- 1 to 1000){
      val arg = a.toFloat/1000;
      if(a.toFloat%2 == 0){
        set(arg) = 1;
      }else{
        set(arg) = 0;
      }
    }
    return set;
  }

  def prepareAndLearningSet :Map[ArrayBuffer[Float], Int] ={
    var set = Map[ArrayBuffer[Float], Int]()
    set(ArrayBuffer(0,0)) = 0
    set(ArrayBuffer(0,1)) = 0
    set(ArrayBuffer(1,0)) = 0
    set(ArrayBuffer(1,1)) = 1
    return set
  }

  def prepareEvenLearningSet :Map[ArrayBuffer[Float], Int] ={
    var set = Map[ArrayBuffer[Float], Int]()
    var a :Float = 0;
    for (a <- 1 to 1000){
      val arg = a.toFloat/1000;
      if(a.toFloat%2 == 0){
        set(ArrayBuffer(arg)) = 1;
      }else{
        set(ArrayBuffer(arg)) = 0;
      }
    }
    return set
  }
}