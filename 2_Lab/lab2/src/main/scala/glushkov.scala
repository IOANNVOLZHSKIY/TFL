type State = Int
type StateTransitions = Map[Char, List[State]]

class Machine {
  var StartState = Int
  var FinalStates = List[State]
  var Transitions = Map[State, StateTransitions]
  var StateCounter = Int
}

object glushkov {

  def BuildMachine(): Unit ={
  }

}
