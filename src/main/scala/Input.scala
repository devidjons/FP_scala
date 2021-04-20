import State._

sealed trait Input
case object Coin extends Input
case object Turn extends Input
case class Machine(locked: Boolean, candies: Int, coins: Int)


object Machine{
  def getInput(inp:Input):State[Machine, (Int, Int)]={
    m=> {
      inp match {
        case Coin if m.candies > 0 && m.locked => {println("correct coin"); ((m.candies, m.coins + 1), Machine(false, m.candies, m.coins + 1))}
        case Turn if !m.locked => {println("correct turn");((m.candies - 1, m.coins), Machine(true, m.candies-1, m.coins))}
        case _ => ((m.candies, m.coins), m)
      }
    }
  }
  def init():State[Machine, (Int, Int)]={
    m=> ((m.candies, m.coins),m)
  }
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)]={
      List.foldLeft(inputs, init())((acc,el)=>flatMap(acc)(x=> getInput(el)))
  }
  def simulateMachineSeq(inputs: List[Input]): State[Machine, (Int, Int)]={
    flatMap(flatMap(sequence(List.map(inputs)(getInput)))(x=> get[Machine]))(x=> init())
  }
}
