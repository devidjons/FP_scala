import State._

sealed trait Input
case object Coin extends Input
case object Turn extends Input
case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine{
  def getInput(inp:Input):State[Machine, (Int, Int)]={
    val newRun = (m:Machine)=> {
      inp match {
        case Coin if m.candies > 0 && m.locked => {println("correct coin"); ((m.candies, m.coins + 1), Machine(false, m.candies, m.coins + 1))}
        case Turn if !m.locked => {println("correct turn");((m.candies - 1, m.coins), Machine(true, m.candies-1, m.coins))}
        case _ => ((m.candies, m.coins), m)
      }
    }
    State(newRun)
  }
  def init():State[Machine, (Int, Int)]={
    State((m:Machine)=> ((m.candies, m.coins),m))
  }
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)]={
      inputs.foldLeft(init())((acc, el)=>acc.flatMap(x=> getInput(el)))
  }
  def simulateMachineSeq(inputs: List[Input]): State[Machine, (Int, Int)]={
    sequence(inputs.map(getInput)).flatMap(x=>State.get[Machine]).flatMap(x=> init())
  }
}
