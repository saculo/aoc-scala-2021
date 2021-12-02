package example

import scala.io.Source

object SecondDay {
    trait Command
    case class Position(aim: Int, depth: Int, horizontal: Int)
    case class Down(value: Int) extends Command
    case class Forward(value: Int) extends Command
    case class Up(value: Int) extends Command

    def dummyPosition(): Int = {
        def commands: List[Command] = parse()

        def startPos: Position = Position(0, 0, 0)

        def position = commands.foldLeft(startPos)((currentPos, command) => command match {
            case Forward(value) => Position(currentPos.aim, currentPos.depth, currentPos.horizontal + value)
            case Down(value) => Position(currentPos.aim, currentPos.depth + value, currentPos.horizontal)
            case Up(value) => Position(currentPos.aim, currentPos.depth - value, currentPos.horizontal)
        })

        position.depth * position.horizontal
    }

    def position(): Int = {
        def commands: List[Command] = parse()

        def startPos = Position(0, 0, 0)

        def endPos = commands.foldLeft(startPos)((currentPos, a) => a match {
            case Forward(value) => Position(currentPos.aim, currentPos.depth + (currentPos.aim * value), currentPos.horizontal + value)
            case Down(value) => Position(currentPos.aim + value, currentPos.depth, currentPos.horizontal)
            case Up(value)  => Position(currentPos.aim - value, currentPos.depth, currentPos.horizontal)
        })

        endPos.horizontal * endPos.depth
    }

    def parse(): List[Command] =
        Source.fromResource("second.txt").getLines().toList
           .map(it => it.split(" "))
           .map { case Array(a, b) => (a, b.toInt) }
           .map { case (command, value) =>
               command match {
                   case "forward" => Forward(value)
                   case "down" => Down(value)
                   case "up" => Up(value)
               }
           }
}
