package bloxorz

case class Direction(dir : Char)

object MoveConverter {
  def convertInputToMove(input : Char) : Direction = {
    input match {
      case 'w' | 'W' => Direction('u')
      case 'a' | 'A' => Direction('l')
      case 's' | 'S' => Direction('d')
      case 'd' | 'D' => Direction('r')
    }
  }
}

object MoveReader {
  def readMovesFromFile(path : String) : List[Char] = {
    val moves = scala.io.Source.fromFile(path).getLines().toList
    if(moves.length != moves.flatten.length & moves.nonEmpty) throw new Error("Incorrect move list format.")
    else moves.map {
      case "u" => 'u'
      case "d" => 'd'
      case "l" => 'l'
      case "r" => 'r'
      case _ => throw new Error("Incorrect move list format")
    }
  }
}