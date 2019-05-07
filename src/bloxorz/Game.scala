package bloxorz

object Game {

  def checkWinCondition(level: Level) : Int = {
    def checkBoardColumns(fields : List[Field]) : Int = {
      fields match {
        case Nil => 0
        case EndPos(1) :: xs => 1
        case (NoTile(1) | NoTile(2)) :: xs => -1
        case WeakTile(1) :: xs => -1
        case _ :: xs => checkBoardColumns(xs)
      }
    }

    def checkBoardRows(fields : List[List[Field]]) : Int = {
      fields match {
        case Nil => 0
        case tileList :: xs =>
          val winCond = checkBoardColumns(tileList)
          if (winCond != 0) winCond
          else checkBoardRows(xs)
      }
    }

    checkBoardRows(level.fields)
  }

  def playLevel(level : Level) : Char = {
    println(level)
    checkWinCondition(level) match {
      case -1 =>
        return 'L'
      case 1 =>
        return 'W'
      case _ =>
    }

    println("Make your move W/A/S/D, Q to quit, R to reset")
    val input = scala.io.StdIn.readChar()

    if(input == 'q' || input == 'Q' || input == 'r' || input == 'R') return input
    try {
      val updatedLevel = level.updateBoardState(MoveConverter.convertInputToMove(input))
      playLevel(updatedLevel)
    } catch {
      case _ : IndexOutOfBoundsException => println("exception"); 'L'
    }
  }

  def executeMovesOnLevel(level : Level, moves : List[Char]): Unit = {
    def executeMoves(level : Level, moves : List[Char]) : Level ={
      moves match {
        case Nil => level
        case x :: xs =>
          try {
            val updatedLevel = level.updateBoardState(Direction(x))
            if (checkWinCondition(level) == 0) executeMoves(updatedLevel, xs)
            else level
          } catch {
            case _ : IndexOutOfBoundsException => println("exception"); level
          }
      }
    }

    val endResult = executeMoves(level, moves)
    println(endResult)
    checkWinCondition(endResult) match {
      case 1  => println("Executed moves resulted in a win.")
      case -1 => println("Executed moves resulted in a loss.")
      case 0  => println("Executed moves resulted in the printed board state.")
    }
    if(checkWinCondition(endResult) == 1) println("Executed moves resulted in a win.")
    if(checkWinCondition(endResult) == -1) println("Executed moves resulted in a loss.")
    if(checkWinCondition(endResult) == -1) println("Executed moves resulted in a loss.")
  }

}

