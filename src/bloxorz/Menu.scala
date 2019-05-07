package bloxorz

import bloxorz.LevelEditor.readLevelFromFile
import bloxorz.MoveReader.readMovesFromFile

object Menu {
  def main(args: Array[String]): Unit = {
    var opt = 0


    def printMenu(): Unit = {
      println("Main menu:")
      println("")
      println("1. Load level from file")
      println("2. Play level")
      println("3. Execute moves from file")
      println("4. Solve level")
      println("")
      println("Choice: ")

    }


    def mainMenu(): Unit = {
      do {
        printMenu()
        try {
          opt = scala.io.StdIn.readInt()

          opt match {
            case 1 =>
              println("Path to file: ")
              LevelCollection.levels = readLevelFromFile(scala.io.StdIn.readLine()) :: LevelCollection.levels
              println(LevelCollection.levels)
            case 2 =>
              LevelCollection.printLevelChooseMenu()
              val lvlChoice = scala.io.StdIn.readLine().toInt
              if(lvlChoice != 0){
                val level = LevelCollection.chooseLevel(lvlChoice)
                var replay = 'n'
                do {
                  val retCode = Game.playLevel(level)
                  replay = 'n'
                  retCode match {
                    case 'L' =>
                      println("You lost. Replay? (y/n)")
                      do {
                        replay = scala.io.StdIn.readChar()
                        if(replay != 'n' & replay != 'y' & replay != 'N' & replay != 'Y') println("Incorrect input. Required y or n.")
                      } while(replay != 'n' & replay != 'y' & replay != 'N' & replay != 'Y')
                    case 'W' =>
                      println("You won. Replay? (y/n)")
                      do {
                        replay = scala.io.StdIn.readChar()
                        if(replay != 'n' & replay != 'y' & replay != 'N' & replay != 'Y') println("Incorrect input. Required y or n.")
                      } while(replay != 'n' & replay != 'y' & replay != 'N' & replay != 'Y')
                    case 'Q' | 'q' => 'n'
                    case 'R' | 'r' => 'y'
                  }
                } while(replay == 'y' | replay == 'Y')
              }
            case 3 =>
              println("Path to move list: ")
              val moves = readMovesFromFile(scala.io.StdIn.readLine())
              LevelCollection.printLevelChooseMenu()
              val lvlChoice = scala.io.StdIn.readLine().toInt
              if(lvlChoice != 0)
                Game.executeMovesOnLevel(LevelCollection.chooseLevel(lvlChoice), moves)
            case 4 =>
              LevelCollection.printLevelChooseMenu()
              val lvlChoice = scala.io.StdIn.readLine().toInt
              if(lvlChoice != 0)
                Solver.solve(LevelCollection.chooseLevel(lvlChoice))


            case 0 =>
            case _ => throw new Error("Invalid menu option.")

          }
        } catch {
          case e : Error => println(e)
          case _ : NumberFormatException => println("Invalid menu option")
        }
      } while (opt != 0)


    }

    mainMenu()


  }
}
