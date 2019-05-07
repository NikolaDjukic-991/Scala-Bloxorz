package bloxorz

object LevelCollection {
  var levels: List[Level] = List[Level]()
  def printLevelChooseMenu(): Unit = {
    def printLevels(levels: List[Level], cnt : Int) : Unit = {
      levels match {
        case level :: xs =>
          println(cnt + ".")
          println(level)
          printLevels(xs, cnt+1)
        case Nil => if(cnt == 1) println("Please load levels first.")
      }
    }

    printLevels(levels, 1)
    println("Choose level: ")
  }

  def chooseLevel(cnt : Int) : Level = {
    def chooseLevelInner(levels : List[Level], curElem : Int) : Level= {
      levels match {
        case Nil => throw new Error("Level not found.")
        case level :: xs =>
          if (curElem == cnt) level
          else chooseLevelInner(xs, curElem + 1)
      }
    }

    chooseLevelInner(levels, 1)
  }

}


