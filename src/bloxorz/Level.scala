package bloxorz

case class Level(fields : List[List[Field]]) {

  def updateBoardState(d : Direction) : Level = {
    def findBlockPosition(fields : List[List[Field]], fieldsWithBlock : List[Field]): List[Field] = {
      fields.flatten.filter(field => field.hasBlock)
    }



    def findFieldIndex(field : Field) : (Int, Int) = {
      def findFieldFlatIndex(fields : List[Field], field : Field, partial : Int) : Int = {
        fields match {
          case Nil => throw new Error("Field not found.")
          case x :: xs => if(x eq field) partial else findFieldFlatIndex(xs, field, partial+1)
        }
      }
      val numCols = fields.head.length
      val flatIndex = findFieldFlatIndex(fields.flatten, field, 0)
      (flatIndex / numCols, flatIndex % numCols)
    }

    def findNewBlockPosition(position: List[Field], direction: Direction): List[Field] = {
      def findUprightBlockNewPosition(pos : Field, direction: Direction) : List[Field] = {
        val (posFieldRow, posFieldCol) = findFieldIndex(pos)
        direction.dir match {
          case 'u' => fields(posFieldRow-2)(posFieldCol) :: fields(posFieldRow-1)(posFieldCol) :: Nil
          case 'd' => fields(posFieldRow+1)(posFieldCol) :: fields(posFieldRow+2)(posFieldCol) :: Nil
          case 'l' => fields(posFieldRow)(posFieldCol-2) :: fields(posFieldRow)(posFieldCol-1) :: Nil
          case 'r' => fields(posFieldRow)(posFieldCol+1) :: fields(posFieldRow)(posFieldCol+2) :: Nil
          case _ => throw new Error("Incorrect direction")
        }
      }
      def findLayingBlockNewPosition(position : List[Field], direction: Direction) : List[Field] = {
        val(x1,y1) = findFieldIndex(position.head)
        val(x2,y2) = findFieldIndex(position(1))
        direction.dir match {
          case 'u' =>
            if(x1 == x2) fields(x1 - 1)(y1) :: fields(x2 - 1)(y2) :: Nil
            else
            {
              val x = if(x1 < x2) x1 else x2
              fields(x-1)(y1) :: Nil
            }
          case 'd' =>
            if(x1 == x2) fields(x1 + 1)(y1) :: fields(x2 + 1)(y2) :: Nil
            else
            {
              val x = if(x1 > x2) x1 else x2
              fields(x+1)(y1) :: Nil
            }
          case 'l' =>
            if(y1 == y2) fields(x1)(y1-1) :: fields(x2)(y2-1) :: Nil
            else
            {
              val y = if(y1 < y2) y1 else y2
              fields(x1)(y-1) :: Nil
            }
          case 'r' =>
            if(y1==y2) fields(x1)(y1+1) :: fields(x2)(y2+1) :: Nil
            else
            {
              val y = if(y1>y2) y1 else y2
              fields(x1)(y+1) :: Nil
            }
        }
      }
      if(position.length == 1) findUprightBlockNewPosition(position.head, d)
      else if(position.length == 2) findLayingBlockNewPosition(position, d)
      else throw new Error("Invalid block position.")
    }

    val blockPosList = findBlockPosition(fields, List())
    val newBlockPosList = findNewBlockPosition(blockPosList, d)
    val toUpdate = blockPosList ::: newBlockPosList

    updateBoardFields(toUpdate, newBlockPosList.length)
  }

  def transformBoardAtField(toUpdate : Field, f : Field => Field): Level ={
    Level(fields.map(rows => rows.map(field =>
      if(field eq toUpdate)
        f(field)
      else field
    )))
  }

  def updateBoardFields(toUpdate : List[Field], newBlockOrientation : Int) : Level = {
    def invertField(f : Field) : Field = {
      if(f.hasBlock)
        f match {
          case Tile(_) => Tile(0)
          case WeakTile(_) => WeakTile(0)
          case NoTile(_) => NoTile(0)
          case StartingPos(_) => StartingPos(0)
          case EndPos(_) => EndPos(0)
        }
      else
        f match {
          case Tile(_) => Tile(newBlockOrientation)
          case WeakTile(_) => WeakTile(newBlockOrientation)
          case NoTile(_) => NoTile(newBlockOrientation)
          case StartingPos(_) => StartingPos(newBlockOrientation)
          case EndPos(_) => EndPos(newBlockOrientation)
        }
    }

    toUpdate match {
      case Nil => this
      case x :: xs => transformBoardAtField(x,invertField).updateBoardFields(xs, newBlockOrientation)
    }
  }

  def printRow(row : List[Field], partial : String): String ={
    row match {
      case Nil => partial
      case field :: xs =>
        if(field.hasBlock)
          field match {
            case Tile(_) => printRow(xs, partial + "B")
            case WeakTile(_) => printRow(xs, partial + "B")
            case NoTile(_) => printRow(xs, partial + "B")
            case StartingPos(_) => printRow(xs, partial + "B")
            case EndPos(_) => printRow(xs, partial + "B")
          }
        else
          field match {
            case Tile(_) => printRow(xs, partial + "o")
            case WeakTile(_) => printRow(xs, partial + ".")
            case NoTile(_) => printRow(xs, partial + "-")
            case StartingPos(_) => printRow(xs, partial + "o")
            case EndPos(_) => printRow(xs, partial + "T")
          }
    }
  }

  def printLevel(fields : List[List[Field]], partial : String) : String = {
    fields match {
      case Nil => partial
      case x :: xs => printLevel(xs, printRow(x, partial) + "\n")
    }
  }
  override def toString: String = {
    printLevel(fields, "")
  }
}

object LevelEditor {
  def convertToFields(lines: List[String]) : List[List[Field]] = {
    def createListOfFields(s : List[Char]) : List[Field] = {
      s match {
        case 'o' :: suffix => Tile(0)        :: createListOfFields(suffix)
        case '.' :: suffix => WeakTile(0)    :: createListOfFields(suffix)
        case '-' :: suffix => NoTile(0)      :: createListOfFields(suffix)
        case 'S' :: suffix => StartingPos(1) :: createListOfFields(suffix)
        case 'T' :: suffix => EndPos(0)      :: createListOfFields(suffix)
        case Nil => Nil
        case _ => throw new Error("Incorrect level format " + s.head)
      }
    }
    lines match {
      case Nil => Nil
      case x :: xs => createListOfFields(x.toList) :: convertToFields(xs)
    }
  }

  def readLevelFromFile(path : String) : Level = {
    val lines = scala.io.Source.fromFile(path).getLines().toList
    padEdges(convertToFields(lines))
  }

  def padEdges(fields : List[List[Field]]) : Level = {
    def padColumns(fields : List[List[Field]]) : List[List[Field]] = {
      def padLeft(fields : List[List[Field]]) : List[List[Field]] = {
        val padding =
          if (fields.forall(row => row.head == NoTile(0)))
            if (fields.forall(row => row(1) != NoTile(0))) Nil
            else NoTile(0) :: Nil
          else NoTile(0) :: NoTile(0) :: Nil

        fields.map(padding ::: _)
      }
      def padRight(fields : List[List[Field]]) : List[List[Field]] = {
        val padding =
          if (fields.forall(row => row.last == NoTile(0)))
            if (fields.forall(row => row(row.length-2) == NoTile(0))) Nil
            else NoTile(0) :: Nil
          else NoTile(0) :: NoTile(0) :: Nil

        fields.map(_ ::: padding)
      }
      padLeft(padRight(fields))
    }

    def padRows(fields : List[List[Field]]) : List[List[Field]] = {
      def padTop(fields : List[List[Field]]) : List[List[Field]]  = {
        val noTileList = List.fill(fields.head.length)(NoTile(0))
        val padding =
          if (fields.head.forall(_ == NoTile(0)))
            if (fields(1).forall(_ == NoTile(0))) Nil
            else noTileList :: Nil
          else noTileList :: noTileList :: Nil

        padding ::: fields
      }
      def padBottom(fields : List[List[Field]]) : List[List[Field]] = {
        val noTileList = List.fill(fields.head.length)(NoTile(0))
        val padding =
          if (fields.last.forall(_ == NoTile(0)))
            if (fields(fields.length-2).forall(_ == NoTile(0))) Nil
            else noTileList :: Nil
          else noTileList :: noTileList :: Nil

        fields ::: padding
      }

      padTop(padBottom(fields))
    }

    Level(padRows(padColumns(fields)))
  }
}
