package bloxorz

case class Node(boardState : Level, moves : String, winningState : Boolean, var expanded : Boolean)

case class Graph(var allNodes : List[Node]){
  def nextUnvisitedNode():Node = {
    allNodes.find(_.expanded == false) match {
      case None => throw new Error("Level has no solution.")
      case Some(n) => n
    }
  }
}

object Solver {
  def solve(level : Level) : Unit = {
    def generateGraphFromNode(node: Node) : Graph = {
      def expandGraphFromNode(g : Graph, n : Node) : Graph = {
        def expandNode(n : Node) : List[Node] = {
          val boardU = n.boardState.updateBoardState(Direction('u'))
          val boardR = n.boardState.updateBoardState(Direction('r'))
          val boardD = n.boardState.updateBoardState(Direction('d'))
          val boardL = n.boardState.updateBoardState(Direction('l'))
          val nodeU = Game.checkWinCondition(boardU) match {
            case 0 => Some(Node(boardU, n.moves + 'u', winningState = false, expanded = false))
            case 1 => Some(Node(boardU, n.moves + 'u', winningState = true, expanded = false))
            case -1 => None
          }
          val nodeR = Game.checkWinCondition(boardR) match {
            case 0 => Some(Node(boardR, n.moves + 'r', winningState = false, expanded = false))
            case 1 => Some(Node(boardR, n.moves + 'r', winningState = true, expanded = false))
            case -1 => None
          }
          val nodeD = Game.checkWinCondition(boardD) match {
            case 0 => Some(Node(boardD, n.moves + 'd', winningState = false, expanded = false))
            case 1 => Some(Node(boardD, n.moves + 'd', winningState = true, expanded = false))
            case -1 => None
          }
          val nodeL = Game.checkWinCondition(boardL) match {
            case 0 => Some(Node(boardL, n.moves + 'l', winningState = false, expanded = false))
            case 1 => Some(Node(boardL, n.moves + 'l', winningState = true, expanded = false))
            case -1 => None
          }
          n.expanded = true
          (nodeU :: nodeR :: nodeD :: nodeL :: Nil).flatten
        }
        if(!n.expanded) {
          val expandedNode = expandNode(n)
          val filteredForKnown = expandedNode.filter(newNode => g.allNodes.forall(newNode.boardState != _.boardState))
          g.allNodes = g.allNodes ::: filteredForKnown
          if(filteredForKnown.forall(_.winningState == false)) expandGraphFromNode(g, g.nextUnvisitedNode())
          else g
        }
        else g
      }
      expandGraphFromNode(Graph(List(node)), node)
    }
    val g = generateGraphFromNode(Node(level, "", winningState = false, expanded = false))
    println(g.allNodes.find(_.winningState).get.moves)
  }
}
