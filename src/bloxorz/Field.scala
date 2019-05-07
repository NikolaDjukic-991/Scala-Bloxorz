package bloxorz


abstract class Field(orientation: Int){
  val hasBlock = orientation != 0
}

case class Tile(orientation: Int)        extends Field(orientation)
case class StartingPos(orientation: Int) extends Field(orientation)
case class EndPos(orientation: Int)       extends Field(orientation)
case class NoTile(orientation: Int)      extends Field(orientation)
case class WeakTile(orientation: Int)    extends Field(orientation)
