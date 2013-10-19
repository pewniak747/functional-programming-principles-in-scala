package natural

class NoSuchNumber extends Error

abstract class Natural {
  def isZero: Boolean
  def predecessor: Natural
  def successor = new Successor(this)
  def +(that: Natural): Natural
  def -(that: Natural): Natural
  def ==(that: Natural): Boolean
}

class Zero extends Natural {
  def isZero = true
  def predecessor = throw new NoSuchNumber
  def +(that: Natural) = that
  def -(that: Natural) =
    if (that.isZero)
      this
    else
      throw new NoSuchNumber
  def ==(that: Natural) = that.isZero
  override def toString = "zero"
}

class Successor(val predecessor: Natural) extends Natural {
  def isZero = false
  def +(that: Natural) = new Successor(predecessor + that)
  def -(that: Natural) =
    if (that.isZero)
      this
    else
      predecessor - that.predecessor
  def ==(that: Natural) = !that.isZero && that.predecessor == predecessor
  override def toString = "successor of (" + predecessor + ")"
}
