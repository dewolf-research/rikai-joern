package org.fkie.rikai.behavior

abstract class Operand() {
  override def toString: String = ""
}

abstract class Literal() extends Operand

case class UnboundOperand() extends Operand {
  override def toString: String = "_"
}

case class Variable(name: String) extends Operand {
  override def toString: String = name
}

case class StringLiteral(value: String) extends Literal {
  override def toString: String = value
}

case class IntegerLiteral(value: Int) extends Literal {
  override def toString: String = value.toString
}