package org.fkie.rikai.behavior

trait BaseConstraint {
  def function: StringLiteral

  def parameters: Array[Operand]

  def variables(): Set[Variable] = dependencies()

  def dependencies(): Set[Variable] = {
    parameters.collect { case variable: Variable => variable }.toSet
  }
}

case class Constraint(
                       function: StringLiteral,
                       parameters: Array[Operand]
                     ) extends BaseConstraint {

}

case class DefiningConstraint(
                               function: StringLiteral,
                               parameters: Array[Operand],
                               defines: Variable
                             ) extends BaseConstraint {
  override def variables(): Set[Variable] = {
    super.variables() | Set(defines)
  }
}