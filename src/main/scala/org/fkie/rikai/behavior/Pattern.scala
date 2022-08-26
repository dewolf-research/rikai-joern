package org.fkie.rikai.behavior

/** Definitions of classes defining behavior rules. */
case class Pattern(constraints: Array[BaseConstraint]) {
  /** Get a set of all variables defined or used in the pattern. */
  def variables(): Set[Variable] = {
    constraints.collect { case constraint: BaseConstraint => constraint.variables() }.flatten.toSet
  }

  /** Get all variables defines in the constraints of the patterns */
  def get_defined_variables(): Array[Variable] = {
    get_definitions().collect { case definition: DefiningConstraint => definition.defines }
  }

  /** Get all variables utilized in the constraints. */
  def get_dependencies(): Set[Variable] = {
    constraints.collect { case constraint: BaseConstraint => constraint.dependencies() }.flatten.toSet
  }

  /** Get all constraints defining variables */
  def get_definitions(): Array[DefiningConstraint] = {
    constraints.collect { case constraint: DefiningConstraint => constraint }
  }

  /** Get the definition of the given variable, if any. */
  def get_definition(variable: Variable): Option[DefiningConstraint] = {
    get_definitions().find(_.defines == variable)
  }

  /** Get all constraints depending on the given variable. */
  def get_dependent_constraints(variable: Variable): Array[BaseConstraint] = {
    constraints.filter { constraint: BaseConstraint => constraint.dependencies() contains variable }
  }

  /** Get a set of all function symbols involved. */
  def get_function_symbols(): Set[StringLiteral] = {
    constraints.collect { case constraint: BaseConstraint => constraint.function }.toSet
  }
}




