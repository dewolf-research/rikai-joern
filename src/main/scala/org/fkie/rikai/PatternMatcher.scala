package org.fkie.rikai

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._
import io.joern.dataflowengineoss.language._
import io.joern.joerncli.console.Joern.semantics
import org.fkie.rikai.behavior.{BaseConstraint, DefiningConstraint, Operand, Pattern, Variable, Literal => RikaiLiteral}

import scala.collection.mutable

/** Class utilized to match patterns on the current cpg. */
class PatternMatcher(cpg: Cpg) {
  val value_groups: Map[Long, Long] = init_value_groups()

  /** Create a directory of all ids of identifiers which may represent the same value. */
  def init_value_groups(): Map[Long, Long] = {
    val identifier_neighborhood: Map[Long, mutable.Set[Long]] = cpg.identifier.map { x: Identifier => x.id -> x.ddgIn.id.toSetMutable }.toMap
    val identifier_to_group: mutable.HashMap[Long, Long] = mutable.HashMap()
    for ((identifier: Long, parents: mutable.Set[Long]) <- identifier_neighborhood) {
      parents.map(identifier_neighborhood(_).add(identifier))
    }
    var todo = identifier_neighborhood.keySet
    var n = 0
    while (todo.nonEmpty) {
      val group = weakly_connected_components(todo.last, identifier_neighborhood)
      group.map(identifier_to_group.put(_, n))
      todo = todo -- group
      n = n + 1
    }
    identifier_to_group.toMap
  }

  /** Return the set of nodes contained in the same weakly connected component as the head node. */
  def weakly_connected_components(head: Long, neighborhoods: Map[Long, mutable.Set[Long]]): Set[Long] = {
    val visited: mutable.Set[Long] = mutable.Set()
    val todo = mutable.Stack(head)
    while (todo.nonEmpty) {
      val current_node = todo.pop()
      visited.add(current_node)
      todo.pushAll(neighborhoods(current_node).filter(!visited.contains(_)))
    }
    visited.toSet
  }

  /** Check whether the pattern can be found in the current cpg. */
  def match_pattern(pattern: Pattern): Boolean = {
    val constraint_candidates: mutable.Map[BaseConstraint, List[Call]] = mutable.Map()
    for (constraint <- pattern.constraints) {
      val calls = get_constraint_candidates(constraint)
      if (calls.isEmpty)
        return false
      constraint_candidates.update(constraint, calls)
    }
    find_variable_mapping(constraint_candidates)
  }

  /** Return a list of calls as potential candidates for the given constraint. */
  def get_constraint_candidates(constraint: BaseConstraint): List[Call] = {
    cpg.call.name(constraint.function.value).l.filter(call => check_call_constants(call, constraint.parameters))
  }

  /** Check if the given call contains all constants contained in the parameters. */
  def check_call_constants(call: Call, parameters: Array[Operand]): Boolean = {
    val arguments = call.argument.l
    for ((parameter, i) <- parameters.zipWithIndex)
      if (parameter.isInstanceOf[RikaiLiteral] && arguments(i).code != parameter.toString)
        return false
    true
  }

  /** Try to find a mapping of variables between constraints and the mapped calls satisfying the pattern. */
  def find_variable_mapping(constraint_candidates: mutable.Map[BaseConstraint, List[Call]]): Boolean = {
    val fixed_constraints = constraint_candidates.filter(x => x._2.length == 1)
    fixed_constraints.keys.map(_ => constraint_candidates.remove(_))
    if (!check_constraints(fixed_constraints))
      return false
    if (constraint_candidates.isEmpty)
      return true
    for (combination <- combs(constraint_candidates.values.toList)) {
      val candidates = constraint_candidates.clone()
      for ((constraint, candidate) <- constraint_candidates.keys.zip(combination))
        candidates.update(constraint, List(candidate))
      if (check_constraints(candidates))
        return true
    }
    false
  }

  /** Return all combinations of the elements contained in a list of lists. */
  def combs[A](xss: List[List[A]]): List[List[A]] = xss match {
    case Nil => List(Nil)
    case xs :: rss => for (x <- xs; cs <- combs(rss)) yield x :: cs
  }

  /** Check if the calls mapped to the constraints are compatible with each other. */
  def check_constraints(mapping: mutable.Map[BaseConstraint, List[Call]]): Boolean = {
    var variable_mapping: Map[Variable, Set[Expression]] = mapping.map { case (k, v) => get_variable_mapping(k, v.last) }.flatten.groupMapReduce(_._1)(_._2)(_ ++ _)
    val variable_to_definition = mapping.collect { case (constraint: DefiningConstraint, calls: List[Call]) => (constraint.defines, calls.last) }
    for ((variable, expressions) <- variable_mapping) {
      if (variable_to_definition.contains(variable)) {
        val return_value = get_return_value(variable_to_definition(variable))
        if (return_value == null)
          return false
        variable_mapping = variable_mapping.updated(variable, variable_mapping(variable) ++ Set(return_value))
      }
      if (!check_if_usages_have_a_common_predecessor(expressions))
        return false
    }
    true
  }

  /** Map the variables in the given constraint to the call parameters. */
  def get_variable_mapping(constraint: BaseConstraint, call: Call): Map[Variable, Set[Expression]] = {
    constraint.parameters.zip(call.argument.l).collect { case (variable: Variable, expression) => (variable, expression) }.groupBy(_._1).view.mapValues(_.map(_._2).toSet).toMap
  }

  /** Degenerated Function. Checks if the actual names of all expressions are the same. */
  def check_if_usages_have_a_common_predecessor(expressions: Set[Expression]): Boolean = {
    expressions.collect { expr: Expression => value_groups(expr.id)}.size == 1
  }

  /** Get the return value of the given function, if any. */
  def get_return_value(call: Call): Expression = {
    call.parentExpression.isCall.astChildren.order(1).isExpression.l.applyOrElse(0, null)
  }
}
