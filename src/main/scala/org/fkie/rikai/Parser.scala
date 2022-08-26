package org.fkie.rikai

import org.fkie.rikai.behavior._

/** Methods used to parse behavior patterns in string format. */
object Parser {
  val REGEX_CONSTRAINT = raw"(?:([\w ,]+) = )?([\w@!-_]+)\(([\w\- ,]*)\)".r

  /** Parse a pattern from the given multiline String. */
  def parse_pattern(data: String): Pattern = {
    val lines = data.split('\n')
    val constraints = lines.map(Parser.parse_constraint)
    Pattern(constraints)
  }

  /** Parse a String defining a constraint. */
  def parse_constraint(data: String): BaseConstraint = {
    data match {
      case Parser.REGEX_CONSTRAINT(null, label, parameters) =>
        Constraint(
          StringLiteral(label),
          parameters.split(",").map(Parser.parse_operand)
        )
      case Parser.REGEX_CONSTRAINT(defines, label, parameters) =>
        DefiningConstraint(
          StringLiteral(label),
          parameters.split(",").map(Parser.parse_operand),
          Variable(defines)
        )
      case _ => throw new Exception("Could not parse.")
    }
  }

  /** Parse the String representation of an operand. */
  def parse_operand(data: String): Operand = {
    val identifier = data.strip()
    if (identifier == "_")
      return UnboundOperand()
    if (identifier.startsWith("\""))
      return StringLiteral(identifier)
    try {
      return IntegerLiteral(identifier.toInt)
    } catch {
      case _: java.lang.NumberFormatException => None
    }
    Variable(identifier)
  }

}
