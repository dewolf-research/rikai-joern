package org.fkie.rikai
import com.vaticle.typedb.client.api.{TypeDBSession, TypeDBTransaction}
import com.vaticle.typeql.lang.TypeQL
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._


trait JoernExport {
  this: DatabaseManager =>

  // Extract the given cpg as a database
  def add(name: String, cpg: Cpg): Unit = {
    create(name)
    val session: TypeDBSession = client.session(name, TypeDBSession.Type.DATA)
    extract_vertices(cpg, session)
    val transaction = session.transaction(TypeDBTransaction.Type.WRITE)

    for (call <- cpg.call.filter(is_api_call)) {
      for (argument <- call.argument) {
        for (predecessor <- backward_slice(cpg, call, argument)) {
          if (predecessor.isLiteral)
            add_source_literal(transaction, call, argument, predecessor)
          if (predecessor.isCall) {
            val parent_call = cpg.call.id(predecessor.id()).head
            extract_source_call(transaction, call, argument, parent_call)
          }
        }
      }
    }

    transaction.commit()
    session.close()
  }

  // Create a backward slice fron the given argument containing literals and calls
  private def backward_slice(cpg: Cpg, call: Call, argument: Expression): Set[Expression] = {
    var todo: Set[Expression] = Set(argument.expr)
    var visited: Set[Expression] = Set(call)
    var slice: Set[Expression] = Set()
    while (todo.nonEmpty) {
      val head = todo.last
      var parents: Set[Expression] = Set()

      visited += head
      if (head.isLiteral) {
        slice = slice + head
      }
      if (head.isIdentifier) {
        parents = get_definitions(cpg, head.code)
      } else if (head.isCall) {
        val parent_call = cpg.call.id(head.id()).head
        if (is_api_call(parent_call)) {
          slice = slice + parent_call
        } else {
          parents = parent_call.argument.isExpression.toSet
        }
      }
      todo = todo.init ++ parents -- visited
    }
    slice
  }

  // Extract the given call->call relation
  private def extract_source_call(transaction: TypeDBTransaction, call: Call, argument: Expression, parent_call: Call) = {
    val relation = TypeQL
      .`match`(
        TypeQL
          .`var`("parent")
          .isa("Call")
          .has("Line", parent_call.lineNumber.get.toLong)
          .has("Label", parent_call.name),
        TypeQL.`var`("call").isa("Call").has("Line", call.lineNumber.get.toLong).has("Label", call.name)
      )
      .insert(
        TypeQL
          .`var`(s"param")
          .rel("Source", "parent")
          .rel("Sink", s"call")
          .isa("Parameter")
          .has("Index", argument.argumentIndex)
      )
    transaction.query().insert(relation)
  }

  // Extract the given literal->call relation
  private def add_source_literal(transaction: TypeDBTransaction, call: Call, argument: Expression, head: Expression) = {
    val relation = TypeQL
      .`match`(
        TypeQL.`var`("value").isa("Literal").has("StringValue", head.code),
        TypeQL.`var`("call").isa("Call").has("Line", call.lineNumber.get.toLong).has("Label", call.name)
      )
      .insert(
        TypeQL
          .`var`(s"param")
          .rel("Source", "value")
          .rel("Sink", s"call")
          .isa("Parameter")
          .has("Index", argument.argumentIndex)
      )
    transaction.query().insert(relation)
  }

  // Extract Function Calls and Literals
  private def extract_vertices(cpg: Cpg, session: TypeDBSession): Unit = {
    val transaction: TypeDBTransaction = session.transaction(TypeDBTransaction.Type.WRITE)
    println("Creating call nodes...")
    for (call <- cpg.call.filter(is_api_call)) {
      val query =
        TypeQL.insert(TypeQL.`var`(s"call").isa("Call").has("Line", call.lineNumber.get.toLong).has("Label", call.name))
      transaction.query().insert(query)
    }
    println("Creating literal nodes...")
    for (literal <- cpg.literal) {
      val query = TypeQL.insert(TypeQL.`var`(s"literal").isa("Literal").has("StringValue", literal.code))
      transaction.query().insert(query)
    }
    transaction.commit()
  }

  // Get the definitions of the given identifier name
  def get_definitions(cpg: Cpg, name: String): Set[Expression] = {
    cpg.assignment.where(_.argument(1).code(name)).toSet
  }

  // Check if the given call is an api call
  def is_api_call(call: Call): Boolean = {
    if (call.name.startsWith("<operator>"))
      return false
    true
  }
}
