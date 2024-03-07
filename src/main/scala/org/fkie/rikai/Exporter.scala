package org.fkie.rikai

import scala.collection.mutable.Map

import com.vaticle.typeql.lang.TypeQL
import com.vaticle.typedb.driver.api.{TypeDBSession, TypeDBTransaction}

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._
import io.joern.dataflowengineoss.language._
import com.vaticle.typeql.lang.query.TypeQLInsert
import com.vaticle.typeql.lang.pattern.statement.ThingStatement;
import cask.endpoints.get
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes
import scala.collection.immutable.LazyList.cons
import replpp.shaded.mainargs.arg

class Exporter(session: TypeDBSession):

    /**
     * Exports the given CPG (Code Property Graph), printing various statistics and information.
     *
     * @param cpg The CPG to be exported.
     */
    def exportCpg(cpg: Cpg): Unit =
        val conditions = cpg.controlStructure.condition.isCall.l
        val apiCalls = cpg.call.l.filter(call => isApiCall(call))
        println(s"Added ${add(cpg.literal.groupBy(x => x.code).map{(value, literals) => getLiteral(literals.head)})} literals")
        println(s"Added ${add(apiCalls.map(getCall(_)))} API calls...")
        println(s"Added ${add(conditions.map(getCondition(_)))} conditions...")
        println(s"Added ${apiCalls.map(getControlsRelations(_)).sum} relations between calls and conditions...")
        println(s"Added ${apiCalls.map(addParameterRelations(_)).sum} sliced expressions for api calls...")
        println(s"Added ${conditions.map(addParameterRelations(_)).sum} sliced expressions for conditions...")

    /**
     * Adds a collection of ThingStatements to the database.
     *
     * @param things The collection of ThingStatements to be added.
     * @return The number of variables added.
     */
    def add(things: Iterable[ThingStatement[_]]): Int =
        if (things.isEmpty) then return 0
        val transaction = session.transaction(TypeDBTransaction.Type.WRITE)
        val query = TypeQL.insert(things.toSeq*)
        transaction.query().insert(query)
        transaction.commit()
        things.size
        println

    def getControlsRelations

    /**
     * Adds parameter relations for a given call.
     *
     * @param call The call for which parameter relations need to be added.
     * @return The number of parameter relations added.
     */
    def addParameterRelations(call: Call): Int = 
        val transaction = session.transaction(TypeDBTransaction.Type.WRITE)
        var count = 0
        for ((argument, index) <- call.argument.zipWithIndex) do
            for predecessor <- backwardSlice(argument) do
                val query = TypeQL.`match`((call ++ predecessor).map(getSourceObject(_)).toSeq*)
                    .insert(getParameter(call, predecessor, index))
                transaction.query().insert(query)
                count += 1
        transaction.commit()
        count

    /**
     * Retrieves the number of control relations for a given call.
     *
     * @param call The call for which to retrieve the control relations.
     * @return The number of control relations for the call.
     */
    def getControlsRelations(call: Call): Int =
        val transaction = session.transaction(TypeDBTransaction.Type.WRITE)
        for (controller <- call.controlledBy.isCall) do
            val query = TypeQL.`match`((call ++ controller).map(getSourceObject(_)).toSeq*)
                .insert(getControls(call, controller))
            transaction.query().insert(query)
        transaction.commit()
        call.controlledBy.size

    /**
     * This file contains the implementation of the Exporter class, which provides methods for generating TypeQL inserts and retrieving ThingStatements.
     */
    def getInsert(things: Iterator[ThingStatement[_]]): TypeQLInsert =
        TypeQL.insert(things.toSeq*)

    /**
     * Retrieves a ThingStatement representing a parameter of a call.
     *
     * @param call The call containing the parameter.
     * @param parameter The expression representing the parameter.
     * @param index The index of the parameter.
     * @return The ThingStatement representing the parameter.
     */
    def getParameter(call: Call, parameter: Expression, index: Int): ThingStatement[_] =
        TypeQL.cVar(s"${call.id()}-${parameter.id()}").rel("Source", TypeQL.cVar(s"${parameter.id()}")).rel("Sink", TypeQL.cVar(s"${call.id()}")).has("Index", index).isa("Parameter")

    /**
     * Retrieves a ThingStatement representing the control relationship between two calls.
     *
     * @param controlled The call being controlled.
     * @param controller The call controlling the other call.
     * @return The ThingStatement representing the control relationship.
     */
    def getControls(controlled: Call, controller: Call): ThingStatement[_] =
        TypeQL.cVar(s"${controlled.id()}-${controller.id()}").rel("Controller", TypeQL.cVar(s"${controller.id()}")).rel("Controlled",TypeQL.cVar(s"${controlled.id()}")).isa("Controls")

    /**
     * Retrieves a ThingStatement representing a condition or loop.
     *
     * @param condition The call representing the condition or loop.
     * @return The ThingStatement representing the condition or loop.
     */
    def getCondition(condition: Call): ThingStatement[_] =
        val typestr = if ((condition.conditionIn.isFor ++ condition.conditionIn.isWhile).nonEmpty) "Loop" else "Condition"
        TypeQL.cVar(s"${condition.id()}").isa(typestr).has("Line", condition.lineNumber.get.toLong)

    /**
     * Retrieves a ThingStatement representing a call.
     *
     * @param call The call.
     * @return The ThingStatement representing the call.
     */
    def getCall(call: Call): ThingStatement[_] =
        TypeQL.cVar(s"${call.id()}").isa("Call").has("Line", call.lineNumber.get.toLong).has("Label", call.name)

    /**
     * Retrieves a ThingStatement based on the given Literal.
     *
     * @param literal The Literal to retrieve the ThingStatement for.
     * @return The ThingStatement associated with the Literal.   
     * 
     */
    def getLiteral(literal: Literal): ThingStatement[_] = 
        parseInt(literal.code) match
            case Some(n) => TypeQL.cVar(s"${literal.id()}").isa("IntegerLiteral").has("IntegerValue", n)
            case None => TypeQL.cVar(s"${literal.id()}").isa("StringLiteral").has("StringValue", getString(literal))

    /**
     * Generate a ThingStatement describing the given expression.
     *
     * @param expression The expression to be matched.
     * @return The corresponding ThingStatement representing the source object.
     */
    def getSourceObject(expression: Expression): ThingStatement[_] =
        expression match 
            case literal: Literal =>
                return getLiteral(literal)
            case call: Call =>
                call match 
                    case condition if condition.conditionIn.nonEmpty =>
                        return getCondition(condition)
                    case _ =>
                        return getCall(call)
            case _ =>
                throw IllegalArgumentException(s"Unsupported expression type: ${expression.getClass.getName}")

    /*
     * Parses a string into an optional long value.
     *
     * @param s the string to parse
     * @return an optional long value if the string can be parsed, None otherwise
     */
    private def parseInt(s: String): Option[Long] =
        try
            Some(java.lang.Long.decode(s))
        catch 
            case e: NumberFormatException => None

    /**
     * Retrieves the string representation of an expression.
     *
     * @param expr The expression to retrieve the string representation from.
     * @return The string representation of the expression.
     */
    private def getString(expr: Expression): String =
        if (expr.code.startsWith("L\""))
            return expr.code.stripPrefix("L\"").stripSuffix("\"")
        expr.code.stripPrefix("\"").stripSuffix("\"")

    /**
     * Computes the backward slice of an expression.
     *
     * The backward slice of an expression includes all the expressions that directly or indirectly
     * influence the given expression. It starts from the given expression and traverses the expression
     * graph in a backward direction, following the data dependencies.
     *
     * @param head The expression for which to compute the backward slice.
     * @return The set of expressions that make up the backward slice.
     */
    private def backwardSlice(head: Expression): Set[Expression] =
        var todo: Set[Expression] = Set(head)
        var visited: Set[Expression] = Set(head)
        var slice: Set[Expression] = Set()
        while (todo.nonEmpty)
            val head = todo.last
            var parents: Set[Expression] = Set()
            visited += head
            head match
                case literal: Literal =>
                    slice += literal
                case identifier: Identifier =>
                    parents = identifier.ddgIn.isExpression.toSet
                case call: Call =>
                    call match
                        case apiCall if isApiCall(apiCall) && !visited.contains(call) =>
                            slice += apiCall
                        case _ =>
                            parents = call.argument.isExpression.toSet
                case _ =>
                    //println(s"Unsupported expression type: ${head.getClass.getName} ${head.code}")
                    ()
            todo = todo.init ++ parents -- visited
        slice

    /**
     * Checks if the given call is an API call.
     *
     * @param call The call to check.
     * @return `true` if the call is an API call, `false` otherwise.
     */
    def isApiCall(call: Call): Boolean = 
        return !call.name.startsWith("<operator>")

end Exporter