package org.fkie.rikai

import com.rethinkdb.RethinkDB
import com.rethinkdb.gen.ast.ReqlExpr
import com.rethinkdb.net.{Connection, Result}

import scala.jdk.CollectionConverters._


case class Rule(data: AnyRef) {
  def tags(): Seq[String] = {
    val hashmap = data.asInstanceOf[java.util.LinkedHashMap[String, java.util.ArrayList[String]]]
    hashmap.get("tags").asScala.toSeq
  }
  def pattern(): String = {
    data.asInstanceOf[java.util.LinkedHashMap[String, String]].get("rule")
  }
}


case class RuleDatabase(cursor: Result[AnyRef]) extends Iterable[Rule] {
  override def iterator: Iterator[Rule] = new Iterator[Rule] {
    override def hasNext: Boolean = cursor.hasNext
    override def next(): Rule = Rule(cursor.next())
  }

}


class Database(hostname: String, port: Int, name: String) extends AutoCloseable {
  val r: RethinkDB = RethinkDB.r
  val connection: Connection = r.connection.hostname(hostname).port(port).db(name).connect

  /** Return a cursor with rules from the database using the given filter. */
  def get_rules(filter: String = null): RuleDatabase = {
    var rules: ReqlExpr = r.table("rules")
    if (filter != null)
      rules = rules.filter{row: ReqlExpr => row.g("tags").contains(filter)}
    RuleDatabase(rules.run(connection))
  }

  def close(): Unit = {
    connection.close()
  }
}
