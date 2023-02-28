package org.fkie.rikai

import com.vaticle.typedb.client.TypeDB
import com.vaticle.typedb.client.api.{TypeDBClient, TypeDBSession, TypeDBTransaction}

import java.io.{BufferedReader, InputStreamReader}
import java.util.stream.Collectors


class DatabaseManager(hostname: String, port: Int) extends AutoCloseable with JoernExport {
  val client: TypeDBClient = TypeDB.coreClient(s"$hostname:$port")
  val schema: String = get_schema()

  // Create a new database with the given name and the default scheme.
  def create(name: String): Unit = {
    if (client.databases().contains(name)) {
      client.databases().get(name).delete()
    }
    client.databases().create(name)
    val session: TypeDBSession = client.session(name, TypeDBSession.Type.SCHEMA)
    val transaction: TypeDBTransaction = session.transaction(TypeDBTransaction.Type.WRITE)

    transaction.query().define(schema)
    transaction.commit()
    session.close()
  }

  // Close the database connection when the class is deconstructed.
  def close(): Unit = {
    client.close()
  }

  // Read the default schema from the resource file
  def get_schema(): String = {
    val resource = getClass.getClassLoader.getResourceAsStream("schema.tql")
    val isr = new InputStreamReader(resource)
    val reader = new BufferedReader(isr)
    reader.lines().collect(Collectors.joining("\n"))
  }
}
