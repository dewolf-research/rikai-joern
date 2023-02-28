package org.fkie.rikai

import com.typesafe.config.{Config, ConfigFactory}
import io.joern.c2cpg.{C2Cpg, Config => JoernConfig}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays

import scala.util.{Failure, Success}

/** Example program that makes use of Joern as a library
  */
object Main extends App {
  println("Rikai")

  if (args.length < 2) {
    println("Please provide a database name and path to the sample to be analyzed.")
    sys.exit(1)
  }
  if (!new java.io.File(args(1)).exists()) {
    println("File to be analyzed does not exist.")
    sys.exit(2)
  }
  println("Parsing config...")
  val config: Config = ConfigFactory.load("rikai.conf")
  println("Loaded config")
  val db_config = config.getConfig("database")

  println("Connecting to DB ...")
  val db: DatabaseManager = new DatabaseManager(db_config.getString("hostname"), db_config.getInt("port"))

  println("Creating CPG...")
  val joern_config = JoernConfig(inputPaths = Set(args(1)))
  val cpgOrException = new C2Cpg().createCpg(joern_config)

  cpgOrException match {
    case Success(cpg) =>
      applyDefaultOverlays(cpg)
      println("Exporting graph...")
      db.add(args(0), cpg)
    case Failure(exception) =>
      println(exception)
      sys.exit(3)
  }
  println("Closing connection...")
  db.close()
}


