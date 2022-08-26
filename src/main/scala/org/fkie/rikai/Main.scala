package org.fkie.rikai

import com.typesafe.config.{Config, ConfigFactory}
import io.joern.c2cpg.{C2Cpg, Config => JoernConfig}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Cpg

import scala.util.{Failure, Success}

/** Example program that makes use of Joern as a library
  */
object Main extends App {
  println("Rikai")

  if (args.length == 0) {
    println("Please provide a path to the sample to be analyzed.")
    sys.exit(1)
  }
  if (!new java.io.File(args(0)).exists()) {
    println("File to be analyzed does not exist.")
    sys.exit(2)
  }
  println("Parsing config...")
  val config: Config = ConfigFactory.load("rikai.conf")
  println("Loaded config")
  val db_config = config.getConfig("database")

  println("Fetching rules...")
  val db: Database = new Database(db_config.getString("hostname"), db_config.getInt("port"), db_config.getString("database"))
  val filter = if (args.length > 1) args(1) else null
  val ruledb = db.get_rules(filter)

  println("Creating CPG...")
  val joern_config = JoernConfig(inputPaths = Set(args(0)))
  val cpgOrException = new C2Cpg().createCpg(joern_config)

  cpgOrException match {
    case Success(cpg) =>
      applyDefaultOverlays(cpg)
      analyze(cpg, ruledb)
    case Failure(exception) =>
      println(exception)
      sys.exit(3)
  }



  def analyze(cpg: Cpg, ruledb: RuleDatabase): Unit = {
    println("Matching rules...")
    val matcher = new PatternMatcher(cpg)
    for (db_entry <- ruledb) {
      val pattern = Parser.parse_pattern(db_entry.pattern())
      if (matcher.match_pattern(pattern)) {
        println(db_entry.pattern())
      }
    }
  }
}


