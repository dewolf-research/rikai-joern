package org.fkie.rikai

import com.typesafe.config.{Config, ConfigFactory}
import io.joern.c2cpg.{C2Cpg, Config => JoernConfig}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays

import scala.util.{Failure, Success}
import io.shiftleft.codepropertygraph.Cpg
import scala.util.Try
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

object Main:
  def main(args: Array[String]): Unit =
    println("Rikai")

    if (args.length < 2) then
      println("Please provide a database name and path to the sample to be analyzed.")
      sys.exit(1)
    if (!new java.io.File(args(1)).exists()) then
      println("File to be analyzed does not exist.")
      sys.exit(2)

    println("Parsing config...")
    val config: Config = ConfigFactory.load("rikai.conf")
    println("Loaded config")
    val db_config: Config = config.getConfig("database")

    println("Connecting to DB ...")
    val hostname: String = sys.env.getOrElse("RIKAI_DBHOST", db_config.getString("hostname"))
    val db: DatabaseManager = new DatabaseManager(hostname, db_config.getInt("port"))
    db.create(args(0))

    println("Creating CPG...")
    val defaultConfig: JoernConfig = JoernConfig(includePaths = Set(args(1))).withInputPath(args(1))
    val cpgOrException: Try[Cpg] = new C2Cpg().createCpg(defaultConfig)

    cpgOrException match
      case Success(cpg) =>
        println("Applying Overlays...")
        applyDefaultOverlays(cpg)
        println("Tracking dataflow...")
        new OssDataFlow(new OssDataFlowOptions()).run(new LayerCreatorContext(cpg))
        println("Exporting graph...")
        val exporter = Exporter(db.getSession(args(0)))
        exporter.exportCpg(cpg)
      case Failure(exception) =>
        println(exception)
        sys.exit(3)

    println("Closing connection...")
    db.close()
