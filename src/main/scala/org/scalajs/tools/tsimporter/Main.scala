/* TypeScript importer for Scala.js
 * Copyright 2013-2014 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package org.scalajs.tools.tsimporter

import java.io.{Console => _, Reader => _, _}
import java.nio.file.{Files, Path, Paths}

import scala.collection.immutable.PagedSeq
import scala.collection.JavaConverters._
import Trees._

import scala.util.parsing.input._
import parser.TSDefParser

/** Entry point for the TypeScript importer of Scala.js */
object Main {
  def main(args: Array[String]) {
    if (args.length < 2) {
      Console.err.println("""
        |Usage: scalajs-ts-importer <input.d.ts> <output.scala> [<package>]
        |  <input.d.ts>     TypeScript type definition file to read
        |  <output.scala>   Output Scala.js file
        |  <package>        Package name for the output (defaults to "importedjs")
      """.stripMargin.trim)
      System.exit(1)
    }

    val inputPath = Paths.get(args(0))
    val outputPath = Paths.get(args(1))
    val outputPackage = if (args.length > 2) args(2) else "importedjs"

    if (Files.isRegularFile(inputPath))
      importTsFile(inputPath, outputPath, outputPackage) match {
        case Right(()) =>
          ()
        case Left(message) =>
          Console.err.println(message)
          System.exit(2)
      }
    else if (Files.isDirectory(inputPath) && Files.isDirectory(outputPath))
      processDir(inputPath, outputPath)
    else Console.err.println("ERROR")
}

  def importTsFile(inputPath: Path, outputPath: Path, outputPackage: String): Either[String, Unit] = {
    parseDefinitions(readerForFile(inputPath)).map { definitions =>
      val output = new PrintWriter(new BufferedWriter(
          new FileWriter(outputPath.toFile)))
      try {
        process(definitions, output, outputPackage)
        Right(())
      } finally {
        output.close()
      }
    }
  }

  private def processDir(inputDir: Path, outputDir: Path): Unit = {
    val inputFiles = Files.walk(inputDir.toAbsolutePath)
      .filter(Files.isRegularFile(_))
      .filter(_.toString.endsWith(".d.ts"))
      .iterator().asScala.toVector
    for (inputFile <- inputFiles) {
      val outputFile = outputDir
        .resolve(inputDir.toAbsolutePath.relativize(inputFile)) // replace inputDir with outputDir
        .resolveSibling(inputFile.getFileName.toString.dropRight("d.ts".length) + "scala")  // replace file extension
      if (!Files.exists(outputFile.getParent)) Files.createDirectories(outputFile.getParent)
      println("importing " + inputFile)
      if (!Files.exists(outputFile)) importTsFile(inputFile, outputFile, "importedjs") // TODO package
    }
  }

  private def process(definitions: List[DeclTree], output: PrintWriter,
      outputPackage: String) {
    new Importer(output)(definitions, outputPackage)
  }

  private def parseDefinitions(reader: Reader[Char]): Either[String, List[DeclTree]] = {
    val parser = new TSDefParser
    parser.parseDefinitions(reader) match {
      case parser.Success(rawCode, _) =>
        Right(rawCode)

      case parser.NoSuccess(msg, next) =>
        Left(
            "Parse error at %s\n".format(next.pos.toString) +
            msg + "\n" +
            next.pos.longString)
    }
  }

  /** Builds a [[scala.util.parsing.input.PagedSeqReader]] for a file
   *
   *  @param fileName name of the file to be read
   */
  private def readerForFile(fileName: Path) = {
    new PagedSeqReader(PagedSeq.fromReader(
        new BufferedReader(new FileReader(fileName.toFile))))
  }
}
