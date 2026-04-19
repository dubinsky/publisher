package org.podval.tools.publish

import java.io.File
import java.net.URI
import java.nio.file.{Paths, StandardCopyOption, StandardOpenOption, Files as NFiles, Path as NPath}
import scala.jdk.CollectionConverters.ListHasAsScala

object Files:
  def requireExists(file: File): Unit = require(file.exists, s"File does not exist: $file")

  def requireDirectory(file: File): Unit = require(file.isDirectory, s"File is not a directory: $file")

  def requireFile(file: File): Unit = require(file.isFile, s"File is a directory: $file")

  def list(directory: File): List[File] = Option(directory.listFiles).getOrElse(Array.empty[File]).toList

  def split(what: String, on: Char): (String, Option[String]) = what.lastIndexOf(on) match
    case -1 => (what, None)
    case index => (what.substring(0, index), Some(what.substring(index + 1)))

  def nameAndExtension(fullName: String): (String, Option[String]) = split(fullName, '.')

  def read(file: File): String = new String(NFiles.readAllBytes(file.toPath))

  def write(toFile: File, content: String): Unit =
    toFile.getParentFile.mkdirs()
    NFiles.writeString(toFile.toPath, content, StandardOpenOption.CREATE)
  
  def copy(fromFile: File, toFile: File): Unit =
    requireExists(fromFile)
    requireFile(fromFile)
    toFile.getParentFile.mkdirs()
    NFiles.copy(fromFile.toPath, toFile.toPath, StandardCopyOption.REPLACE_EXISTING)

  def main(args: Array[String]): Unit =
    val base: String = "org/podval/tools/publish/site"
    val basePath: NPath = Paths.get(getClass.getClassLoader.getResource(base).toURI)
    val resources: List[URI] = NFiles.walk(basePath).toList.asScala.toList.map(_.toUri).flatMap(uri =>
            println(uri.toString)
            Some(uri)
    )


