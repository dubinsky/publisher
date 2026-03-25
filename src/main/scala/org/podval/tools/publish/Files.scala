package org.podval.tools.publish

import java.io.File
import java.nio.file.{Files as NFiles, StandardCopyOption}

object Files:
  def requireExists(file: File): Unit = require(file.exists, s"File does not exist: $file")

  def requireDirectory(file: File): Unit = require(file.isDirectory, s"File is not a directory: $file")

  def requireFile(file: File): Unit = require(file.isFile, s"File is a directory: $file")

  def read(file: File): String = new String(NFiles.readAllBytes(file.toPath))

  def write(string: String, file: File): Unit = NFiles.writeString(file.toPath, string)

  def list(directory: File): List[File] = Option(directory.listFiles).getOrElse(Array.empty[File]).toList

  // TODO Strings
  def split(what: String, on: Char): (String, Option[String]) = what.lastIndexOf(on) match
    case -1 => (what, None)
    case index => (what.substring(0, index), Some(what.substring(index + 1)))

  def nameAndExtension(fullName: String): (String, Option[String]) = split(fullName, '.')

  def copy(fromFile: File, toFile: File): Unit =
    requireExists(fromFile)
    requireFile(fromFile)
    toFile.getParentFile.mkdirs()
    NFiles.copy(fromFile.toPath, toFile.toPath, StandardCopyOption.REPLACE_EXISTING)
