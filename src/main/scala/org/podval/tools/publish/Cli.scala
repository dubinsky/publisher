package org.podval.tools.publish

import org.slf4j.event.Level

object  Cli:
  def main(args: Array[String]): Unit =
    val (optionArgs: List[String], positional: List[String]) = args.toList.partition(_.startsWith("--"))
    val options: List[(String, String)] = optionArgs.map(_.substring(2)).map: string =>
      val eqIndex = string.indexOf('=')
      (string.substring(0, eqIndex), string.substring(eqIndex+1))
    def option(name: String): Option[String] =
      // TODO add retrieval of environment variables
      options.find(_._1 == name).map(_._2)

    Site(
      sourceDirectoryPath = positional(0),
      production = option("production").exists(_.toBoolean),
      targetDirectoryName = option("target-directory-name").getOrElse("_site"),
      includeDrafts = option("include-drafts").exists(_.toBoolean),
      treatErrorsAsWarnings = option("treat-errors-as-warnings").exists(_.toBoolean),
      logLevel = option("log-level").map(option => Level.valueOf(option.toUpperCase)).getOrElse(Level.DEBUG)
    )
      .generate()

