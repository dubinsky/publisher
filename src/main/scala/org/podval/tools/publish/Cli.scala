package org.podval.tools.publish

import org.slf4j.event.Level

object Cli:
  def main(args: Array[String]): Unit =
    Site(
      sourceDirectoryPath = "/home/dub/Podval/dub.podval.org",
      treatErrorsAsWarnings = false,
      logLevel = Level.INFO
    )
      .generateAndReport()
