package org.podval.tools.publish

final class Errors(site: Site):
  private var errorsVar: List[PageError] = List.empty
//  def errors: List[PageError] = errorsVar

  def warning(error: PageError): Unit =
    errorsVar = errorsVar.appended(error)
    site.log.warn(error.getMessage)

  def error(error: PageError): Unit =
    if site.treatErrorsAsWarnings
    then warning(error)
    else throw error
