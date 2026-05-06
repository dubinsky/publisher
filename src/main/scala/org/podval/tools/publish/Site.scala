package org.podval.tools.publish

import zio.blocks.schema.xml.{Xml, XmlName}
import org.slf4j.{Logger, LoggerFactory}
import org.slf4j.event.Level
import scala.reflect.{ClassTag, TypeTest}
import java.io.File
import java.net.{URI, URISyntaxException}
import XmlUtil.{a, div, withText}

final class Site(
  sourceDirectoryPath: String,
  production: Boolean,
  targetDirectoryName: String,
  includeDrafts: Boolean,
  treatErrorsAsWarnings: Boolean,
  logLevel: Level = Level.INFO
):
  private given CanEqual[File, File] = CanEqual.derived
  private given CanEqual[XmlName, XmlName] = CanEqual.derived

  Logging.configureLogBack(level = logLevel, useLogStash = false)
  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  private val config: Config = Config(
    sourceDirectoryPath,
    targetDirectoryName,
    includeDrafts
  )

  def sourceDirectory: File = config.sourceDirectory
  def targetDirectory: File = config.targetDirectory
  def title: String = config.title
  def description: String = config.description
  def url: String = config.url
  def author: String = config.author
  def email: String = config.email
  def lang: String = config.lang.getOrElse("en")

  private var pagesVar: List[Page] = List.empty
  def pages: List[Page] = pagesVar
  def markupPages: List[MarkupPage] = pages.collect { case page: MarkupPage => page }

  def getOrElse[P <: MarkupPage](path: Path)(make: => P)(using TypeTest[MarkupPage, P]): P = markupPages
    .find(_.path == path)
    .map {
      case page: P => page
      case _ => throw IllegalArgumentException(s"Not a ???") // TODO use some tag to print the class name
    }
    .getOrElse:
      val page = make
      log.warn(s"Added $page")
      pagesVar = pagesVar.appended(page)
      page

  private val pageMakers: Seq[MarkupPage.Maker[?]] = Seq(
    Post.Maker(
      site = this,
      postsDirectoryName = config.postsDirectoryName,
      draftsDirectoryName = config.draftsDirectoryName,
      dailyNotesDirectoryName = config.dailyNotesDirectoryName
    ),
    Directory.Maker,
    Tags.Maker,
    Errors.Maker,
    Posts.Maker,
    MarkupPage.DefaultMaker
  )

  private var errorsVar: List[PageError] = List.empty
  def errors: List[PageError] = errorsVar

  // TODO always return None
  def reportError[R](error: PageError, result: R): R =
    if !treatErrorsAsWarnings then throw error else
      errorsVar = errorsVar.appended(error)
      log.warn(error.getMessage)
      result

  def generateAndReport(): Unit =
    try
      generate()
      println("Done!")
    catch case error: PageError =>
      println(s"Error generating site: ${error.getMessage}")

  private def generate(): Unit =
    // Wipe out output directory
    Files.deleteDirectory(config.targetDirectory)

    // Write embedded resources
    writePages(Site.resourcesList.map(Asset.Embedded(this, _)))

    // Write synthetic assets
    writePages(List(Sitemap(this), Robots(this), Feed(this)))

    // Read and add pages
    pagesVar = pagesVar.appendedAll(directoryPages(Seq.empty, config.sourceDirectory))

    // Add automatic pages
    pageMakers.collect { case autoMaker: MarkupPage.AutoMaker[?] => autoMaker }.foreach(_.get(this))

    // Report conflicting pages
    pages
      .groupBy(_.path)
      .filter(_._2.length > 1)
      .toList
      .foreach((path: Path, pages: List[Page]) =>
        reportError(PageError.Duplicate(
          path,
          s"Duplicates for the path $path: ${pages.map(_.title).tail.mkString(", ")}"
        ), ())
      )

    // Add directory pages
    Directory.addParentDirectories(this)

    // Resolve links
    markupPages.foreach(_.resolveLinks())

    // Write pages
    writePages(pages)

  private def directoryPages(directoryPath: Seq[String], directory: File): List[Page] =
    Files.requireExists(directory)
    Files.requireDirectory(directory)

    val (files: List[File], directories: List[File]) = Files
      .list(directory)
      .filterNot(config.exclude)
      .partition(_.isFile)

    files.map(filePage(directoryPath, _)) ++
    directories.flatMap(directory => directoryPages(directoryPath :+ directory.getName, directory))

  private def filePage(directoryPath: Seq[String], file: File): Page =
    Files.requireExists(file)
    Files.requireFile(file)
    val (name: String, extension: Option[String]) = Files.nameAndExtension(file.getName)
    val sourcePath: Path = Path(directoryPath :+ name, extension)

    val page: Page = sourcePath.extension.flatMap(extension => Markup.all.find(_.isExtension(extension))) match
      case None => Asset.WithSource(site = this, path = sourcePath)

      case Some(markup) =>
        val (frontMatterOrError: Either[PageError, FrontMatter], markupContent: String) =
          FrontMatter.parse(sourcePath, Files.read(sourcePath.file(sourceDirectory)))

        val frontMatter: FrontMatter = frontMatterOrError match
          case Right(frontMatter) => frontMatter
          case Left(error) => reportError(error, FrontMatter.absent)

        val xml: Xml.Element = markup.parse(sourcePath, markupContent) match
          case Right(xml) => xml
          case Left(error) => reportError(error, div("malformed-xml").withText(s"Malformed XML: $error"))

        val pageMarkup: PageMarkup = PageMarkup(sourcePath, markup, xml)

        pageMakers
          .flatMap(_.withSource(this, frontMatter, pageMarkup))
          .headOption
          .getOrElse(throw PageError.Unmakable(sourcePath, s"Can't make the page!"))

    log.debug(s"Read: $page")
    page

  private def writePages(pages: List[Page]): Unit = pages.foreach: page =>
    page.write()
    log.debug(s"Wrote: $page")

  lazy val tags: Tags = Tags.Maker.get(this)

  private var linksResolved: List[Link.Resolved] = List.empty

  def backLinks(page: Page): List[Link.Resolved] = linksResolved
    .filter(_.to.page == page)
    .filterNot(_.from.page == page)
    .distinctBy(_.from.page.path) // TODO once we have context, each link should be listed (grouped by page)

  val socialLinks: Seq[SocialLink] = Seq(
    config.social.github.map(SocialLink.GitHub(_)),
    config.social.twitter.map(SocialLink.Twitter(_)),
    config.social.linkedin.map(SocialLink.LinkedIn(_))
  ).flatten

  lazy val headerPages: List[Page.Link] = config.headerPages.flatMap: ref =>
    val result: Option[Page.Link] = resolveRef(ref)
    if result.isDefined
    then result
    else reportError(PageError.Unresolved(Path.root, s"header link ref='$ref'"), result)

  // TODO mark errors with class attribute
  def resolveLink(link: Link): Xml.Element =
    def unresolved = link.element match
      case Some(element) => element
      case None => a("wiki-link", link.ref).withText(link.text.getOrElse(link.ref))
    // TODO can not transclude external links
    (if !link.transclude then None else Site.embedLink(link.ref, link.text)).getOrElse:
      if externalRef(link.ref).isDefined then unresolved
      else resolveRef(link.ref) match
        case None =>
          reportError(
            PageError.Unresolved(link.page.path, s"internal link ref='${link.ref}' text='${link.text.getOrElse("")}'"),
            unresolved
          )
        case Some(linkTo) =>
          // Register resolved link
          linksResolved = linksResolved.appended(Link.Resolved(link, linkTo))

          if link.transclude then linkTo.a("transclude") else link.element match
            case None => linkTo.a("wiki-link")
            case Some(element) => linkTo.a(element)

  private def resolveRef(refString: String): Option[Page.Link] =
    val ref: Page.Ref = Page.Ref(refString)
    if ref.path.path.isEmpty || ref.path.path.exists(_.isEmpty)
    then None
    else pages.find(_.is(ref.path, ref.isAbsolute)).flatMap(_.resolveRef(ref.fragment))

  private def externalRef(ref: String): Option[URI] =
    if ref.contains(" ") then None else
      try
        val uri: URI = URI(ref)
        // TODO recognize and resolve links to *this* site
        Option.when(uri.getScheme != null)(uri)
      catch
        case e: URISyntaxException =>
          // TODO handle errors better - and log them
          log.warn(s"Malformed URL: $ref $e")
          None

object Site:
  def main(args: Array[String]): Unit = Cli.main(Array(
    "--log-level=INFO",
    "/home/dub/Podval/dub.podval.org"
  ))

  // TODO list using Files.listResources
  val resourcesBase: String = "/org/podval/tools/publish/site"
  private val resourcesList: List[Path] = List(
    Path("assets", "css", "base").withExtension("css"),
    Path("assets", "css",  "initialize").withExtension("css"),
    Path("assets", "css",  "layout").withExtension("css"),
    Path("assets", "css",  "skin").withExtension("css"),
    Path("assets", "css",  "style").withExtension("css"),
  )

  // see https://obsidian.md/help/embeds
  // TODO FlexMark inlines image links for the ![]() references - but does not process image sizes...
  private def embedLink(ref: String, text: Option[String]): Option[Xml.Element] =
    Files.nameAndExtension(ref)._2.flatMap: extension =>
      if Files.imageExtensions.contains(extension) then None
      // Embed image
      //      else if Files.audioExtensions.contains(extension) then
      //        // Embed audio player
      //      else if extension == "pdf" then
      //        // Embed PDF viewer
      else None
