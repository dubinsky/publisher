package org.podval.tools.publish

import zio.blocks.schema.xml.{Xml, XmlBuilder}

object Layout:
  def el(name: String, attrs: (String, String)*): XmlBuilder.ElementBuilder =
    attrs.foldLeft(XmlBuilder.element(name))((result, attr) => result.attr(attr._1, attr._2))

  extension (builder: XmlBuilder.ElementBuilder)
    def apply(children: Xml*): Xml.Element = builder.children(children *).build
    def apply(text: String): Xml.Element = builder.child(XmlBuilder.text(text)).build

// TODO add backlinks
// TODO calculate based on PageKind?
// TODO icons!
final class Layout(
  config: Config,
  page: Page.MarkupPage,
  backLinks: List[Link]
):
  import Layout.{apply, el}

  // TODO make Highlights conditional?
  private val libraries: List[JavascriptLibrary] = List(
    JavascriptLibrary.Highlights
  ) ++ Option.when(page.frontMatter.math)(
    JavascriptLibrary.MathJax
  ).toList

  def render: Xml =
    el("html", "lang" -> page.frontMatter.lang.orElse(config.lang).getOrElse("en"))(
      el("head")
        .child(el("title")(page.title))
        .children(libraries.flatMap(_.head) *)
        .child(el("link", "rel" -> "stylesheet", "href" -> "/assets/css/style.css").build)
        .build,
      el("body")
        .child(header)
        .child(main)
        .child(footer)
        .children(libraries.flatMap(_.body) *)
        .build
    )

  private def header: Xml.Element =
    el("header", "class" -> "site-header", "role" -> "banner")(
      el("div", "class" -> "wrapper")(
        el("a", "class" -> "site-title", "rel" -> "author", "href" -> "/")(config.title),
        nav
      )
    )

  private def nav: Xml.Element =
    el("nav", "class" -> "site-nav")(
      el("input", "class" -> "nav-trigger", "type" -> "checkbox", "id" -> "nav-trigger")(),
      el("label", "for" -> "nav-trigger")(
        el("span", "class" -> "menu-icon")(
          el("svg", "viewBox" -> "0 0 18 15", "width" -> "18px", "height" -> "15px")(
            el("path", "d" -> "M18,1.484c0,0.82-0.665,1.484-1.484,1.484H1.484C0.665,2.969,0,2.304,0,1.484l0,0C0,0.665,0.665,0,1.484,0 h15.032C17.335,0,18,0.665,18,1.484L18,1.484z M18,7.516C18,8.335,17.335,9,16.516,9H1.484C0.665,9,0,8.335,0,7.516l0,0 c0-0.82,0.665-1.484,1.484-1.484h15.032C17.335,6.031,18,6.696,18,7.516L18,7.516z M18,13.516C18,14.335,17.335,15,16.516,15H1.484 C0.665,15,0,14.335,0,13.516l0,0c0-0.82,0.665-1.483,1.484-1.483h15.032C17.335,12.031,18,12.695,18,13.516L18,13.516z")()
          )
        )
      ),
      el("div", "class" -> "trigger")(
        // TODO from Config!
        el("a", "class" -> "nav-item", "href" -> "/tags/")("Tags"),
        el("a", "class" -> "nav-item", "href" -> "/notes/")("Notes")
      )
    )

  private def main: Xml.Element =
    el("main", "class" -> "page-content", "aria-label" -> "Content") {
      val result = el("div", "class" -> "wrapper").child(article)
      (if backLinks.isEmpty then result else result.child(backlinksDiv)).build
    }

  private def backlinksDiv: Xml.Element =
    el("div", "class" -> "backlinks")
      .child(el("hr")())
      .child(el("h4")())
      .children(backLinks.flatMap(link => List(
        XmlBuilder.text("•"),
        el("a", "class" -> "backlink", "href" -> link.url)(link.target.targetPath.path.last) // TODO page title
      )) *)
      .build

  private def article: Xml.Element =
    val isBlogPost: Boolean = page.targetPath.path.last != "index"

    val content: Xml = page.getXml
  
    if isBlogPost then
      el("article", "class" -> "post h-entry", "itemscope" -> "", "itemtype" -> "http://schema.org/BlogPosting")(
        el("header", "class" -> "post-header")(
          el("h1", "class" -> "post-title  p-name", "itemprop" -> "name headline")(page.title),
          postMeta
        ),
        // TODO unwrap the content div?
        el("div", "class" -> "post-content e-content", "itemprop" -> "articleBody")(content),
        // TODO el("a", "class" -> "u-url", "href" -> "TODO page url", "hidden" -> "")()
      )
    else
      el("article", "class" -> "post")(
        el("header", "class" -> "post-header")(
          el("h1", "class" -> "post-title")(page.title)
        ),
        // TODO unwrap the content div?
        el("div", "class" -> "post-content")(
          content
          // TODO add page directory listing to the page
        )
      )

  private def postMeta: Xml.Element =
    el("p", "class" -> "post-meta")
      // TODO <time class="dt-published" datetime="2010-08-02T15:16:00-04:00" itemprop="datePublished">Aug 2, 2010</time> // TODO only for blog posts
      .child(XmlBuilder.text("|"))
      .children(page.frontMatter.tags.map(tag => // TODO link!
        el("a", "class" -> "post-tag", "href" -> s"/tags/#$tag")(tag)
      )*)
      // TODO only if page.frontMatter.author.isDefined
      .child(XmlBuilder.text("•"))
      .child(
        el("span", "itemprop" -> "author", "itemscope" -> "", "itemtype" -> "http://schema.org/Person")(
          el("span", "class" -> "p-author h-card", "itemprop" -> "name")(page.frontMatter.author.getOrElse(config.author))
        )
      )
      .build

  // TODO data href: base?
  private def footer: Xml.Element =
    el("footer", "class" -> "site-footer h-card")(
      el("data", "class" -> "u-url", "href" -> "/")(XmlBuilder.comment("do not self-close")),
      el("div", "class" -> "wrapper")(
        el("h2", "class" -> "footer-heading")(config.title),
        el("div", "class" -> "footer-col-wrapper")(
          el("div", "class" -> "footer-col footer-col-1")(
            el("ul", "class" -> "contact-list")(
              el("li", "class" -> "p-name")(config.author),
              el("li")(
                el("a", "class" -> "u-email", "href" -> s"mailto:${config.email}")(config.email)
              )
            )
          ),
          el("div", "class" -> "footer-col footer-col-2")(
            el("ul", "class" -> "social-media-list")
              // TODO from Config
              //    <li><a href="https://github.com/dubinsky"><svg class="svg-icon"><use xlink:href="/assets/minima-social-icons.svg#github"></use></svg> <span class="username">dubinsky</span></a></li>
              //    <li><a href="https://www.linkedin.com/in/leoniddubinsky"><svg class="svg-icon"><use xlink:href="/assets/minima-social-icons.svg#linkedin"></use></svg> <span class="username">leoniddubinsky</span></a></li>
              //    <li><a href="https://www.twitter.com/leoniddubinsky"><svg class="svg-icon"><use xlink:href="/assets/minima-social-icons.svg#twitter"></use></svg> <span class="username">leoniddubinsky</span></a></li>
              .build
          ),
          el("div", "class" -> "footer-col footer-col-3")(config.description)
        )
      )
    )
