package org.podval.tools.publish

import zio.blocks.schema.xml.{Xml, XmlBuilder}

// TODO add backlinks
// TODO calculate based on PageKind?
// TODO icons!
final class Layout(
  config: Config,
  page: Page.MarkupPage,
  content: Xml,
  backLinks: List[Link]
):
  // TODO make Highlights conditional?
  private val libraries: List[JavascriptLibrary] = List(
    JavascriptLibrary.Highlights
  ) ++ Option.when(page.frontMatter.math)(
    JavascriptLibrary.MathJax
  ).toList

  def render: Xml = XmlBuilder
    .element("html")
    .attr("lang", page.frontMatter.lang.orElse(config.lang).getOrElse("en"))
    .child(head)
    .child(XmlBuilder
      .element("body")
      .child(header)
      .child(main)
      .child(footer)
      .children(libraries.flatMap(_.body)*)
      .build
    )
    .build

  private def head: Xml.Element = XmlBuilder
    .element("head")
    .child(XmlBuilder
      .element("title")
      .child(XmlBuilder.text(page.title))
      .build
    )
    .children(libraries.flatMap(_.head)*)
    .child(XmlBuilder
      .element("link")
      .attr("rel", "stylesheet")
      .attr("href", "/assets/css/style.css")
      .build
    )
    .build

  private def header: Xml.Element = XmlBuilder
    .element("header")
    .attr("class", "site-header")
    .attr("role", "banner")
    .child(XmlBuilder
      .element("div")
      .attr("class", "wrapper")
      .child(XmlBuilder
        .element("a")
        .attr("class", "site-title")
        .attr("rel", "author")
        .attr("href", "/")
        .child(XmlBuilder.text(config.title))
        .build
      )
      .child(nav)
      .build
    )
    .build

  private def nav: Xml.Element = XmlBuilder
    .element("nav")
    .attr("class", "site-nav")
    .child(XmlBuilder
      .element("input")
      .attr("type", "checkbox")
      .attr("id", "nav-trigger")
      .attr("class", "nav-trigger")
      .build
    )
    .child(XmlBuilder
      .element("label")
      .attr("for", "nav-trigger")
      .child(XmlBuilder
        .element("span")
        .attr("class", "menu-icon")
        .child(XmlBuilder
          .element("svg")
          .attr("viewBox", "0 0 18 15")
          .attr("width", "18px")
          .attr("height", "15px")
          .child(XmlBuilder
            .element("path")
            .attr("d", "M18,1.484c0,0.82-0.665,1.484-1.484,1.484H1.484C0.665,2.969,0,2.304,0,1.484l0,0C0,0.665,0.665,0,1.484,0 h15.032C17.335,0,18,0.665,18,1.484L18,1.484z M18,7.516C18,8.335,17.335,9,16.516,9H1.484C0.665,9,0,8.335,0,7.516l0,0 c0-0.82,0.665-1.484,1.484-1.484h15.032C17.335,6.031,18,6.696,18,7.516L18,7.516z M18,13.516C18,14.335,17.335,15,16.516,15H1.484 C0.665,15,0,14.335,0,13.516l0,0c0-0.82,0.665-1.483,1.484-1.483h15.032C17.335,12.031,18,12.695,18,13.516L18,13.516z")
            .build
          )
          .build
        )
        .build
      )
      .build
    )
    .child(pageLinks)
    .build

  private def pageLinks: Xml.Element = XmlBuilder
    .element("div")
    .attr("class", "trigger")
    .child(XmlBuilder // TODO from Config!
      .element("a")
      .attr("class", "nav-item")
      .attr("href", "/tags/")
      .child(XmlBuilder.text("Tags"))
      .build
    )
    .child(XmlBuilder // TODO from Config!
      .element("a")
      .attr("class", "nav-item")
      .attr("href", "/notes/")
      .child(XmlBuilder.text("Notes"))
      .build
    )
    .build

  private def main: Xml.Element = XmlBuilder
    .element("main")
    .attr("class", "page-content")
    .attr("aria-label", "Content")
    .child(XmlBuilder
      .element("div")
      .attr("class", "wrapper")
      .child(XmlBuilder
        .element("article")
        .attr("class", "post")
        // TODO for blog post: class="post h-entry" itemscope itemtype="http://schema.org/BlogPosting">
        .child(XmlBuilder
          .element("header")
          .attr("class", "post-header")
          .child(XmlBuilder
            .element("h1")
            .attr("class", "post-title")
            // TODO for blog post: class="post-title p-name" itemprop="name headline"
            .child(XmlBuilder.text(page.title))
            .build
          )
          .child(postMeta)
          .build
        )
        .child(XmlBuilder
          .element("div")
          .attr("class", "post-content")
          // TODO for blog post: class="post-content e-content" itemprop="articleBody"
          .child(content) // TODO unwrap the content div?
          // TODO
          //  {% if page.name == 'index.md' %}
          //    {% include page-list.html %}
          //  {% endif %}
          .build
        )
        // TODO for blog post: <a class="u-url" href="{{ page.url | relative_url }}" hidden></a>
        .build
      )
      .child(backlinksDiv)
      .build
    )
    .build

  private def backlinksDiv =
    val div = XmlBuilder
    .element("div")
    .attr("class", "backlinks")

    val result = if backLinks.isEmpty then div else div
      .child(XmlBuilder.element("hr").build)
      .child(XmlBuilder.element("h4").build)
      .children(backLinks.flatMap(link => List(
        XmlBuilder.text("•"),
        XmlBuilder
          .element("a")
          .attr("class", "backlink")
          .attr("href", link.url)
          .child(XmlBuilder.text(link.target.targetPath.path.last)) // TODO page title
          .build
      ))*)

    result.build


  private def postMeta: Xml.Element = XmlBuilder
    .element("p")
    .attr("class", "post-meta")
    // TODO <time class="dt-published" datetime="2010-08-02T15:16:00-04:00" itemprop="datePublished">Aug 2, 2010</time> // TODO only for blog posts
    .child(XmlBuilder.text("|")) // TODO only for blog posts
    .children(page.frontMatter.tags.map(tag => XmlBuilder
      .element("a")
      .attr("class", "post-tag")
      .attr("href", s"/tags/#$tag") // TODO link!
      .child(XmlBuilder.text(tag))
      .build
    )*)
    // TODO only for blog posts and only if page.frontMatter.author.isDefined
    .child(XmlBuilder.text("•"))
    .child(XmlBuilder
      .element("span")
      .attr("itemprop", "author")
      .attr("itemscope", "")
      .attr("itemtype", "http://schema.org/Person")
      .child(XmlBuilder
        .element("span")
        .attr("class", "p-author h-card")
        .attr("itemprop", "name")
        .child(XmlBuilder.text(page.frontMatter.author.getOrElse(config.author)))
        .build
      )
      .build
    )
    .build

  private def footer: Xml.Element = XmlBuilder
    .element("footer")
    .attr("class", "site-footer h-card")
    .child(XmlBuilder
      .element("data")
      .attr("class", "u-url")
      .attr("href", "/") // TODO base?
      .child(XmlBuilder.comment("do not self-close"))
      .build
    )
    .child(XmlBuilder
      .element("div")
      .attr("class", "wrapper")
      .child(XmlBuilder
        .element("h2")
        .attr("class", "footer-heading")
        .child(XmlBuilder.text(config.title))
        .build
      )
      .child(XmlBuilder
        .element("div")
        .attr("class", "footer-col-wrapper")
        .child(XmlBuilder
          .element("div")
          .attr("class", "footer-col footer-col-1")
          .child(XmlBuilder
            .element("ul")
            .attr("class", "contact-list")
            .child(XmlBuilder
              .element("li")
              .attr("class", "p-name")
              .child(XmlBuilder.text(config.author))
              .build
            )
            .child(XmlBuilder
              .element("li")
              .child(XmlBuilder
                .element("a")
                .attr("class", "u-email")
                .attr("href", s"mailto:${config.email}")
                .child(XmlBuilder.text(config.email))
                .build
              )
              .build
            )
            .build
          )
          .build
        )
        .child(XmlBuilder
          .element("div")
          .attr("class", "footer-col footer-col-2")
          .child(XmlBuilder
            .element("ul")
            .attr("class", "social-media-list")
            // TODO from Config
            //    <li><a href="https://github.com/dubinsky"><svg class="svg-icon"><use xlink:href="/assets/minima-social-icons.svg#github"></use></svg> <span class="username">dubinsky</span></a></li>
            //    <li><a href="https://www.linkedin.com/in/leoniddubinsky"><svg class="svg-icon"><use xlink:href="/assets/minima-social-icons.svg#linkedin"></use></svg> <span class="username">leoniddubinsky</span></a></li>
            //    <li><a href="https://www.twitter.com/leoniddubinsky"><svg class="svg-icon"><use xlink:href="/assets/minima-social-icons.svg#twitter"></use></svg> <span class="username">leoniddubinsky</span></a></li>
            .build
          )
         .build
        )
        .child(XmlBuilder
          .element("div")
          .attr("class", "footer-col footer-col-3")
          .child(XmlBuilder.text(config.description))
          .build
        )
        .build
      )
      .build
    )
    .build
