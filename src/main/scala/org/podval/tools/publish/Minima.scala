package org.podval.tools.publish

import zio.blocks.schema.xml.Xml
import XmlUtil.{a, apply, child, div, el, span, ul, setId, stylesheet, withText}

// Based on https://github.com/jekyll/minima
// TODO add up/prev/next to navigation!
// TODO unfold?
object Minima:
  def render(page: MarkupPage, content: Seq[Xml.Element]): Xml.Element =
    val isBaseLayout = page.isInstanceOf[MarkupPage.BaseLayout]

    val libraries: List[XmlUtil.JavascriptLibrary] = List(
      Highlights.get(content),
      Option.when(page.math)(MathJax),
      Some(FontAwesome())
    ).flatten

    // TODO I may have to rip out the titles at layout...
    el("html", "lang" -> page.lang)(
      head(page, libraries),
      el("body")
        .child(header(page.site))
        .child(el("main", "class" -> "page-content", "aria-label" -> "Content")(
          div("wrapper")(
            el("article", "class" -> "post h-entry", "itemscope" -> "", "itemtype" -> "http://schema.org/BlogPosting")(
              el("header", "class" -> "post-header")
                .child(el("h1", "class" -> "post-title p-name", "itemprop" -> "name headline").withText(page.title))
                .child(Option.when(!isBaseLayout)(articleMeta(page)))
                .build,
              div("post-content e-content", "itemprop" -> "articleBody")(content*),
              // Note: skipped Disqus comments for posts
              a("u-url", page.path.toString).attr("hidden", "")()
            ),
            // TODO do not include when isBaseLayout?
            div("backlinks")(
              el("hr")(), // TODO do a border
              el("h3").withText("Backlinks"),
              ul("backlinks-list", page.site.backLinks(page), link => link.from.page.ref("backlink"))  // TODO!
            )
          ),
        ))
        .child(footer(page.site))
        .children(libraries.flatMap(_.body) *)
        .build
    )

  private def articleMeta(page: MarkupPage): Xml.Element =
    div("post-meta")(
      join(
        join(
          join(
            time(Option.when(page.dateModified.nonEmpty)("Published:"), page.date, "dt-published", "datePublished"),
            "•",
            time(Some("Updated:"), page.dateModified, "dt-modified", "dateModified")
          ),
          "•",
          Seq(
            span("post-authors"):
              span("post-author", "itemprop" -> "author", "itemscope" -> "", "itemtype" -> "http://schema.org/Person"):
                span("p-author h-card", "itemprop" -> "name").withText(page.author)
          )
        ),
        "|",
        page.tags.map(page.site.tags.tagRef)
      )
    *)

  private def join(left: Seq[Xml.Element], text: String, right: Seq[Xml.Element]): Seq[Xml.Element] =
    if left.nonEmpty && right.nonEmpty
    then left ++ Seq(span("bullet-divider").withText(text)) ++ right
    else left ++ right

  private def time(label: Option[String], date: Option[Date], cls: String, itemprop: String): Seq[Xml.Element] =
    date.fold(Seq.empty): date =>
      label.fold(Seq.empty)(label => Seq(span("meta-label").withText(label))) ++
      Seq(el("time", "class" -> cls, "datetime" -> date.toString, "itemprop" -> itemprop).withText(date.toShortString))

  private def head(
    page: MarkupPage,
    libraries: List[XmlUtil.JavascriptLibrary]
  ): Xml.Element =
    el("head")
      .child(el("meta", "charset" -> "utf-8")())
      .child(el("meta", "http-equiv" -> "X-UA-Compatible", "content" -> "IE=edge")())
      .child(el("meta", "name" -> "viewport", "content" -> "width=device-width, initial-scale=1")())
      // TODO {%- seo -%}: https://github.com/jekyll/jekyll-seo-tag
      .child(el("title").withText(page.title)) // TODO this is here until seo is implemented - it covers the title...
      .children(libraries.flatMap(_.head) *)
      .child(stylesheet("/assets/css/style.css", id = Some("main-stylesheet")))
      // TODO {%- feed_meta -%}: https://github.com/jekyll/jekyll-feed
      // TODO
      // {%- if jekyll.environment == 'production' and site.google_analytics -%}
      //   .children(googleAnalytics*)
      // {%- endif -%}
      // Skipped: {%- include custom-head.html -%}
      .build

  private def googleAnalytics: Seq[Xml.Element] = Seq.empty
    // TODO
    //<script async src="https://www.googletagmanager.com/gtag/js?id={{ site.google_analytics }}"></script>
    //<script>
    //  window.dataLayer = window.dataLayer || [];
    //  function gtag(){window.dataLayer.push(arguments);}
    //  gtag('js', new Date());
    //
    //  gtag('config', '{{ site.google_analytics }}');
    //</script>

  private def header(site: Site): Xml.Element =
    el("header", "class" -> "site-header")(
      div("wrapper")(
        a("site-title", "/").attr("rel", "author").withText(site.title),
        el("nav", "class" -> "site-nav")(
          el("input", "type" -> "checkbox").setId("nav-trigger")(),
          el("label", "for" -> "nav-trigger")(
            span("menu-icon")(
//                el("svg", "viewBox" -> "0 0 18 15", "width" -> "18px", "height" -> "15px")(
//                  el("path", "d" -> "M18,1.484c0,0.82-0.665,1.484-1.484,1.484H1.484C0.665,2.969,0,2.304,0,1.484l0,0C0,0.665,0.665,0,1.484,0 h15.032C17.335,0,18,0.665,18,1.484L18,1.484z M18,7.516C18,8.335,17.335,9,16.516,9H1.484C0.665,9,0,8.335,0,7.516l0,0 c0-0.82,0.665-1.484,1.484-1.484h15.032C17.335,6.031,18,6.696,18,7.516L18,7.516z M18,13.516C18,14.335,17.335,15,16.516,15H1.484 C0.665,15,0,14.335,0,13.516l0,0c0-0.82,0.665-1.483,1.484-1.483h15.032C17.335,12.031,18,12.695,18,13.516L18,13.516z")()
//                )
            )
          ),
          div("nav-items")(site.headerPages.map(_.a("nav-item"))*)
        )
      )
    )

  private def footer(site: Site): Xml.Element =
    el("footer", "class" -> "site-footer h-card")(
      el("data", "class" -> "u-url", "href" -> "/")(), // TODO base url?
      div("wrapper")(
        el("h2", "class" -> "footer-heading").withText(site.title), // TODO as it used to be
        div("footer-col-wrapper")(
          div("footer-col footer-col-1")(
            el("ul", "class" -> "contact-list")(
              el("li", "class" -> "p-name").withText(site.author), // TODO escape
              el("li")(a("u-email", s"mailto:${site.email}").withText(site.email))
            )
          ),
          div("footer-col footer-col-2")(
            div("social-links")(
              ul("social-media-list", site.socialLinks, social =>
                el("li")(
                  el("a", "rel" -> "me", "href" -> social.href, "target" -> "_blank", "title" -> social.title)(
                    XmlUtil.faBrand(social.icon),
                    span("username").withText(social.userName)
                  )
                )
              )
            )
          ),
          div("footer-col footer-col-3")(
            el("p").withText(site.description), // TODO escape!
            el("p")(
              el("a", "href" -> Feed.path.toString)(
                XmlUtil.faIcon("rss"),
                span("rss-feed").withText("RSS feed")
              )
            )
          )
        )
      )
    )
