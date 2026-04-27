package org.podval.tools.publish

import zio.blocks.schema.xml.{Xml, XmlBuilder}
import XmlUtil.{a, apply, childWhen, childrenWhen, div, el, setId, stylesheet, withText}

// Based on https://github.com/jekyll/minima
// TODO icons!
final class Minima(
  site: Site,
  page: PageBase
):
  private val libraries: List[XmlUtil.JavascriptLibrary] = List(
    Highlights.get(page.xml),
    Option.when(page.frontMatter.math)(MathJax),
    Some(FontAwesome())
  ).flatten

  def render: Xml = page.frontMatter.layout match
    case Some("post") => postLayout(page.xml)
    case Some("page") => pageLayout(page.xml)
    case Some("home") => homeLayout(page.xml)
    case _ => pageLayout(page.xml)
//    case _  => baseLayout(page.xml)

  private def pageLayout(content: Xml): Xml.Element =
    // TODO better CSS; link up.
    val subDirectories: List[PageBase] = site.subDirectories(page)
    val subPages: List[PageBase] = site.subPages(page)

    baseLayout(
      el("article", "class" -> "post")(
        el("header", "class" -> "post-header")
          .child(el("h1", "class" -> "post-title").withText(page.title))
          .childWhen(page.frontMatter.tags.nonEmpty, tags)
          .build,
        div("post-content")(
          content
        ),
        el("ul", "class" -> "page-list")((subDirectories ++ subPages).map(sub =>
          el("li")(sub.ref("sub"))
        )*)
      )
    )

  private def postLayout(content: Xml): Xml.Element = baseLayout(
    el("article", "class" -> "post h-entry", "itemscope" -> "", "itemtype" -> "http://schema.org/BlogPosting")(
      el("header", "class" -> "post-header")(
        el("h1", "class" -> "post-title p-name", "itemprop" -> "name headline").withText(page.title),
        div("post-meta")
          // TODO
          // .childWhen(page.modified_date.isDefined,
          //    el("span", "class" -> "meta-label")("Published:")
          // )
          .childWhen(page.frontMatter.date.isDefined,
            el("time",
              "class" -> "dt-published",
              "datetime" -> page.frontMatter.date.get.toString,
              "itemprop" -> "datePublished"
            )
              .withText(page.dateString)
          )
          // TODO
          // {%- if page.modified_date -%}
          //   <span class="bullet-divider">•</span>
          //   <span class="meta-label">Updated:</span>
          //   {%- assign mdate = page.modified_date | date_to_xmlschema %}
          //   <time class="dt-modified" datetime="{{ mdate }}" itemprop="dateModified">
          //     {{ mdate | date: date_format }}
          //   </time>
          // {%- endif -%}
          .childWhen(page.frontMatter.tags.nonEmpty, tags)
          // TODO!
          // - default: site author
          //.childWhen(page.author.nonEmpty, XmlBuilder.text("•"))
          //.childWhen(page.author.nonEmpty,
          //  div(s"${if page.frontMatter.modified_date then "" else "force-inline "}post-authors")(
          //    TODO multiple authors?
          //    {%- for author in page.author %}
          //      <span itemprop="author" itemscope itemtype="http://schema.org/Person">
          //        <span class="p-author h-card" itemprop="name">{{ author }}</span></span>
          //      {%- if forloop.last == false %}, {% endif -%}
          //    {% endfor %}
          //  )
          //)
          .build
      ),
      div("post-content e-content").attr("itemprop", "articleBody")(
        content
      ),
      // Note: skipped Disqus comments
      a("u-url", page.path.toString).attr("hidden", "")()
    )
  )

  private def tags: Xml.Element = el("span")
    .child(XmlBuilder.text("|"))
    .children(page.frontMatter.tags.map(site.tags.tagRef)*)
    .build

  private def homeLayout(content: Xml.Element): Xml.Element = baseLayout(
    div("home")(
      //      el("h1", "class" -> "page-heading").withText(page.title)
      content,
      el("h2", "class" -> "post-list-heading").withText("Posts"),
      el("ul", "class" -> "post-list")(site.posts.map(post =>
        el("li")(
          el("span", "class" -> "post-meta").withText(post.dateString),
          el("h3")(post.ref("post-link"))
          // {%- if site.minima.show_excerpts -%} {{ post.excerpt }} {%- endif -%}
        )
      )*),
      el("p", "class" -> "rss-subscribe")(
        XmlBuilder.text("subscribe"),
        el("a", "href" -> "/feed.xml").withText("via RSS")
      )
    )
  )

  private def baseLayout(content: Xml): Xml.Element =
    el("html", "lang" -> page.frontMatter.lang.getOrElse(site.lang))(
      head,
      el("body")
        .child(header)
        .child(el("main", "class" -> "page-content", "aria-label" -> "Content")(
          div("wrapper")(
            content,
            backLinks
          ),
        ))
        .child(footer)
        .children(libraries.flatMap(_.body) *)
        .build
    )

  // TODO do not include in home and index pages
  private def backLinks: Xml.Element = div("backlinks")(
    el("hr")(), // TODO do a border
    el("h4").withText("Backlinks"),
    el("ul")(site.backLinks(page).map(link =>
      el("li")(link.from.page.ref("backlink"))  // TODO!
    )*)
  )

  private def head: Xml.Element = el("head")
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

  private def header: Xml.Element =
    el("header", "class" -> "site-header")(
      div("wrapper")(
        a("site-title", "/").attr("rel", "author").withText(site.title),
        el("nav", "class" -> "site-nav")(
          el("input", "type" -> "checkbox").setId("nav-trigger")(),
          el("label", "for" -> "nav-trigger")(
            el("span", "class" -> "menu-icon")(
//                el("svg", "viewBox" -> "0 0 18 15", "width" -> "18px", "height" -> "15px")(
//                  el("path", "d" -> "M18,1.484c0,0.82-0.665,1.484-1.484,1.484H1.484C0.665,2.969,0,2.304,0,1.484l0,0C0,0.665,0.665,0,1.484,0 h15.032C17.335,0,18,0.665,18,1.484L18,1.484z M18,7.516C18,8.335,17.335,9,16.516,9H1.484C0.665,9,0,8.335,0,7.516l0,0 c0-0.82,0.665-1.484,1.484-1.484h15.032C17.335,6.031,18,6.696,18,7.516L18,7.516z M18,13.516C18,14.335,17.335,15,16.516,15H1.484 C0.665,15,0,14.335,0,13.516l0,0c0-0.82,0.665-1.483,1.484-1.483h15.032C17.335,12.031,18,12.695,18,13.516L18,13.516z")()
//                )
            ) // TODO is it ok for span to self-close?
          ),
          div("nav-items")(site.headerPages.map(_.a("nav-item"))*)
        )
      )
    )

  private def footer: Xml.Element =
    el("footer", "class" -> "site-footer h-card")(
      el("data", "class" -> "u-url", "href" -> "/")(XmlBuilder.comment("do not self-close")), // TODO base url?
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
            // Note: moved back here, where it was before, from after the footer-col-wrapper
            div("social-links")(social)
          ),
          div("footer-col footer-col-3")(
            el("p").withText(site.description) // TODO escape!
          )
        )
      )
    )

  private def social: Xml.Element =
    el("ul", "class" -> "social-media-list")
      .children(site.socialLinks.map(social =>
        el("li")(social.xml)
      )*)

      // TODO move RSS feed link here from home
      //{% unless site.minima.hide_site_feed_link %}
      //  <li>
      //    <a href="{{ site.feed.path | default: 'feed.xml' | absolute_url }}" target="_blank" title="Subscribe to syndication feed">
      //      <svg class="svg-icon grey" viewbox="0 0 16 16">
      //        <path d="M12.8 16C12.8 8.978 7.022 3.2 0 3.2V0c8.777 0 16 7.223 16 16h-3.2zM2.194
      //          11.61c1.21 0 2.195.985 2.195 2.196 0 1.21-.99 2.194-2.2 2.194C.98 16 0 15.017 0
      //          13.806c0-1.21.983-2.195 2.194-2.195zM10.606
      //          16h-3.11c0-4.113-3.383-7.497-7.496-7.497v-3.11c5.818 0 10.606 4.79 10.606 10.607z"
      //        />
      //      </svg>
      //    </a>
      //  </li>
      //{%- endunless %}
    .build
