package org.podval.tools.publish

import zio.blocks.schema.xml.{Xml, XmlBuilder}
import Html.{apply, childWhen, childrenWhen, el}

// Based on https://github.com/jekyll/minima
// TODO calculate based on PageKind?
// TODO icons!
// TODO add TOC
final class Minima(
  config: Config,
  page: Page,
  backLinks: List[Link]
):
  private val libraries: List[JavascriptLibrary] = List(
    Highlights.get(page.xml),
    Option.when(page.frontMatter.math)(MathJax)
  ).flatten

  // TODO val isBlogPost: Boolean = page.targetPath.path.last != "index"

  def render: Xml = page.frontMatter.layout match
    case Some("post") => postLayout(page.xml)
    case Some("default") => baseLayout(page.xml)
    case _ => pageLayout(page.xml)

  private def pageLayout(content: Xml): Xml.Element = baseLayout(
    el("article", "class" -> "post")(
      el("header", "class" -> "post-header")(
        el("h1", "class" -> "post-title")(
          page.title
        )
      ),
      el("div", "class" -> "post-content")(
        content
        // TODO add page directory listing to 'index' pages
      )
    )
  )

  private def postLayout(content: Xml): Xml.Element = baseLayout(
    el("article", "class" -> "post h-entry", "itemscope" -> "", "itemtype" -> "http://schema.org/BlogPosting")(
      el("header", "class" -> "post-header")(
        el("h1", "class" -> "post-title p-name", "itemprop" -> "name headline")(
          page.title
        ),
        el("div", "class" -> "post-meta")
          // TODO
          // .childWhen(page.modified_date.isDefined,
          //    el("span", "class" -> "meta-label")("Published:")
          // )
          // TODO!
          //.child(
          //  el("time", "class" -> "dt-published", "datetime" -> page.frontMatter.date, "itemprop" -> "datePublished")(
          //    page.frontMatter.date.mmddyy
          //  )
          //)
          // TODO
          // {%- if page.modified_date -%}
          //   <span class="bullet-divider">•</span>
          //   <span class="meta-label">Updated:</span>
          //   {%- assign mdate = page.modified_date | date_to_xmlschema %}
          //   <time class="dt-modified" datetime="{{ mdate }}" itemprop="dateModified">
          //     {{ mdate | date: date_format }}
          //   </time>
          // {%- endif -%}
          .childWhen(page.frontMatter.tags.nonEmpty, XmlBuilder.text("|"))
          .childrenWhen(page.frontMatter.tags.nonEmpty, page.frontMatter.tags.map(tag => // TODO link!
            el("a", "class" -> "post-tag", "href" -> s"/tags/#$tag")(tag)
          ))
          // TODO!
          // - default: site author
          //.childWhen(page.author.nonEmpty, XmlBuilder.text("•"))
          //.childWhen(page.author.nonEmpty,
          //  el("div", "class" -> s"${if page.frontMatter.modified_date then "" else "force-inline "}post-authors")(
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
      el("div", "class" -> "post-content e-content", "itemprop" -> "articleBody")(
        content
      ),
      // Note: skipped Disqus comments
      // TODO el("a", "class" -> "u-url", "href" -> page.url, "hidden" -> "")()
    )
  )

  // TODO used for the main index!!!
  private def homeLayout(content: Xml.Element): Xml.Element =
    ???
    //<div class="home">
    //  {%- if page.title -%}
    //    <h1 class="page-heading">{{ page.title }}</h1>
    //  {%- endif -%}
    //
    //  {{ content }}
    //
    //
    //  {% if site.paginate %}
    //    {% assign posts = paginator.posts %}
    //  {% else %}
    //    {% assign posts = site.posts %}
    //  {% endif %}
    //
    //
    //  {%- if posts.size > 0 -%}
    //    {%- if page.list_title -%}
    //      <h2 class="post-list-heading">{{ page.list_title }}</h2>
    //    {%- endif -%}
    //    <ul class="post-list">
    //      {%- assign date_format = site.minima.date_format | default: "%b %-d, %Y" -%}
    //      {%- for post in posts -%}
    //      <li>
    //        <span class="post-meta">{{ post.date | date: date_format }}</span>
    //        <h3>
    //          <a class="post-link" href="{{ post.url | relative_url }}">
    //            {{ post.title | escape }}
    //          </a>
    //        </h3>
    //        {%- if site.minima.show_excerpts -%}
    //          {{ post.excerpt }}
    //        {%- endif -%}
    //      </li>
    //      {%- endfor -%}
    //    </ul>
    //
    //    {% if site.paginate %}
    //      <div class="pager">
    //        <ul class="pagination">
    //        {%- if paginator.previous_page %}
    //          <li>
    //            <a href="{{ paginator.previous_page_path | relative_url }}" class="previous-page" title="Go to Page {{ paginator.previous_page }}">
    //              {{ paginator.previous_page }}
    //            </a>
    //          </li>
    //        {%- else %}
    //          <li><div class="pager-edge">•</div></li>
    //        {%- endif %}
    //          <li><div class="current-page">{{ paginator.page }}</div></li>
    //        {%- if paginator.next_page %}
    //          <li>
    //            <a href="{{ paginator.next_page_path | relative_url }}" class="next-page" title="Go to Page {{ paginator.next_page }}">
    //              {{ paginator.next_page }}
    //            </a>
    //          </li>
    //        {%- else %}
    //          <li><div class="pager-edge">•</div></li>
    //        {%- endif %}
    //        </ul>
    //      </div>
    //    {%- endif %}
    //  {%- endif -%}
    //</div>

  private def baseLayout(content: Xml): Xml.Element =
    el("html", "lang" -> page.frontMatter.lang.orElse(config.lang).getOrElse("en"))(
      head,
      el("body")
        .child(header)
        .child(
          el("main", "class" -> "page-content", "aria-label" -> "Content")(
            el("div", "class" -> "wrapper")
              .child(content)
              .childWhen(backLinks.nonEmpty, backlinksDiv)
              .build
          )
        )
        .child(
          el("link", "id" -> "fa-stylesheet", "rel" -> "stylesheet", "href" -> "https://cdn.jsdelivr.net/npm/@fortawesome/fontawesome-free@7.0.0/css/all.min.css")()
        )
        .child(footer)
        // Skipped: .child(subFooter)
        .children(libraries.flatMap(_.body) *)
        .build
    )

  private def backlinksDiv: Xml.Element =
    el("div", "class" -> "backlinks")
      .child(el("hr")())
      .child(el("h4")())
      .children(backLinks.flatMap(link => List(
        XmlBuilder.text("•"),
        el("a", "class" -> "backlink", "href" -> link.url)(link.fromPage.title)
      )) *)
      .build

  private def head: Xml.Element =
    el("head")
      .child(el("meta", "charset" -> "utf-8")())
      .child(el("meta", "http-equiv" -> "X-UA-Compatible", "content" -> "IE=edge")())
      .child(el("meta", "name" -> "viewport", "content" -> "width=device-width, initial-scale=1")())
      // TODO {%- seo -%}: https://github.com/jekyll/jekyll-seo-tag
      .child(el("title")(page.title)) // TODO this is here until seo is implemented - it covers the title...
      .children(libraries.flatMap(_.head) *)
      .child(el("link", "id" -> "main-stylesheet", "rel" -> "stylesheet", "href" -> "/assets/css/style.css")())
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
      el("div", "class" -> "wrapper")
        .child(
          el("a", "class" -> "site-title", "rel" -> "author", "href" -> "/")(
            config.title
          )
        )
        .childWhen(config.headerPages.nonEmpty,
          el("nav", "class" -> "site-nav")(
            el("input", "type" -> "checkbox", "id" -> "nav-trigger")(),
            el("label", "for" -> "nav-trigger")(
              el("span", "class" -> "menu-icon")(
//                el("svg", "viewBox" -> "0 0 18 15", "width" -> "18px", "height" -> "15px")(
//                  el("path", "d" -> "M18,1.484c0,0.82-0.665,1.484-1.484,1.484H1.484C0.665,2.969,0,2.304,0,1.484l0,0C0,0.665,0.665,0,1.484,0 h15.032C17.335,0,18,0.665,18,1.484L18,1.484z M18,7.516C18,8.335,17.335,9,16.516,9H1.484C0.665,9,0,8.335,0,7.516l0,0 c0-0.82,0.665-1.484,1.484-1.484h15.032C17.335,6.031,18,6.696,18,7.516L18,7.516z M18,13.516C18,14.335,17.335,15,16.516,15H1.484 C0.665,15,0,14.335,0,13.516l0,0c0-0.82,0.665-1.483,1.484-1.483h15.032C17.335,12.031,18,12.695,18,13.516L18,13.516z")()
//                )
              ) // TODO is it ok for span to self-close?
            ),
            navItems
          )
        )
        .build
    )

  private def navItems: Xml.Element =
    el("div", "class" -> "nav-items")(
      // TODO from config.headerPages, with resolution!
      el("a", "class" -> "nav-item", "href" -> "/tags/")("Tags"),
      el("a", "class" -> "nav-item", "href" -> "/notes/")("Notes")
    )

  private def footer: Xml.Element =
    el("footer", "class" -> "site-footer h-card")(
      el("data", "class" -> "u-url", "href" -> "/")(XmlBuilder.comment("do not self-close")),
      el("div", "class" -> "wrapper")(
        el("div", "class" -> "footer-col-wrapper")(
          el("div", "class" -> "footer-col footer-col-1")
            .childWhen(config.author.name.isDefined || config.author.email.isDefined,
              el("ul", "class" -> "contact-list")
                .childWhen(config.author.name.isDefined,
                  el("li", "class" -> "p-name")(config.author.name.get) // TODO escape
                )
                .childWhen(config.author.email.isDefined,
                  el("li")(
                    el("a", "class" -> "u-email", "href" -> s"mailto:${config.author.email.get}")(config.author.email.get)
                  )
                )
              .build
            )
          .build,
          el("div", "class" -> "footer-col footer-col-2")(
            el("p")(
              config.description // TODO escape!
            )
          )
        ),
        el("div", "class" -> "social-links")(
          social
        )
      )
    )

  // TODO alternative
  //  // TODO data href: base?
  //  private def footer: Xml.Element =
  //    el("footer", "class" -> "site-footer h-card")(
  //      el("data", "class" -> "u-url", "href" -> "/")(XmlBuilder.comment("do not self-close")),
  //      el("div", "class" -> "wrapper")(
  //        el("h2", "class" -> "footer-heading")(config.title),
  //        el("div", "class" -> "footer-col-wrapper")(
  //          el("div", "class" -> "footer-col footer-col-1")(
  //            el("ul", "class" -> "contact-list")(
  //              el("li", "class" -> "p-name")(config.author.name.get), // TODO conditional!
  //              el("li")(
  //                el("a", "class" -> "u-email", "href" -> s"mailto:${config.author.email.get}")(config.author.email.get)  // TODO conditional!
  //              )
  //            )
  //          ),
  //          el("div", "class" -> "footer-col footer-col-2")(
  //            el("ul", "class" -> "social-media-list")
  //              // TODO from Config
  //              //    <li><a href="https://github.com/dubinsky"><svg class="svg-icon"><use xlink:href="/assets/minima-social-icons.svg#github"></use></svg> <span class="username">dubinsky</span></a></li>
  //              //    <li><a href="https://www.linkedin.com/in/leoniddubinsky"><svg class="svg-icon"><use xlink:href="/assets/minima-social-icons.svg#linkedin"></use></svg> <span class="username">leoniddubinsky</span></a></li>
  //              //    <li><a href="https://www.twitter.com/leoniddubinsky"><svg class="svg-icon"><use xlink:href="/assets/minima-social-icons.svg#twitter"></use></svg> <span class="username">leoniddubinsky</span></a></li>
  //              .build
  //          ),
  //          el("div", "class" -> "footer-col footer-col-3")(config.description)
  //        )
  //      )
  //    )

  private def social: Xml.Element =
    el("ul", "class" -> "social-media-list")
      // TODO
      //{%- for entry in site.minima.social_links -%}
      //  <li>
      //    <a rel="me" href="{{ entry.url }}" target="_blank" title="{{ entry.title }}">
      //      <span class="grey fa-brands fa-{{ entry.icon }} fa-lg"></span>
      //    </a>
      //  </li>
      //{%- endfor -%}
      // TODO
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


