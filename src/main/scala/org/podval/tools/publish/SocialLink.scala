package org.podval.tools.publish

import zio.blocks.schema.xml.{Xml, XmlBuilder}
import XmlUtil.{apply, el, withText}

sealed abstract class SocialLink(
  title: String,
  site: String,
  icon: String,
  userName: String
):
  final def xml: Xml.Element =
    el("a", "rel" -> "me", "href" -> s"https://$site/$userName", "target" -> "_blank", "title" -> title)(
      el("span", "class" -> s"grey fa-brands fa-$icon fa-lg")(XmlBuilder.comment("do not self-close")),
      el("span", "class" -> "username").withText(userName)
    )

object SocialLink:
  final class GitHub(userName: String) extends SocialLink(
    title = "GitHub",
    site = "github.com",
    icon = "github",
    userName = userName
  )

  final class Twitter(userName: String) extends SocialLink(
    title = "Twitter",
    site = "www.twitter.com",
    icon = "twitter",
    userName = userName
  )

  final class LinkedIn(userName: String) extends SocialLink(
    title = "LinkedIn",
    site = "www.linkedin.com/in",
    icon = "linkedin",
    userName = userName
  )
