package org.podval.tools.publish

final case class Section(
  title: String,
  level: Int,
  id: String,
  sections: Seq[Section]
):
  def is(name: String): Boolean = title == name || id == name

  // TODO move to Toc
  def sectionsFlat: Seq[Section] = sections.flatMap(_.sectionsFlat)
