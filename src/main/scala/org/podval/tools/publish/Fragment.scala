package org.podval.tools.publish

sealed abstract class Fragment(val id: String)

object Fragment:
  final class Block(id: String) extends Fragment(id)

  final class Section(
    id: String,
    val title: String,
    val sections: Seq[Section]
  ) extends Fragment(id)

  object Section:
    def resolve(
      result: Seq[Section],
      sections: Seq[Section],
      names: Seq[String],
      includeNested: Boolean
    ): Option[Seq[Section]] =
      if names.isEmpty then Some(result) else sections
        .find(section => section.title == names.head || section.id == names.head)
        .flatMap(section => resolve(
          result = result :+ section,
          sections = section.sections,
          names = names.tail,
          includeNested = false
        ))
        .orElse:
          if !includeNested then None else sections
            .flatMap(section => resolve(
              result = result :+ section,
              sections = section.sections,
              names = names.tail,
              includeNested = includeNested
            ))
            .headOption
