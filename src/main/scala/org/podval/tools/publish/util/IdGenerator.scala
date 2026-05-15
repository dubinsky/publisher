package org.podval.tools.publish.util
 
final class IdGenerator:
  private var generatedId: Int = 0

  def generate(): String =
    generatedId = generatedId + 1
    s"_generated_id$generatedId"
