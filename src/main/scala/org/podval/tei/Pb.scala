package org.podval.tei

final class Pb(
  val n: String,
  val id: Option[String],
  val facs: Option[String],
  val missing: Boolean = false,
  val empty: Boolean = false
)
