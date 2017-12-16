package org.dubik.analytics

sealed case class Event(timestamp: Long, articleId: Long, parentId: Option[Long], category: String)

object Event {
  private val num = """(-?\d+)""".r
  private val strings = """([a-zA-Z_]+)""".r

  def fromString(jsonStr: String): Event = {
    val str = jsonStr.substring(1, jsonStr.length - 1) // strip {}
    val allNums = num.findAllIn(str).map(_.toLong).toArray
    val allStrings = strings.findAllIn(str).toArray

    val parentId = if (allNums(2) == -1) None else Some(allNums(2))
    Event(allNums(0), allNums(1), parentId, allStrings(4))
  }
}
