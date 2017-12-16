package org.dubik.analytics

import org.dubik.analytics.Analytics.Session

case class Analytics(childrenMap: Map[Option[Long], List[Event]]) {
  /**
    * Returns list of sessions for specified event. If session
    * represented by following tree:
    * Science -> Kittens -> Kittens
    *         \
    *          -> Politics -> Fashion
    * Returned list would be like following:
    * List( List(Science, Kittens, Kittens), List(Science, Politics, Fashion))
    * @param event specific event for which all sessions needs to be find
    * @return list of sessions
    */
  def findSessionsForEvent(event: Event): List[Session] = {
    def findSessionsRec(event: Event, currentSession: Session, sessions: List[Session]): List[Session] = {
      val parentId = Some(event.articleId)
      val children = childrenMap.get(parentId)
      val newSession = event :: currentSession
      children match {
        // If we reached the end if sequence, reverse the list because it was build in reverse order initially
        // and prepend it to other sessions we found so far
        case None => newSession.reverse :: sessions
        // Go through all children and call findSessionsRec recursively
        case Some(childs) => childs.flatMap(findSessionsRec(_, newSession, sessions)
        )
      }
    }

    findSessionsRec(event, List(), List())
  }
}

object Analytics {
  def calculateSessionDuration(session: List[Session]): Long = {
    session.map(
      subSession => {
        val startStop = subSession.foldLeft((Long.MaxValue, Long.MinValue))(
          (res, v) => (Math.min(res._1, v.timestamp), Math.max(res._2, v.timestamp)))

        startStop._2 - startStop._1
      }
    ).max
  }

  type Session = List[Event]

  def makeChildrenMap(events: List[Event]): Map[Option[Long], List[Event]] =
    events.groupBy(_.parentId)

  def evaluateSessionWeight(session: Session): Int = {
    if (session.length < 2)
      0
    else {
      // Go through sliding pairs of input array and increment head of resulting list if they match
      // if the don't match, add new value: 1
      session.sliding(2).foldLeft(List(1))((res, v) => {
        val firstCategory = v.head.category
        val secondCategory = v.tail.head.category
        if (firstCategory == secondCategory) {
          (res.head + 1) :: res.tail
        } else {
          1 :: res
        }
      }
      ).max
    }
  }

  def findEventsWithoutParents(events: Seq[Event]): Seq[Event] = events.filter(_.parentId.isEmpty)
}
