package org.dubik.analytics

import org.dubik.analytics.Analytics.Session

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val events = loadEventsList()
    val (session, weight) = findSessionWithHighestWeight(events)
    val duration = Analytics.calculateSessionDuration(session)

    println(s"Maximum weight: $weight, duration: $duration")
    session.foreach(s => println(s.map(_.category).mkString(" - ")))
  }

  private def loadEventsList(): List[Event] =
    Source.fromResource("events.json").getLines().map(Event.fromString).toList

  /**
    * Finds session/subsessions and it's weight. Session is a tree not a list,
    * so function returns all possible sessions for specific event. If session
    * represented by following tree:
    * Science -> Kittens -> Kittens
    *         \
    *          -> Politics -> Fashion
    * Returned list would be like following:
    * List( List(Science, Kittens, Kittens), List(Science, Politics, Fashion))
    *
    * @param events list of events which needs to be processed, shouldn't contain unknown parent ids
    * @return a pair of list of sessions and it's weight
    */
  private def findSessionWithHighestWeight(events: List[Event]): (List[Session], Int) = {
    val childrenMap = Analytics.makeChildrenMap(events)
    val analytics = Analytics(childrenMap)

    val (event, weight) = Analytics.findEventsWithoutParents(events) // getting all events which don't have parent
      .flatMap(analytics.findSessionsForEvent) // find all sessions
      .map(session => (session.head, Analytics.evaluateSessionWeight(session))) // evalue weights of session and combine it with parent event
      .maxBy(_._2) // selects events which was parent of maximum weight session

    (analytics.findSessionsForEvent(event), weight)
  }
}

