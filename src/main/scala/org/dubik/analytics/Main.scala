package verto

import org.dubik.analytics.Analytics.Session
import org.dubik.analytics.{Analytics, Event}

import scala.io.Source

object Main {
  private def loadEventsList(): List[Event] =
    Source.fromResource("events.json").getLines().map(Event.fromString).toList

  def main(args: Array[String]): Unit = {
    val events = loadEventsList()
    val (session, weight) = findSessionWithHighestWeight(events)
    val duration = Analytics.calculateSessionDuration(session)

    println(s"Maximum weight: $weight, duration: $duration")
    session.foreach(s => println(s.map(_.category).mkString(" - ")))
  }

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

