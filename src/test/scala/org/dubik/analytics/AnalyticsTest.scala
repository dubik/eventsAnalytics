package org.dubik.analytics

import org.dubik.analytics.Analytics.Session
import org.junit.Assert._
import org.junit.{Assert, Test}

class AnalyticsTest {
  @Test
  def testSingleEventParsingNoParent(): Unit = {
    val str = """{"timestamp":1505379681,"article_id":1822925634,"parent_id":-1,"category":"Science"}"""
    assertEquals(Event(1505379681, 1822925634, None, "Science"), Event.fromString(str))
  }

  @Test
  def testSingleEventParsingWithValidParent(): Unit = {
    val str = """{"timestamp":1505379681,"article_id":1822925634,"parent_id":234,"category":"Science"}"""
    assertEquals(Event(1505379681, 1822925634, Some(234), "Science"), Event.fromString(str))
  }

  private val events =
    List(
      Event(1, 100, None, "Games"),
      Event(2, 101, Some(100), "Games"),
      Event(3, 102, Some(101), "Politics"),
      Event(4, 103, Some(100), "Kittens")
    )

  private val subSession1 = List(events(0), events(1), events(2)) // Games - Games - Politics
  private val subSession2 = List(events(0), events(3)) // Gamges - Kittens

  @Test
  def testFindSessions(): Unit = {
    val childrenMap = Analytics.makeChildrenMap(events)
    val actualSessions: Seq[Session] = Analytics(childrenMap).findSessionsForEvent(events.head)

    val expectedSession = List(subSession1, subSession2)
    assertEquals(expectedSession, actualSessions)
  }

  @Test
  def testEvaluateSmallestSession1Weight(): Unit = {
    // Session with one event should have weight 0
    val subSession1Weight = Analytics.evaluateSessionWeight(List(events.head))
    assertEquals(0, subSession1Weight)
  }

  @Test
  def testEvaluateSession1Weight(): Unit = {
    val subSession1Weight = Analytics.evaluateSessionWeight(subSession1)
    assertEquals(2, subSession1Weight)
  }

  @Test
  def testEvaluateSession2Weight(): Unit = {
    val sessionWeight = Analytics.evaluateSessionWeight(subSession2)
    assertEquals(1, sessionWeight)
  }

  @Test
  def testCalculateSessionDuration(): Unit = {
    val duration = Analytics.calculateSessionDuration(List(subSession1, subSession2))
    assertEquals(3, duration)
  }
}
