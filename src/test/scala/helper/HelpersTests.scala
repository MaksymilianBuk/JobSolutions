package com.jobsolutions
package helper

import helper.Helpers.BetweenDates
import org.joda.time.{DateTime, Interval}
import org.scalatest.{FunSuite, Matchers}

class HelpersTests extends FunSuite with Matchers {
  def initDateTime(month: Integer): DateTime = new DateTime(2021, month, 1, 0, 0)

  def initInterval(start: Integer, end: Integer): Interval = new Interval(initDateTime(start), initDateTime(end))

  test("inBetween on date that is in interval should return true") {
    initDateTime(3) isBetween initInterval(1, 5) shouldBe true
  }

  test("inBetween on date that is not in interval should return false") {
    initDateTime(3) isBetween initInterval(4, 5) shouldBe false
    initDateTime(4) isBetween initInterval(4, 5) shouldBe false
  }

  test("inBetweenOrEqual on date that is in interval should return true") {
    initDateTime(4) isBetweenOrEqual initInterval(4, 6) shouldBe true
    initDateTime(5) isBetweenOrEqual initInterval(4, 6) shouldBe true
  }

  test("inBetweenOrEqual on date that is not in interval should return true") {
    initDateTime(3) isBetweenOrEqual initInterval(4, 5) shouldBe false
  }
}
