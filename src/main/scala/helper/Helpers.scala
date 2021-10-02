package com.jobsolutions
package helper

import org.joda.time.{DateTime, Interval}

object Helpers {
  implicit class BetweenDates(date: DateTime) {
    def isBetween(interval: Interval): Boolean = date.isAfter(interval.getStart) && date.isBefore(interval.getEnd)

    def isBetweenOrEqual(interval: Interval): Boolean = (date.isAfter(interval.getStart) || date.isEqual(interval.getStart)) && (date.isBefore(interval.getEnd) || date.isEqual(interval.getEnd))
  }
}
