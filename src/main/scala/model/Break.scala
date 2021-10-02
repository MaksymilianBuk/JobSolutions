package com.jobsolutions
package model

import org.joda.time.{DateTime, Interval}

import java.time.Duration

sealed trait Break

case class NonPaidBreak() extends Break

case class PaidBreak(duration: Duration, minWorkTime: Option[Duration] = None, interval: Interval = new Interval(new DateTime(Long.MinValue), new DateTime(Long.MaxValue))) extends Break {
  val format = "yyyy-MM-dd"

  override def toString(): String = {
    s"${}${this.interval.getStart.formatted(format)} -> ${this.interval.getEnd.formatted(format)} \t ${this.duration}  ${this.minWorkTime.getOrElse("")}"
  }

}