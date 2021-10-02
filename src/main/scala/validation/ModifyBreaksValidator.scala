package com.jobsolutions
package validation

import model.{Confirmation, PaidBreak}

import helper.Helpers.BetweenDates
import exception.{OverlapException, WorkedDayException}
import org.joda.time.{DateTime, Interval}

import java.time.Duration

object ModifyBreaksValidator {
  def validateNewBreaks(oldBreaksSeq: Seq[PaidBreak], newBreaksSeq: Seq[PaidBreak], confirmations: Seq[Confirmation]): Boolean = {
    try {
      // Check if overlap
      checkIfOverlap(newBreaksSeq)

      // The clue is to get seq of Intervals that cannot contain any of Confirmation inside
      // It's needed to make two seq the same size. Then, it will be possible to iterate through all elements.
      val edgePoints: Seq[DateTime] = (getEdgePoints(oldBreaksSeq) ++ getEdgePoints(newBreaksSeq)).distinct.sorted
      val forbiddenIntervals = getForbiddenIntervals(getSeqUnified(edgePoints, oldBreaksSeq), getSeqUnified(edgePoints, newBreaksSeq))

      // Check if any confirmation is between forbidden intervals edges
      checkIfAnyConfirmationInIntervalSeq(confirmations, forbiddenIntervals)

      true
    }
    catch {
      case e: Exception =>
        println(e.getMessage)
        false
    }
  }

  private[validation] def checkIfOverlap(newBreaksSeq: Seq[PaidBreak]) = {
    newBreaksSeq.sortBy(_.interval.getStart).sliding(2).foreach(i => if (i.head.interval.getEnd.isAfter(i(1).interval.getStart) || i.head.interval.getEnd.isEqual(i(1).interval.getStart))
      throw OverlapException(s"OverlapException: Overlapping intervals has been found. ${i.head.interval} \t ${i(1).interval}"))
  }

  private[validation] def getBreakByDate(startTime: DateTime, seq: Seq[PaidBreak]): Option[PaidBreak] = {
    seq.find(i => i.interval.getStart == startTime || startTime.isBetween(i.interval))
  }

  private[validation] def getEdgePoints(seq: Seq[PaidBreak]): Seq[DateTime] = seq.map(_.interval.getStart) ++ seq.map(_.interval.getEnd)

  private[validation] def getSeqUnified(edgePoints: Seq[DateTime], seq: Seq[PaidBreak]): Seq[PaidBreak] = edgePoints.sliding(2).map(i => {
    val tuple = getBreakByDate(i.head, seq) match {
      case Some(x) => (x.duration, x.minWorkTime)
      case None => (Duration.ZERO, None)
    }
    PaidBreak(tuple._1, tuple._2, new Interval(i.head, i(1)))
  }).toSeq

  private[validation] def getForbiddenIntervals(oldSeqUnified: Seq[PaidBreak], newSeqUnified: Seq[PaidBreak]) =
    for {i <- oldSeqUnified.indices
         if oldSeqUnified(i) != newSeqUnified(i)
         } yield newSeqUnified(i).interval

  private[validation] def checkIfAnyConfirmationInIntervalSeq(confirmations: Seq[Confirmation], forbiddenIntervals: Seq[Interval]) = {
    confirmations.foreach(i => {
      forbiddenIntervals.foreach(j => if (i.date.isBetween(j)) throw WorkedDayException(s"WorkedDayException: Cannot change sequence! Confirmation found at ${i.date}"))
    })
  }
}
