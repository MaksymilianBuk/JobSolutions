package com.jobsolutions
package validation

import exception.{OverlapException, WorkedDayException}
import model.{Confirmation, PaidBreak}
import org.joda.time.{DateTime, Interval}
import org.scalatest.{FunSuite, Matchers}

import java.time.Duration

class ModifyBreaksValidatorSuite extends FunSuite with Matchers {
  def initDateTime(month: Integer): DateTime = new DateTime(2021, month, 1, 0, 0)

  def initInterval(start: Integer, end: Integer): Interval = new Interval(initDateTime(start), initDateTime(end))

  val defaultSeq1: Seq[PaidBreak] = Seq(
    PaidBreak(Duration.parse("PT15M"), Some(Duration.parse("PT6H")), initInterval(1, 3)),
    PaidBreak(Duration.parse("PT15M"), Some(Duration.parse("PT6H")), initInterval(5, 9)),
    PaidBreak(Duration.parse("PT15M"), Some(Duration.parse("PT6H")), initInterval(10, 11))
  )

  val defaultSeq2: Seq[PaidBreak] = Seq(
    PaidBreak(Duration.parse("PT15M"), Some(Duration.parse("PT6H")), initInterval(1, 2)),
    PaidBreak(Duration.parse("PT15M"), Some(Duration.parse("PT6H")), initInterval(3, 6)),
    PaidBreak(Duration.parse("PT15M"), Some(Duration.parse("PT6H")), initInterval(7, 9)),
    PaidBreak(Duration.parse("PT30M"), Some(Duration.parse("PT6H")), initInterval(10, 11))
  )

  test("check if overlap of non-overlapping intervals should not throw exception") {
    ModifyBreaksValidator.checkIfOverlap(defaultSeq1)
  }

  test("check if overlap of overlapping intervals should throw exception") {
    val seq = defaultSeq1 ++ Seq(PaidBreak(Duration.parse("PT15M"), Some(Duration.parse("PT6H")), initInterval(1, 2)))

    assertThrows[OverlapException] {
      ModifyBreaksValidator.checkIfOverlap(seq)
    }
  }

  test("check if overlap of edge overlapping intervals should throw exception") {
    val seq = defaultSeq1 ++ Seq(PaidBreak(Duration.parse("PT15M"), Some(Duration.parse("PT6H")), initInterval(3, 4)))

    assertThrows[OverlapException] {
      ModifyBreaksValidator.checkIfOverlap(seq)
    }
  }

  test("get break by date should find proper PaidBreak object") {
    val expected = PaidBreak(Duration.parse("PT15M"), Some(Duration.parse("PT6H")), initInterval(1, 3))
    val result = ModifyBreaksValidator.getBreakByDate(initDateTime(1), defaultSeq1)
    val result2 = ModifyBreaksValidator.getBreakByDate(initDateTime(2), defaultSeq1)

    result.get shouldBe expected
    result2.get shouldBe expected
  }

  test("get break should return None if not found") {
    val result = ModifyBreaksValidator.getBreakByDate(initDateTime(12), defaultSeq1)
    result shouldBe None
  }

  test("get edge points should return all edge points of sequence") {
    val expected = Seq(1, 2, 3, 6, 7, 9, 10, 11).map(i => initDateTime(i))
    val result = ModifyBreaksValidator.getEdgePoints(defaultSeq2).sorted

    result should contain theSameElementsAs expected
  }

  test("get seq unified should return splitted sequence by edge points and the same size") {
    val edgePoints: Seq[DateTime] = Seq(1, 2, 3, 5, 6, 7, 9, 10, 11).map(i => initDateTime(i))
    val expected: Seq[PaidBreak] = Seq(
      PaidBreak(Duration.parse("PT15M"), Some(Duration.parse("PT6H")), initInterval(1, 2)),
      PaidBreak(Duration.parse("PT15M"), Some(Duration.parse("PT6H")), initInterval(2, 3)),
      PaidBreak(Duration.ZERO, None, initInterval(3, 5)),
      PaidBreak(Duration.parse("PT15M"), Some(Duration.parse("PT6H")), initInterval(5, 6)),
      PaidBreak(Duration.parse("PT15M"), Some(Duration.parse("PT6H")), initInterval(6, 7)),
      PaidBreak(Duration.parse("PT15M"), Some(Duration.parse("PT6H")), initInterval(7, 9)),
      PaidBreak(Duration.ZERO, None, initInterval(9, 10)),
      PaidBreak(Duration.parse("PT15M"), Some(Duration.parse("PT6H")), initInterval(10, 11))
    )
    val result = ModifyBreaksValidator.getSeqUnified(edgePoints, defaultSeq1)
    result should contain theSameElementsAs expected
    result.size shouldBe (edgePoints.size - 1)
  }

  test("get forbidden intervals should return intervals that have been changed in new breaks definition") {
    val expected: Seq[Interval] = Seq(
      initInterval(2, 3),
      initInterval(3, 5),
      initInterval(6, 7),
      initInterval(10, 11)
    )
    val edgePoints: Seq[DateTime] = Seq(1, 2, 3, 5, 6, 7, 9, 10, 11).map(i => initDateTime(i))
    val oldUnified = ModifyBreaksValidator.getSeqUnified(edgePoints, defaultSeq1)
    val newUnified = ModifyBreaksValidator.getSeqUnified(edgePoints, defaultSeq2)
    val result = ModifyBreaksValidator.getForbiddenIntervals(oldUnified, newUnified)

    result should contain theSameElementsAs expected
  }

  test("check if any confirmation in intervals should not throw exception when confirmation date is not between any interval") {
    val confirmations: Seq[Confirmation] = Seq(Confirmation(initDateTime(1)), Confirmation(initDateTime(8)), Confirmation(initDateTime(5)))
    val edgePoints: Seq[DateTime] = Seq(1, 2, 3, 5, 6, 7, 9, 10, 11).map(i => initDateTime(i))
    val oldUnified = ModifyBreaksValidator.getSeqUnified(edgePoints, defaultSeq1)
    val newUnified = ModifyBreaksValidator.getSeqUnified(edgePoints, defaultSeq2)
    val forbiddenIntervals = ModifyBreaksValidator.getForbiddenIntervals(oldUnified, newUnified)

    ModifyBreaksValidator.checkIfAnyConfirmationInIntervalSeq(confirmations, forbiddenIntervals)
  }

  test("check if any confirmation in intervals should throw exception because confirmation date is between interval") {
    val confirmations: Seq[Confirmation] = Seq(Confirmation(initDateTime(1)), Confirmation(initDateTime(8)), Confirmation(initDateTime(4)))
    val edgePoints: Seq[DateTime] = Seq(1, 2, 3, 5, 6, 7, 9, 10, 11).map(i => initDateTime(i))
    val oldUnified = ModifyBreaksValidator.getSeqUnified(edgePoints, defaultSeq1)
    val newUnified = ModifyBreaksValidator.getSeqUnified(edgePoints, defaultSeq2)
    val forbiddenIntervals = ModifyBreaksValidator.getForbiddenIntervals(oldUnified, newUnified)

    assertThrows[WorkedDayException] {
      ModifyBreaksValidator.checkIfAnyConfirmationInIntervalSeq(confirmations, forbiddenIntervals)
    }
  }

  test("validate new breaks should return true if none of confirmation were found in non-overlapping intervals") {
    val confirmations: Seq[Confirmation] = Seq(1, 1, 2, 5, 8, 8).map(i => Confirmation(initDateTime(i)))
    ModifyBreaksValidator.validateNewBreaks(defaultSeq1, defaultSeq2, confirmations) shouldBe true
  }

  test("validate new breaks should return false on two overlapping breaks") {
    val confirmations: Seq[Confirmation] = Seq(1, 1, 2, 5, 8, 8).map(i => Confirmation(initDateTime(i)))
    val newSeq = defaultSeq2 ++ Seq(PaidBreak(Duration.parse("PT15M"), Some(Duration.parse("PT6H")), initInterval(7, 8)))
    ModifyBreaksValidator.validateNewBreaks(defaultSeq1, newSeq, confirmations) shouldBe false
  }

  test("validate new breaks should return false due to confirmation in modified interval") {
    val confirmations: Seq[Confirmation] = Seq(1, 1, 2, 4, 5, 8, 8).map(i => Confirmation(initDateTime(i)))
    ModifyBreaksValidator.validateNewBreaks(defaultSeq1, defaultSeq2, confirmations) shouldBe false
  }
}
