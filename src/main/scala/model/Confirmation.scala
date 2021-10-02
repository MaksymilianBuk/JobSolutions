package com.jobsolutions
package model

import org.joda.time.DateTime

case class Confirmation(date: DateTime, employeeId: Long = 0, comments: Option[String] = None)
