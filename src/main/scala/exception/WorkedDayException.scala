package com.jobsolutions
package exception

final case class WorkedDayException(private val message: String = "",
                                    private val cause: Throwable = None.orNull)
  extends Exception(message, cause)

