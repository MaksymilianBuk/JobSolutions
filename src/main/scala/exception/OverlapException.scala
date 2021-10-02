package com.jobsolutions
package exception

final case class OverlapException(private val message: String = "",
                                  private val cause: Throwable = None.orNull)
  extends Exception(message, cause)
