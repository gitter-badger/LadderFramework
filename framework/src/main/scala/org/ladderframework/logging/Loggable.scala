package org.ladderframework.logging

import org.slf4j.LoggerFactory

trait Loggable {

	final lazy val logger = LoggerFactory.getLogger(getClass);
	
	def trace(msg: => Any){
		if (logger.isTraceEnabled) logger.trace(msg.toString)
	}
  def trace(msg: => Any, t: => Throwable){
  	if (logger.isTraceEnabled) logger.trace(msg.toString, t)
  }
  def debug(msg: => Any){
  	if (logger.isDebugEnabled) logger.debug(msg.toString)
  }
  def debug(msg: => Any, t: => Throwable){
  	if (logger.isDebugEnabled) logger.debug(msg.toString, t)
  }
  def info(msg: => Any){
  	if (logger.isInfoEnabled) logger.info(msg.toString)
  }
  def info(msg: => Any, t: => Throwable){
  	if (logger.isInfoEnabled) logger.info(msg.toString, t)
  }
  def warn(msg: => Any){
  	if (logger.isWarnEnabled) logger.warn(msg.toString)
  }
  def warn(msg: => Any, t: => Throwable){
  	if (logger.isWarnEnabled) logger.warn(msg.toString, t);
  } 
  def error(msg: => Any){
  	if (logger.isErrorEnabled) logger.error(msg.toString)
  }
  def error(msg: => Any, t: => Throwable){
  	if (logger.isErrorEnabled) logger.error(msg.toString, t);
  } 
	
}