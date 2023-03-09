package ch.epfl.scala.debugadapter.internal
import com.microsoft.java.debug.core.adapter.{StackTraceFilterProvider => JavaStackTraceFilterProvider}

import com.microsoft.java.debug.core.protocol.Requests.StepFilters
import com.sun.jdi.Method

import ch.epfl.scala.debugadapter.Logger
import ch.epfl.scala.debugadapter.internal.stepfilter._
import ch.epfl.scala.debugadapter.Debuggee
import ch.epfl.scala.debugadapter.DebugTools
import com.sun.jdi.Location

class StackTraceFilterProvider( stepFilters: Seq[StepFilter]) extends JavaStackTraceFilterProvider(){
  override def shouldSkipFrame(method: Method, filters: StepFilters): Boolean = {
    try {
        
      val skipOver = super.shouldSkipFrame(method,filters)|| stepFilters.exists(_.shouldSkipOver(method))
      skipOver
    } catch {
      case cause: Throwable =>
       
        false
    }
  }

}
   object StackTraceFilterProvider {
  def apply(
      debuggee: Debuggee,
      tools: DebugTools,
      sourceLookUp: SourceLookUpProvider,
      logger: Logger,
      testMode: Boolean
  ): StackTraceFilterProvider= {
    val scalaStepFilter = ScalaStepFilter(debuggee, tools, sourceLookUp, logger, testMode)
    val runtimeStepFilter = RuntimeStepFilter(debuggee.scalaVersion)
    new StackTraceFilterProvider(
      Seq(ClassLoadingStepFilter, runtimeStepFilter, scalaStepFilter))
  }
}
