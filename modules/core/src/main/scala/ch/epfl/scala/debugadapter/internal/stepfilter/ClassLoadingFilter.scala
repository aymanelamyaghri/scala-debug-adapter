package ch.epfl.scala.debugadapter.internal.stepfilter

import com.sun.jdi.{Location, Method}
import ch.epfl.scala.debugadapter.internal.ByteCodes
import java.util.Optional

object ClassLoadingStepFilter extends StepFilter {
  override def formatName(method : Method) : Optional[String] = Optional.of(method.name())

  override def shouldSkipOut(upperLocation: Location, method: Method): Boolean = {
    val previousByteCode = upperLocation.method.bytecodes.apply(upperLocation.codeIndex.toInt)
    previousByteCode == ByteCodes.NEW && method.name != "<init>"
  }
}
