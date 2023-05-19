package ch.epfl.scala.debugadapter.internal.stepfilter

import com.sun.jdi.Method
import com.sun.jdi.Location
import ch.epfl.scala.debugadapter.ScalaVersion
import java.util.Optional

private class RuntimeStepFilter(
    classesToSkip: Set[String],
    methodsToSkip: Set[String]
) extends StepFilter {

  override def formatName(method : Method) : Optional[String] = Optional.of(method.name())
  override def shouldSkipOver(method: Method): Boolean =
    classesToSkip.contains(method.declaringType.name) ||
      methodsToSkip.contains(method.toString)

  override def shouldSkipOut(upperLocation: Location, method: Method): Boolean =
    classesToSkip.contains(method.declaringType.name) ||
      methodsToSkip.contains(method.toString)
}

private[internal] object RuntimeStepFilter {
  private val javaClassesToSkip = Set("sun.misc.Unsafe")
  private val javaMethodsToSkip = Set(
    "java.lang.invoke.DirectMethodHandle.internalMemberName(java.lang.Object)",
    "java.lang.invoke.DirectMethodHandle.allocateInstance(java.lang.Object)",
    "java.lang.invoke.DirectMethodHandle.constructorMethod(java.lang.Object)"
  )
  private val scala3ClassesToSkip = Set("scala.runtime.LazyVals$")
  private val scala2ClassesToSkip = Set.empty[String]

  def apply(scalaVersion: ScalaVersion): StepFilter = {
    if (scalaVersion.isScala2)
      new RuntimeStepFilter(
        scala2ClassesToSkip ++ javaClassesToSkip,
        javaMethodsToSkip
      )
    else
      new RuntimeStepFilter(
        scala3ClassesToSkip ++ scala2ClassesToSkip ++ javaClassesToSkip,
        javaMethodsToSkip
      )
  }
}
