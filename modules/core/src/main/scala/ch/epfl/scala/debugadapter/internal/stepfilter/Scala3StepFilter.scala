package ch.epfl.scala.debugadapter.internal.stepfilter

import java.lang.reflect.Method
import ch.epfl.scala.debugadapter.Logger
import com.sun.jdi
import ch.epfl.scala.debugadapter.Debuggee
import java.util.function.Consumer
import java.lang.reflect.InvocationTargetException
import ch.epfl.scala.debugadapter.Java8
import ch.epfl.scala.debugadapter.Java9OrAbove
import scala.util.Try
import java.nio.file.Path
import scala.util.Success
import scala.util.Failure
import java.util.Optional
import ch.epfl.scala.debugadapter.ScalaVersion

class Scala3StepFilter(
    scalaVersion: ScalaVersion,
    bridge: Any,
    skipMethod: Method,
    formatMethod: Method
) extends ScalaStepFilter(scalaVersion) {
  override protected def skipScalaMethod(method: com.sun.jdi.Method): Boolean = {
    try skipMethod.invoke(bridge, method).asInstanceOf[Boolean]
    catch {
      case e: InvocationTargetException => throw e.getCause
    }
  }

  private def isAnonFunction(method: jdi.Method): Boolean =
    method.name.matches(".+\\$anonfun\\$\\d+")

  private def formatAnonFunction(method: jdi.Method): Optional[String] = {
    val regex = "\\$\\$anonfun.*"
    var result = method.toString.replaceAll(regex, "").replaceAll("\\$", ".")
    result = result ++ ".anonfun" ++ "("

    method.argumentTypeNames().forEach(t => result = result ++ t)
    Optional.of(result ++ ")")
  }

  override def formatName(method: jdi.Method): Optional[String] = {
    try {

      if (isAnonFunction(method)) {
        return formatAnonFunction(method)

      }
      formatMethod.invoke(bridge, method).asInstanceOf[Optional[String]]

    } catch {
      case e: InvocationTargetException => throw e.getCause
    }
  }
}

object Scala3StepFilter {
  def tryLoad(
      debuggee: Debuggee,
      classLoader: ClassLoader,
      logger: Logger,
      testMode: Boolean
  ): Try[Scala3StepFilter] = {
    try {
      val className = "ch.epfl.scala.debugadapter.internal.stepfilter.ScalaStepFilterBridge"
      val cls = classLoader.loadClass(className)
      val ctr = cls.getConstructor(classOf[Array[Path]], classOf[Consumer[String]], classOf[Boolean])

      // TASTy Query needs the javaRuntimeJars
      val javaRuntimeJars = debuggee.javaRuntime.toSeq.flatMap {
        case Java8(_, classJars, _) => classJars
        case java9OrAbove: Java9OrAbove =>
          java9OrAbove.classSystems.map(_.fileSystem.getPath("/modules", "java.base"))
      }
      val debuggeeClasspath = debuggee.classPath.toArray ++ javaRuntimeJars
      val warnLogger: Consumer[String] = msg => logger.warn(msg)
      val bridge = ctr.newInstance(
        debuggeeClasspath,
        warnLogger,
        testMode: java.lang.Boolean
      )
      val skipMethod = cls.getMethods.find(m => m.getName == "skipMethod").get
      val formatMethod = cls.getMethods.find(m => m.getName == "formatName").get

      Success(new Scala3StepFilter(debuggee.scalaVersion, bridge, skipMethod, formatMethod))
    } catch {
      case cause: Throwable => Failure(cause)
    }
  }
}
