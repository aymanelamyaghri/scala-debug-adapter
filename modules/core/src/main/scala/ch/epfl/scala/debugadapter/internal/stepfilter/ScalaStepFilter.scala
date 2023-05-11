package ch.epfl.scala.debugadapter.internal.stepfilter

import ch.epfl.scala.debugadapter.Debuggee
import ch.epfl.scala.debugadapter.Logger
import ch.epfl.scala.debugadapter.ScalaVersion
import ch.epfl.scala.debugadapter.internal.ByteCodes
import ch.epfl.scala.debugadapter.internal.DebugTools
import ch.epfl.scala.debugadapter.internal.ScalaExtension.*
import com.sun.jdi.AbsentInformationException
import com.sun.jdi.Method
import com.sun.jdi.ReferenceType

import scala.jdk.CollectionConverters.*
import java.util.Optional

abstract class ScalaStepFilter(scalaVersion: ScalaVersion) extends StepFilter {
  protected def skipScalaMethod(method: Method): Boolean
  def format(method: Method): Option[String] = {
    if (method.isBridge) None
    else if (isDynamicClass(method.declaringType)) None
    else if (isJava(method)) Some(formatJava(method))
    else if (isConstructor(method)) Some(formatJava(method))
    else if (isStaticConstructor(method)) Some(formatJava(method))
    else if (isAdaptedMethod(method)) None
    else if (isAnonFunction(method)) Some(formatJava(method))
    else if (isLiftedMethod(method)) Some(formatJava(method))
    else if (isAnonClass(method.declaringType)) Some(formatJava(method))
    // TODO in Scala 3 we should be able to find the symbol of a local class using TASTy Query
    else if (isLocalClass(method.declaringType)) Some(formatJava(method))
    else if (scalaVersion.isScala2 && isNestedClass(method.declaringType)) Some(formatJava(method))
    else if (isDefaultValue(method)) Some(formatJava(method))
    else if (isTraitInitializer(method)) Some(formatJava(method))
    else formatScala(method)
  }
  def formatScala(method: Method): Option[String] = Some(formatJava(method))
  def formatJava(method: Method): String =
    method.declaringType().name.split("\\.").last + "." + method.name() + "(" + method.argumentTypes.asScala.toList
      .map(t => t.name().split("\\.").last)
      .reduce((x, y) => x + "," + y) + ")"

  override def shouldSkipOver(method: Method): Boolean = {

    if (method.isBridge) true
    else if (isDynamicClass(method.declaringType)) true
    else if (isJava(method)) false
    else if (isConstructor(method)) false
    else if (isStaticConstructor(method)) false
    else if (isAdaptedMethod(method)) true
    else if (isAnonFunction(method)) false
    else if (isLiftedMethod(method)) !isLazyInitializer(method) && isLazyGetter(method)
    else if (isAnonClass(method.declaringType)) false
    // TODO in Scala 3 we should be able to find the symbol of a local class using TASTy Query
    else if (isLocalClass(method.declaringType)) false
    else if (scalaVersion.isScala2 && isNestedClass(method.declaringType)) false
    else if (isDefaultValue(method)) false
    else if (isTraitInitializer(method)) skipTraitInitializer(method)
    else skipScalaMethod(method)
  }

  private def isDynamicClass(tpe: ReferenceType): Boolean =
    try {
      // source of java.lang.invoke.LambdaForm$DMH.1175962212.invokeStatic_L_L(java.lang.Object, java.lang.Object) is LambdaForm$DMH
      !tpe.sourceName.contains('.')
    } catch {
      case _: AbsentInformationException =>
        // We assume that a ReferenceType with no source name is necessarily a dynamic class
        true
    }

  private def isJava(method: Method): Boolean =
    method.declaringType.sourceName.endsWith(".java")

  private def isConstructor(method: Method): Boolean =
    method.name == "<init>"

  private def isStaticConstructor(method: Method): Boolean =
    method.name == "<clinit>"

  private def isAnonFunction(method: Method): Boolean =
    method.name.matches(".+\\$anonfun\\$\\d+")

  private def isLiftedMethod(method: Method): Boolean =
    method.name.matches(".+\\$\\d+")

  private def isAdaptedMethod(method: Method): Boolean =
    method.name.matches(".+\\$adapted(\\$\\d+)?")

  private def isLazyInitializer(method: Method): Boolean =
    method.name.contains("$lzyINIT") || method.name.contains("$lzycompute$")

  private val lazyTypes: Set[String] = Set(
    "scala.runtime.LazyRef",
    "scala.runtime.LazyBoolean",
    "scala.runtime.LazyByte",
    "scala.runtime.LazyChar",
    "scala.runtime.LazyShort",
    "scala.runtime.LazyInt",
    "scala.runtime.LazyLong",
    "scala.runtime.LazyFloat",
    "scala.runtime.LazyDouble",
    "scala.runtime.LazyUnit"
  )
  private def isLazyGetter(method: Method): Boolean =
    method.argumentTypes.asScala.toSeq match {
      case Seq(argType) => lazyTypes.contains(argType.name)
      case _ => false
    }

  private def isDefaultValue(method: Method): Boolean =
    method.name.contains("$default$")

  private def isAnonClass(tpe: ReferenceType): Boolean =
    tpe.name.contains("$anon$")

  private def isLocalClass(tpe: ReferenceType): Boolean =
    tpe.name.matches(".+\\$\\d+")

  private def isNestedClass(tpe: ReferenceType): Boolean =
    tpe.name.matches(".+\\$\\.+")

  private def isTraitInitializer(method: Method): Boolean =
    method.name == "$init$"

  private def skipTraitInitializer(method: Method): Boolean =
    method.bytecodes.toSeq == Seq(ByteCodes.RETURN)
}

object ScalaStepFilter {
  def apply(
      debuggee: Debuggee,
      tools: DebugTools,
      logger: Logger,
      testMode: Boolean
  ): ScalaStepFilter = {
    if (debuggee.scalaVersion.isScala2)
      new Scala2StepFilter(tools.sourceLookUp, debuggee.scalaVersion, logger, testMode)
    else
      tools.stepFilter
        .flatMap { classLoader =>
          Scala3StepFilter
            .tryLoad(debuggee, classLoader, logger, testMode)
            .warnFailure(logger, s"Cannot load step filter for Scala ${debuggee.scalaVersion}")
        }
        .getOrElse(fallback(debuggee.scalaVersion))
  }

  private def fallback(scalaVersion: ScalaVersion): ScalaStepFilter = new ScalaStepFilter(scalaVersion) {
    override protected def skipScalaMethod(method: Method): Boolean = false
  }
}
