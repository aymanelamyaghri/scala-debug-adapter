package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi.{ArrayReference, ThreadReference, Value}

import scala.jdk.CollectionConverters.*

object JdiArray {
  def apply(value: Value, thread: ThreadReference): JdiArray =
    new JdiArray(value.asInstanceOf[ArrayReference], thread)

  def apply(arrayType: String, arraySize: Int, classLoader: JdiClassLoader): Safe[JdiArray] = {
    val thread = classLoader.thread
    for {
      classClass <- classLoader.loadClass("java.lang.Class")
      intValue <- classLoader.mirrorOf("int")
      intClass <-
        classClass.invoke("getPrimitiveClass", List(intValue))
      arrayClass <- classLoader.loadClass("java.lang.reflect.Array")
      newInstanceValue <- classLoader.mirrorOf("newInstance")
      newInstanceMethod <- arrayClass
        .invoke("getMethod", List(newInstanceValue, classClass.reference, intClass))
        .map(JdiObject(_, thread))
      arrayTypeClass <- classLoader.loadClass(arrayType)
      integerRef <- JdiPrimitive.box(arraySize, classLoader, thread)
      array <- newInstanceMethod
        .invoke("invoke", List(null, arrayTypeClass.reference, integerRef))
        .map(JdiArray(_, thread))
    } yield array
  }
}

class JdiArray(reference: ArrayReference, thread: ThreadReference) extends JdiObject(reference, thread) {
  def setValue(index: Int, value: Value): Unit =
    reference.setValue(index, value)

  def setValues(values: Seq[Value]): Unit = reference.setValues(values.asJava)

  def getValues: Seq[Value] = reference.getValues.asScala.toSeq
}
