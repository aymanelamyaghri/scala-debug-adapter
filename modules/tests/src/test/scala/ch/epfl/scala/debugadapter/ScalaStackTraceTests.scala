package ch.epfl.scala.debugadapter

import ch.epfl.scala.debugadapter.testfmk.*

class ScalaStackTraceTests extends DebugTestSuite {
  val scalaVersion = ScalaVersion.`3.1+`


  test("should show the correct stackTrace when using  a LambdaType") {
    val source =
      """|package example
         |
         |trait Foo[F[_]]
         |
         |object Main {
         |  def foo : Foo[[X] =>> Either[X,Int]] = 
         |    ???
         |  def main(args: Array[String]): Unit = {
         |    foo
         | 
         |  }
         |}
            
        
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(7, List("test"))
    )

  }
 
  test("should show the correct stackTrace in nested calls in different classes with  arguments of methods 1") {
    val source =
      """|package example
         |class A {
         |  def method1(b: B): String = b.method2(this)
         |}
         |class B {
         |  def method2(a: A): String = new C().method3(a)
         |}
         |class C {
         |  def method3(a: A): String = new D().method4(a)
         |}
         |class D {
         |  def method4(a: A): String = "Hello, " + a.toString + "!"
         |}
         |
         |object Main:
         |  def main(args: Array[String]): Unit =
         |    val a = new A()
         |    val b = new B()
         |    val result = a.method1(b)
         |    println(result)
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)

    check(
      Breakpoint(12, List("example.D.method4(A): String",
 "example.C.method3(A): String",
  "example.B.method2(A): String",
  "example.A.method1(B): String",
  "example.Main.main(Array[String]): Unit"))
    )

  }
  test("should show the correct stackTrace when calling a function with 2 lists of args") {
    val source =
      """|package example
         |class A {
         |  def methody(b1: B)(b2 :B): String = b1.method2(this)
         |}
         |class B {
         |  def method2(a: A): String = new C().method3(a)
         |}
         |class C {
         |  def method3(a: A): String = new D().method4(a)
         |}
         |class D {
         |  def method4(a: A): String = "Hello, " + a.toString + "!"
         |}
         |
         |object Main:
         |  def main(args: Array[String]): Unit =
         |    val a = new A()
         |    val b = new B()
         |    val result = a.methody(b)(b)
         |    println(result)
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)

    check(
      Breakpoint(12, List("test"))
    )

  }
  test("should show the correct stackTrace when calling a generic method ") {
    val source =
      """|package example
         |class A {
         |  def methody[T](b1: T)(b2 :String): String = "Test generic method"
         |}

         |object Main:
         |  def main(args: Array[String]): Unit =
         |    val a = new A()
         |    
         |    val result = a.methody("")("")
         |    println(result)
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)

    check(
      Breakpoint(3, List("example.A.methody[T](T)(String): String",
  "example.Main.main(Array[String]): Unit"))
    )

  }
  test(
    "should show the correct stackTrace in nested calls in different classes with non empty arguments of methods 2"
  ) {
    val source =
      """|package example
         |
         |class A {
         |  def method1(arg1: String): String = new B().method2(arg1)
         |}
         |class B {
         |  def method2(arg2: String): String = new C().method3(arg2)
         |}
         |class C {
         |  def method3(arg3: String): String = new D().method4(arg3)
         |}
         |class D {
         |  def method4(arg4: String): String = "Hello, " + arg4 + "!"
         |}
         |
         |object Main:
         |  def main(args: Array[String]): Unit =
         |    val result = new A().method1("world")
         |    println(result)
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)

    check(
      Breakpoint(13, List("test"))
    )

  }
  test("should show the correct stackTrace in nested calls in different classes") {
    val source =
      """|package example
         |class A {
         |  def method1(): String = new B().method2()
         |}
         |class B {
         |  def method2(): String = new C().method3()
         |}
         |class C {
         |  def method3(): String = new D().method4()
         |}
         |class D {
         |  def method4(): String = "Hello, world!"
         |}
         |object Main:
         |  def main(args: Array[String]): Unit =
    
         |    val result=new A().method1()
         |    println(result)
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)

    check(
      Breakpoint(12, List("test"))
    )

  }

  test("should show the correct stackTrace in nested calls") {
    val source =
      """|package example
         |
         |object Main:
         |  def main(args: Array[String]): Unit =
         |    println("Breakpoint in main method")
         |    new Hello().greet()
         |    println("Finished all breakpoints")
         |
         |  class Hello():
         |    def greet(): Unit =
         |      println("Breakpoint in hello class")
         |      greet1()
         |    def greet1(): Unit =
         |     println("hello")
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)

    check(
      Breakpoint(14, List("Main$Hello.greet1()", "Main$Hello.greet()", "Main$.main(String[])", "Main.main(String[])"))
    )

  }
  test("should show the correct stackTrace in a pattern matching") {
    val source =
      """|package example
         |
         |object Main:
         |  def main(args: Array[String]): Unit =
         |    new Hello().greet(1)
         |   
         |
         |  class Hello():
         |    def greet(n : Int ): Unit =
         |     n match {
         |     case 1 => println("Hello 1")
         |     case 2 => println("Hello 2")
         |}
         |   
         
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)

    check(
      Breakpoint(
        11,
        List(
          "Main$Hello.greet(int)",
          "Main$.main(String[])",
          "Main.main(String[])"
        )
      )
    )

  }
  test("should show the correct stackTrace insde a map") {
    val source =
      """|package example
         |
         |object Main:
         |  def main(args: Array[String]): Unit =
         |    new Hello().greet()
         |
         |  class Hello():
         |    def greet(): Unit =
         |     List(1,2,3).map ( n => {
         |           n+1
         |     })
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(
        10,
        List(
          "test"
        )
      ),
      Breakpoint(10),
      Breakpoint(10)
    )

  }
  test("should show the correct stackTrace insde a foreach") {
    val source =
      """|package example
         |
         |object Main:
         |  def main(args: Array[String]): Unit =
         |  
         |    new Hello().greet()
         |    
         |
         |  class Hello():
         |    def greet(): Unit =
         |     List(1,2,3).foreach( n => {
         |     new Hell().gre()
         |     println(n)
         |})
         |    class Hell():
         |        def gre(): Unit = {
         |
         |         println("hey")
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)

    check(
      Breakpoint(
        18,
        List(
          "test"
        )
      ),
      Breakpoint(18),
      Breakpoint(18)
    )

  }
  test("should show the correct stackTrace in an anonymous class") {

    val source =
      """|package example
         |trait Greeter {
         |  def greet(name: String): Unit
         |}
         |object Main:
         |  def main(args: Array[String]): Unit =
         |   
         |    val greeter = new Greeter {
         |      def greet(name: String): Unit = {
         |        println(s"Hello, $name!")
         |      }
         |    }
         |    greeter.greet("World")
         |   
         |
         |  
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)

    check(
      Breakpoint(
        10,
        List(
          "test"
        )
      )
    )

  }
  test("should show the correct stackTrace in a nested class ") {
    val source =
      """|package example
         |case class intg(n : Int)
         |object Main:
         |
         |  def main(args: Array[String]): Unit =
         |  
         |    new Hello().greet()
         |    
         |
         |  class Hello():
         |    def greet(): Unit =
         |      Hell().gre()
         |     
         |     
         |
         |    class Hell():
         |        def gre(): Unit = {
         |
         |         println("hello")
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)

    check(
      Breakpoint(
        19,
        List(
          "test"
        )
      ),
      Breakpoint(19),
      Breakpoint(19)
    )

  }

}
