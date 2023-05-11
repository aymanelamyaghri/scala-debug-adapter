package ch.epfl.scala.debugadapter.internal.stepfilter

import tastyquery.Contexts.Context
import tastyquery.Contexts
import tastyquery.jdk.ClasspathLoaders
import tastyquery.jdk.ClasspathLoaders.FileKind
import tastyquery.Names.*
import tastyquery.Symbols.*
import java.util.function.Consumer
import java.nio.file.Path
import ch.epfl.scala.debugadapter.internal.jdi
import tastyquery.Flags
import scala.util.matching.Regex
import scala.util.Try
import tastyquery.Types.*
import tastyquery.Signatures.*
import java.util.Optional
import scala.jdk.OptionConverters.*
import java.lang.reflect.Method

class ScalaStepFilterBridge(
    classpaths: Array[Path],
    warnLogger: Consumer[String],
    testMode: Boolean
):
  private val classpath = ClasspathLoaders.read(classpaths.toList)
  private given ctx: Context = Contexts.init(classpath)

  private def warn(msg: String): Unit = warnLogger.accept(msg)

  private def throwOrWarn(msg: String): Unit = {}
//    if (testMode) throw new Exception(msg)
  //  else warn(msg)
  def formatType(t: Type): String = {
    t match {

      case t: MethodType => {
        val isreturnType = t.resultType match
          case _: MethodType => false
          case _: PolyType => false
          case _ => true

        t.paramTypes
          .zip(t.paramNames)
          .map((x, y) => y.toString() + ": " + formatType(x))
          .mkString("(", ", ", ")")
          + (if (isreturnType) ": " else "") + formatType(t.resultType)

      }
      case t: TypeRef => {
        val optionalPrefix = t.prefix match
          case prefix: TypeRef => formatType(prefix) + "."
          case prefix: TermParamRef => formatType(prefix) + "."
          case prefix: SuperType => "super."
          case _ => ""
        optionalPrefix + t.name.toString()

      }

      case t: AppliedType =>
        val a = formatType(t.tycon)

        if (isFunction(t.tycon)) {

          (if (t.args.size != 2) "(" else "") + t.args.init
            .map(y => formatType(y))
            .reduce((x, y) => x + "," + y) + (if (t.args.size != 2) ")" else "") + " => " + formatType(
            t.args.last
          )
        } else if (isAndOrOr(t.tycon)) {

          t.args.map(t => formatType(t)).reduce((x, y) => x + a + y)

        } else
          a + "[" + t.args
            .map(y => formatType(y))
            .reduce((x, y) => x + "," + y) + "]"

      case k: PolyType => {

        "[" + k.paramNames.map(t => t.toString).mkString(", ") + "]" + formatType(
          k.resultType
        )

      }
      case t: OrType =>
        formatType(t.first) + "|" + formatType(t.second)
      case t: AndType =>
        formatType(t.first) + "&" + formatType(t.second)
      case t: ThisType => formatType(t.tref)
      case t: TermRefinement => formatType(t.parent) + " { ... }"
      case t: AnnotatedType => formatType(t.typ)
      case t: TypeParamRef => t.toString
      case t: TermRef => formatType(t.underlying)
      case t: ConstantType => t.value.value.toString()
      case t: ByNameType => "=> " + formatType(t.resultType)
      case t: TermParamRef => t.paramName.toString()
      case t: TypeRefinement => formatType(t.parent) + " { ... }"
      case _: WildcardTypeBounds => "?"
      case t: RecType => formatType(t.parent)
      case t: TypeLambda =>
        "[" + t.paramNames.map(t => t.toString).reduce((x, y) => x + "," + y) + "]" + " =>> " + formatType(
          t.resultType
        )

    }
  }

  def isFunction(tpe: Type): Boolean =
    tpe match
      case ref: TypeRef => {
        ref.prefix match
          case t: PackageRef =>
            (ref.name.toString.startsWith("Function") & t.fullyQualifiedName.toString().equals("scala"))

          case _ => { false }
      }
      case _ => false
  def isAndOrOr(tpe: Type): Boolean =
    tpe match
      case ref: TypeRef => {
        ref.prefix match
          case t: PackageRef =>
            (ref.name.toString == "|" || ref.name.toString == "&") &&
            t.fullyQualifiedName.toString == "scala"

          case _ => { false }
      }
      case _ => false

  def formatName(obj: Any): Optional[String] =
    val method = jdi.Method(obj)
    findSymbol(obj).map { t =>
      val notMethodType = t.declaredType match {
        case _: MethodType => false
        case _: PolyType => false
        case _ => true
      }
      val optionalString = if (notMethodType) ": " else ""

      s"${t.owner.name}.${t.name}${optionalString}${formatType(t.declaredType)}"

    }.asJava
  def skipMethod(obj: Any): Boolean =
    findSymbol(obj).forall(skip)

  private[stepfilter] def findSymbol(obj: Any): Option[TermSymbol] =
    val method = jdi.Method(obj)
    val isExtensionMethod = method.name.endsWith("$extension")
    val fqcn = method.declaringType.name
    findDeclaringType(fqcn, isExtensionMethod) match
      case None =>
        throwOrWarn(s"Cannot find Scala symbol of $fqcn")
        None
      case Some(declaringType) =>
        val matchingSymbols =
          declaringType.declarations
            .collect { case sym: TermSymbol if sym.isTerm => sym }
            .filter(matchSymbol(method, _, isExtensionMethod))

        if matchingSymbols.size > 1 then
          val builder = new java.lang.StringBuilder
          builder.append(
            s"Found ${matchingSymbols.size} matching symbols for $method:"
          )
          matchingSymbols.foreach(sym => builder.append(s"\n$sym"))
          throwOrWarn(builder.toString)

        matchingSymbols.headOption

  private[stepfilter] def extractScalaTerms(
      fqcn: String,
      isExtensionMethod: Boolean
  ): Seq[TermSymbol] =
    for
      declaringType <- findDeclaringType(fqcn, isExtensionMethod).toSeq
      term <- declaringType.declarations
        .collect { case sym: TermSymbol if sym.isTerm => sym }
    yield term

  private def findDeclaringType(fqcn: String, isExtensionMethod: Boolean): Option[DeclaringSymbol] =
    val javaParts = fqcn.split('.')
    val isObject = fqcn.endsWith("$")
    val packageNames = javaParts.dropRight(1).toList.map(SimpleName.apply)
    val packageSym =
      if packageNames.nonEmpty
      then ctx.findSymbolFromRoot(packageNames).asInstanceOf[PackageSymbol]
      else ctx.defn.EmptyPackage
    val className = javaParts.last
    val clsSymbols = findSymbolsRecursively(packageSym, className)
    val obj = clsSymbols.filter(_.is(Flags.Module))
    val cls = clsSymbols.filter(!_.is(Flags.Module))
    assert(obj.size <= 1 && cls.size <= 1)
    if isObject && !isExtensionMethod then obj.headOption else cls.headOption

  private def findSymbolsRecursively(owner: DeclaringSymbol, encodedName: String): Seq[DeclaringSymbol] =
    owner.declarations
      .collect { case sym: DeclaringSymbol => sym }
      .flatMap { sym =>
        val encodedSymName = NameTransformer.encode(sym.name.toString)
        val Symbol = s"${Regex.quote(encodedSymName)}\\$$?(.*)".r
        encodedName match
          case Symbol(remaining) =>
            if remaining.isEmpty then Some(sym)
            else findSymbolsRecursively(sym, remaining)
          case _ => None
      }

  private def matchSymbol(method: jdi.Method, symbol: TermSymbol, isExtensionMethod: Boolean): Boolean =
    matchTargetName(method, symbol, isExtensionMethod) &&
      matchSignature(method, symbol, isExtensionMethod)

  def matchTargetName(
      method: jdi.Method,
      symbol: TermSymbol,
      isExtensionMethod: Boolean
  ): Boolean =
    val javaPrefix = method.declaringType.name.replace('.', '$') + "$$"
    // if an inner accesses a private method, the backend makes the method public
    // and prefixes its name with the full class name.
    // Example: method foo in class example.Inner becomes example$Inner$$foo
    val expectedName = method.name.stripPrefix(javaPrefix)
    val encodedScalaName = NameTransformer.encode(symbol.targetName.toString)
    if isExtensionMethod then encodedScalaName == expectedName.stripSuffix("$extension")
    else encodedScalaName == expectedName

  def matchSignature(
      method: jdi.Method,
      symbol: TermSymbol,
      isExtensionMethod: Boolean
  ): Boolean =
    try {
      symbol.signedName match
        case SignedName(_, sig, _) =>
          val javaArgs = method.arguments.headOption.map(_.name) match
            case Some("$this") if isExtensionMethod => method.arguments.tail
            case _ => method.arguments
          matchArguments(sig.paramsSig, javaArgs) &&
          method.returnType.forall(matchType(sig.resSig, _))
        case _ =>
          true // TODO compare symbol.declaredType
    } catch {
      case e: UnsupportedOperationException =>
        warn(e.getMessage)
        true
    }

  private def matchArguments(
      scalaArgs: Seq[ParamSig],
      javaArgs: Seq[jdi.LocalVariable]
  ): Boolean =
    scalaArgs
      .collect { case termSig: ParamSig.Term => termSig }
      .corresponds(javaArgs) { (scalaArg, javaArg) =>
        matchType(scalaArg.typ, javaArg.`type`)
      }

  private val javaToScala: Map[String, String] = Map(
    "scala.Boolean" -> "boolean",
    "scala.Byte" -> "byte",
    "scala.Char" -> "char",
    "scala.Double" -> "double",
    "scala.Float" -> "float",
    "scala.Int" -> "int",
    "scala.Long" -> "long",
    "scala.Short" -> "short",
    "scala.Unit" -> "void",
    "scala.Any" -> "java.lang.Object",
    "scala.Null" -> "scala.runtime.Null$",
    "scala.Nothing" -> "scala.runtime.Nothing$"
  )

  private def matchType(
      scalaType: FullyQualifiedName,
      javaType: jdi.Type
  ): Boolean =
    def rec(scalaType: String, javaType: String): Boolean =
      scalaType match
        case "scala.Any[]" =>
          javaType == "java.lang.Object[]" || javaType == "java.lang.Object"
        case s"$scalaType[]" => rec(scalaType, javaType.stripSuffix("[]"))
        case _ =>
          val regex = scalaType
            .split('.')
            .map(NameTransformer.encode)
            .map(Regex.quote)
            .mkString("", "[\\.\\$]", "\\$?")
            .r
          javaToScala
            .get(scalaType)
            .map(_ == javaType)
            .getOrElse(regex.matches(javaType))
    rec(scalaType.toString, javaType.name)

  private def skip(symbol: TermSymbol): Boolean =
    val isNonLazyGetterOrSetter =
      (!symbol.flags.is(Flags.Method) || symbol.is(Flags.Accessor)) &&
        !symbol.is(Flags.Lazy)
    isNonLazyGetterOrSetter || symbol.is(Flags.Synthetic)
