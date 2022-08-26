package com.reactific.riddl.translator.codegen

import com.reactific.riddl.language.AST._
import com.reactific.riddl.language._
import com.reactific.riddl.utils.Logger

import java.nio.file.Path
import scala.collection.mutable

case class CodegenOptions(
  inputFile: Option[Path] = None,
  outputDir: Option[Path] = None,
  kalixPath: Option[Path] = None,
  projectName: Option[String] = None,
  withValidations: Boolean = false)
    extends TranslatingOptions

object CodegenOptions {
  val default: CodegenOptions = CodegenOptions()
}

case class CodegenState(
  options: CodegenOptions,
  symTab: SymbolTable)
    extends TranslatorState[CodegenWriter] {}

class CodegenTranslator extends Translator[CodegenOptions] {

  def parents(stack: Seq[Definition]): Seq[Definition] = {
    val result = stack.reverse.dropWhile(_.isRootContainer)
    result
  }

  def packages(stack: Seq[Definition]): Seq[String] = {
    stack.flatMap {
      case d: Domain => d.getOptionValue[DomainPackageOption] match {
          case Some(pkg) => pkg.map(_.s.toLowerCase)
          case None      => Seq(d.id.value.toLowerCase())
        }
      case c: Context => c.getOptionValue[ContextPackageOption] match {
          case Some(pkg) => pkg.map(_.s.toLowerCase)
          case None      => Seq(c.id.value.toLowerCase())
        }
      case _ =>
        // Others don't have package specifications
        Seq.empty[String]
    }
  }

  def setUp(
    c: Definition,
    options: CodegenOptions,
    state: CodegenState,
    stack: Seq[Definition],
    isApi: Boolean = false
  ): CodegenWriter = {
    state.addDir(c.id.format)
    val pars: Seq[Definition] = parents(stack)
    val pkgs = packages(pars) :+ (if (isApi) "api" else "domain")
    val prefix = Seq("main") ++ pkgs
    val name = prefix :+ c.id.value
    val path = Path.of("src", name: _*)
    val fullPath: Path = options.outputDir.get.resolve(path)
    val writer = CodegenWriter(fullPath, pkgs, pars, state.symTab)
    state.addFile(writer)
    writer
  }

  override protected def translateImpl(
    root: AST.RootContainer,
    log: Logger,
    commonOptions: CommonOptions,
    options: CodegenOptions
  ): Seq[Path] = {
    val paths = super.translateImpl(root, log, commonOptions, options)
    require(options.projectName.nonEmpty, "A project name must be provided")

    val state = CodegenState(options, SymbolTable(root))
    val parentStack = mutable.Stack[Definition]()

    val newState = Folding.foldLeftWithStack(state, parentStack)(root) {
      case (st, d: AST.Domain, stack) =>
        // val writer = setUp(d, options, st, stack)
        // writer.emitTypes(d.collectMessages)
        st
      case (st, c: AST.Context, stack) =>
        // val writer = setUp(c, options, st, stack)
        // writer.emitTypes(c.collectMessages)
        st
      case (st, e: AST.Entity, stack) =>
        // val writer = setUp(e, options, st, stack)
        // writer.emitEntityApi(e)
        // writer.emitEntityImpl(e)
        st
      case (st, _, _) => // skip, handled by the MarkdownWriter
        st
    }
    paths ++ newState.close
  }

}
