package com.reactific.riddl.translator.codegen

import com.reactific.riddl.language.AST._
import com.reactific.riddl.language.SymbolTable
import com.reactific.riddl.translator.codegen.repr.Repr.TypeExpressionOps
import com.reactific.riddl.utils.TextFileWriter

import java.nio.file.Path

case class CodegenWriter(
  filePath: Path,
  packages: Seq[String],
  parents: Seq[Definition],
  symTab: SymbolTable)
    extends TextFileWriter {
  self =>

  def emitCodegenFileHeader: CodegenWriter = {
    sb.append(s"package ${packages.mkString(".")}\n\n")

    // TODO Import the required packages
    sb.append("import akka.actor.typed.scaladsl.Behaviors\n")
    sb.append("import akka.actor.typed.{ ActorSystem, Behavior }\n\n")

    self
  }

  def emitMessageType(typ: Type): CodegenWriter = {
    require(typ.isMessageKind, "Not a message kind")
    val id: Identifier = typ.id
    val ty: MessageType = typ.typ.asInstanceOf[MessageType]

    val code = ty.toCode.apply(id)
    sb.append(code)

    this
  }
}
