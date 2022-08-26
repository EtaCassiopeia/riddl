package com.reactific.riddl.translator.codegen.repr
import com.reactific.riddl.language.AST
import com.reactific.riddl.language.AST._

trait Repr[A <: TypeExpression] {
  def toCode(tye: A): Identifier => String
  def kind(tye: A): String
}

object Repr {

  def apply[T <: TypeExpression](implicit repr: Repr[T]): Repr[T] = repr

  implicit class FieldOps(field: Field) {
    def toCode[F <: AST.Field](
      implicit repr: Repr[field.Aux],
      ev: field.Aux <:< TypeExpression
    ): String = repr.toCode(field.typeEx)(field.id)
  }

  implicit class TypeExpressionOps[T <: TypeExpression](tye: T) {
    def toCode(implicit repr: Repr[T]): Identifier => String = repr.toCode(tye)

    def kind(implicit repr: Repr[T]): String = repr.kind(tye)
  }

  private def createPredefinedInstance[T <: PredefinedType](): Repr[T] = {
    val typeMapping = Map(
      "String" -> "String",
      "Boolean" -> "Boolean",
      "Number" -> "Double",
      "Decimal" -> "String",
      "Integer" -> "Int",
      "Real" -> "Long",
      "Date" -> "java.time.LocalDate",
      "Time" -> "java.time.LocalTime",
      "DateTime" -> "java.time.DateTime",
      "TimeStamp" -> "Long",
      "Duration" -> "scala.concurrent.duration.Duration",
      "UUID" -> "java.util.UUID",
      "URL" -> "java.net.URL",
      "LatLong" -> "NotDefined", // TODO Define LatLong type
      "Abstract" -> "NotDefined", // TODO Define Abstract type
      "Nothing" -> "NotDefined" // TODO Define Nothing type
    )

    new Repr[T] {
      def kind(tye: T): String = typeMapping(tye.kind)
      def toCode(tye: T): Identifier => String =
        (id: Identifier) => s"${sanitizeId(id)}: ${kind(tye)}"
    }
  }

  // PredefinedType
  implicit def reprPredefinedType: Repr[PredefinedType] =
    createPredefinedInstance[PredefinedType]()

//  implicit val reprTypeRef: Repr[TypeRef] = ???
//  implicit val reprCardinality: Repr[Cardinality] = ???
//  implicit val reprZeroOrMore: Repr[ZeroOrMore] = ???
//  implicit val reprOneOrMore: Repr[OneOrMore] = ???
//  implicit val reprEnumeration: Repr[Enumeration] = ???
//  implicit val reprAlternation: Repr[Alternation] = ???
//  implicit val reprAggregation: Repr[Aggregation] = ???
//  implicit val reprMapping: Repr[Mapping] = ???
//  implicit val reprRangeType: Repr[RangeType] = ???
//  implicit val reprReferenceType: Repr[ReferenceType] = ???
//  implicit val reprPattern: Repr[Pattern] = ???
//  implicit val reprUniqueId: Repr[UniqueId] = ???
//  implicit val reprSpecificRange: Repr[SpecificRange] = ???
//  implicit val reprAggregateTypeExpression: Repr[AggregateTypeExpression] = ???

  // Cardinality
  implicit val reprOptional: Repr[Optional] = new Repr[Optional] {

    override def kind(tye: Optional) = "Option"
    override def toCode(tye: Optional): Identifier => String = tye match {
      case Optional(_, boxedTye) =>
        (id: Identifier) => s"${sanitizeId(id)}: Option[${boxedTye.kind}]"
    }
  }

  // MessageType
  implicit val reprMessageType: Repr[MessageType] = new Repr[MessageType] {
    override def kind(tye: MessageType): String = "product"
    override def toCode(
      tye: MessageType
    ): Identifier => String = { (id: Identifier) =>
      val fields = tye.fields
      s"case class ${sanitizeId(id).capitalize} (${fields.map(_.toCode).mkString(",")})"
    }
  }

  // TODO remove after adding the missing instances
  implicit def allTypeExpressions[T <: TypeExpression]: Repr[T] = new Repr[T] {

    private def repr(tye: T): Repr[T] = tye match {
      case _: Optional       => Repr[Optional].asInstanceOf[Repr[T]]
      case _: MessageType    => Repr[MessageType].asInstanceOf[Repr[T]]
      case _: PredefinedType => Repr[PredefinedType].asInstanceOf[Repr[T]]
      case _ => new Repr[TypeExpression] {
          def toCode(tye: TypeExpression): Identifier => String = id =>
            s"A type class instance is missing for [${sanitizeId(id)}] with type ${tye.format}"
          def kind(tye: TypeExpression): String = "Unknown"
        }.asInstanceOf[Repr[T]]
    }
    override def toCode(
      tye: T
    ): Identifier => String = repr(tye).toCode(tye)
    override def kind(tye: T): String = repr(tye).kind(tye)
  }

  private def sanitize(s: String): String = {
    s // TODO: remove non-identifier chars?
  }

  private def sanitizeId(id: Identifier): String = { sanitize(id.value) }

}
