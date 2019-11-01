package com.yoppworks.ossum.riddl.language

import java.io.File

import fastparse._
import ScalaWhitespace._
import com.yoppworks.ossum.riddl.language.AST._
import Terminals.Keywords
import Terminals.Punctuation
import Terminals.Readability

import scala.reflect.runtime.universe._

/** Common Parsing Rules */
trait CommonParser extends NoWhiteSpaceParsers {

  def undefined[_: P]: P[Unit] = {
    P(Punctuation.undefined /)
  }

  def docBlock[_: P]: P[Seq[LiteralString]] = {
    P(
      (open ~ literalString.rep ~ close) |
        (open ~ markdownLine.rep ~ close) |
        literalString.map(Seq(_))
    )
  }

  def optionalNestedContent[_: P, T](parser: => P[T]): P[Seq[T]] = {
    P(open ~ parser.rep ~ close).?.map(_.getOrElse(Seq.empty[T]))
  }

  def seeAlso[_: P]: P[SeeAlso] = {
    P(
      location ~ "see" ~ "also" ~ docBlock
    ).map(tpl => (SeeAlso.apply _).tupled(tpl))
  }

  def explanation[_: P]: P[Explanation] = {
    P(
      location ~
        "explained" ~ "as" ~/ docBlock
    ).map(tpl => { Explanation.apply _ }.tupled(tpl))
  }

  def addendum[_: P]: P[Option[Addendum]] = {
    P(location ~ explanation.? ~ seeAlso.?).map[Option[Addendum]] {
      case (_, None, None)                    => None
      case (loc, exp @ Some(_), None)         => Some(Addendum(loc, exp, None))
      case (loc, exp @ Some(_), sa @ Some(_)) => Some(Addendum(loc, exp, sa))
      case (loc, None, sa @ Some(_))          => Some(Addendum(loc, None, sa))
    }
  }

  def literalInteger[_: P]: P[LiteralInteger] = {
    P(location ~ CharIn("0-9").rep(1).!.map(_.toInt))
      .map(s => LiteralInteger(s._1, BigInt(s._2)))
  }

  def literalDecimal[_: P]: P[LiteralDecimal] = {
    P(
      location ~
        CharIn("+\\-").?.! ~ CharIn("0-9").rep(1).! ~
        (Punctuation.dot ~ CharIn("0-9").rep(0)).?.! ~
        ("E" ~ CharIn("+\\-") ~ CharIn("0-9").rep(min = 1, max = 3)).?.!
    ).map {
      case (loc, a, b, c, d) => LiteralDecimal(loc, BigDecimal(a + b + c + d))
    }
  }

  def simpleIdentifier[_: P]: P[String] = {
    P((CharIn("a-zA-Z") ~~ CharsWhileIn("a-zA-Z0-9_").?).!)
  }

  def quotedIdentifier[_: P]: P[String] = {
    P("'" ~/ CharsWhileIn("a-zA-Z0-9_+\\-|/@$%&, :", 1).! ~ "'")
  }

  def anyIdentifier[_: P]: P[String] = {
    P(simpleIdentifier | quotedIdentifier)
  }

  def identifier[_: P]: P[Identifier] = {
    P(location ~ anyIdentifier).map(tpl => (Identifier.apply _).tupled(tpl))
  }

  def pathIdentifier[_: P]: P[PathIdentifier] = {
    P(location ~ anyIdentifier.repX(1, P(Punctuation.dot)))
      .map(tpl => (PathIdentifier.apply _).tupled(tpl))
  }

  def is[_: P]: P[Unit] = {
    P(Readability.is | Readability.are | Punctuation.colon | Punctuation.equals)./.?
  }

  def open[_: P]: P[Unit] = {
    P(
      Punctuation.curlyOpen
    )
  }

  def close[_: P]: P[Unit] = {
    P(Punctuation.curlyClose)
  }

  def options[_: P, TY <: RiddlValue](
    validOptions: => P[String]
  )(mapper: => (Location, String) => TY): P[Seq[TY]] = {
    P(
      (Keywords.options ~/ Punctuation.roundOpen ~ (location ~ validOptions)
        .rep(1)
        .map(_.map(mapper.tupled(_))) ~ Punctuation.roundClose) |
        (Keywords.option ~ is ~/ (location ~ validOptions).map(
          tpl => Seq(mapper.tupled(tpl))
        ))
    ).?.map {
      case Some(x) => x
      case None    => Seq.empty[TY]
    }
  }

  def mapTo[T <: Definition](seq: Option[Seq[Definition]]): Seq[T] = {
    seq.map(_.map(_.asInstanceOf[T])).getOrElse(Seq.empty[T])
  }

  def commandRef[_: P]: P[CommandRef] = {
    P(location ~ Keywords.command ~/ identifier)
      .map(tpl => (CommandRef.apply _).tupled(tpl))
  }

  def eventRef[_: P]: P[EventRef] = {
    P(location ~ Keywords.event ~/ identifier)
      .map(tpl => (EventRef.apply _).tupled(tpl))
  }

  def queryRef[_: P]: P[QueryRef] = {
    P(location ~ Keywords.query ~/ identifier)
      .map(tpl => (QueryRef.apply _).tupled(tpl))
  }

  def resultRef[_: P]: P[ResultRef] = {
    P(location ~ Keywords.result ~/ identifier)
      .map(tpl => (ResultRef.apply _).tupled(tpl))
  }

  def messageRef[_: P]: P[MessageReference] = {
    P(commandRef | eventRef | queryRef | resultRef)
  }

  def entityRef[_: P]: P[EntityRef] = {
    P(location ~ Keywords.entity ~/ identifier)
      .map(tpl => (EntityRef.apply _).tupled(tpl))
  }

  def topicRef[_: P]: P[TopicRef] = {
    P(location ~ Keywords.topic ~/ identifier)
      .map(tpl => (TopicRef.apply _).tupled(tpl))
  }

  def functionRef[_: P]: P[FunctionRef] = {
    P(location ~ Keywords.function ~/ identifier)
      .map(tpl => (FunctionRef.apply _).tupled(tpl))
  }

  def contextRef[_: P]: P[ContextRef] = {
    P(location ~ Keywords.context ~/ identifier)
      .map(tpl => (ContextRef.apply _).tupled(tpl))
  }

  def domainRef[_: P]: P[DomainRef] = {
    P(location ~ Keywords.domain ~/ identifier)
      .map(tpl => (DomainRef.apply _).tupled(tpl))
  }
}
