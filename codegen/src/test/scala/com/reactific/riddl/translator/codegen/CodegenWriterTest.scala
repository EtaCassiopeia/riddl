package com.reactific.riddl.translator.codegen

import com.reactific.riddl.language.AST._
import com.reactific.riddl.language.SymbolTable
import com.reactific.riddl.language.parsing.FileParserInput
import com.reactific.riddl.language.testkit.ParsingTest
import org.scalatest.BeforeAndAfterAll

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.io.Source
import org.apache.commons.io.FileUtils

class CodegenWriterTest extends ParsingTest with BeforeAndAfterAll {

  val testDir: Path = Files.createTempDirectory("codegen-writer")

  override def afterAll(): Unit = {
    super.afterAll()
    FileUtils.deleteDirectory(testDir.toFile)
  }

  def run(domainInput: String): (CodegenWriter, Domain) = {
    val input =
      s"domain foo is {\n  options(package(\"com.foo\"))\n\n$domainInput\n}"

    parseTopLevelDomains(input) match {
      case Left(error) => fail(error.map(_.format).mkString("\n"))
      case Right(root) =>
        val pkgs = Seq("com", "foo", "api")
        val path = Paths
          .get("src", Seq("main") ++ pkgs :+ "Foo.scala": _*)
        val file = testDir.resolve(path)
        val symTab = SymbolTable(root)
        CodegenWriter(file, pkgs, Seq.empty[Definition], symTab) -> root.contents.head
    }
  }

  def load(gw: CodegenWriter): String = {
    gw.write()
    println(s"Reading from: ${gw.filePath.toFile}:")
    val writtenContent = Source.fromFile(gw.filePath.toFile, "utf-8")
    val content = writtenContent.getLines().mkString("\n")
    writtenContent.close()
    println(content)
    content
  }

  "CodegenWriter" must {
    "emit a codegen file header" in {
      val (gw, _) = run("")
      gw.emitCodegenFileHeader
      val content = load(gw)
      val expected: String = """package com.foo.api
                               |
                               |import akka.actor.typed.scaladsl.Behaviors
                               |import akka.actor.typed.{ ActorSystem, Behavior }
                               |""".stripMargin
      content must be(expected)
    }

    "emit a simple message type" in {
      val input = """type bar is command {
                    |  num: Number?,
                    |  str: String,
                    |  bool: Boolean,
                    |  int: Integer,
                    |  dec: Decimal,
                    |  real: Real,
                    |  date: Date,
                    |  time: Time,
                    |  dateTime: DateTime,
                    |  ts: TimeStamp,
                    |  dur: Duration,
                    |  latLong: LatLong,
                    |  url: URL
                    |}
                    |""".stripMargin
      val (gw, domain) = run(input)
      val typ = domain.types.head
      gw.emitMessageType(typ)
      val content = load(gw)
      val expected = "case class Bar (num: Option[Double],str: String,bool: Boolean,int: Int,dec: String,real: Long,date: java.time.LocalDate,time: java.time.LocalTime,dateTime: java.time.DateTime,ts: Long,dur: scala.concurrent.duration.Duration,latLong: NotDefined,url: java.net.URL)"
      content must be(expected)
    }


/*
    "emit an event sourced entity for a riddl entity" in {
      val input = new FileParserInput(
        Path.of("codegen/src/test/input/entity/event-sourced/ExampleApp.riddl")
      )
      parseTopLevelDomains(input) match {
        case Left(errors) => fail(errors.map(_.format).mkString("\n"))
        case Right(root) =>
          val packages = Seq("com", "example", "app", "organization", "api")
          val path = Paths
            .get("src", "main", "proto")
            .resolve(Paths.get(packages.head, packages.tail:_*))
            .resolve("Organization.proto")
          val file = testDir.resolve(path)
          val symtab = SymbolTable(root)
          val gw = CodegenWriter(file, packages, Seq.empty[Parent], symtab)
          val context: Context = root.contents.head.includes.head.contents.head
            .asInstanceOf[Context]
          val entity = context.entities.head
          gw.emitCodegenFileHeader
          gw.emitEntityTypes(entity)
          gw.emitEntityApi(entity, packages)
          gw.write()
          val writtenContent = Source.fromFile(file.toFile, "utf-8")
          val content = writtenContent.getLines().mkString("\n")
          writtenContent.close()
          val expected =
            """syntax = "proto3";
              |
              |package com.example.app.organization.api;
              |
              |import "google/api/annotations.proto";
              |import "kalix/annotations.proto";
              |import "validate/validate.proto";
              |import "google/protobuf/empty.proto";
              |
              |message OrganizationId {
              |  string orgId = 1 [(kalix.field).entity_key = true];
              |}
              |
              |message Address {
              |  // maybe put this in a "improving.app.api" package?
              |}
              |
              |message OrganizationInfo {
              |  string name = 1; //i.e. Provo High School. Must be unique within the organizational structure.
              |  string shortName = 2; //i.e. PHS
              |  Address address = 3; //required for BaseOrg. Optional for all other organizations.
              |  bool isPrivate = 4; //defaults to true
              |  string url = 5;
              |  OrganizationId parentOrg =6; //BaseOrganizations do not have a parent. All other organizations must have
              |   a parent. The BaseOrganization (only one per organizational structure) is the financially responsible
              |   party.
              |}
              |
              |message EstablishOrganization {
              |  OrganizationId orgId = 1;
              |  OrganizationInfo info = 2;
              |}
              |
              |message OrganizationEstablished {
              |  OrganizationId orgId = 1;
              |  OrganizationInfo info = 2;
              |  sint64 timestamp = 3;
              |}
              |
              |service OrganizationService {
              |  option (kalix.codegen) = {
              |    event_sourced_entity: {
              |      name: "com.improving.app.organization.domain.Organization"
              |      entity_type: "organization"
              |      state: "com.improving.app.organization.domain.OrgState"
              |      events: [
              |        "com.improving.app.organization.api.OrganizationEstablished"
              |      ]
              |    }
              |  };
              |
              |  rpc establishOrganization (EstablishOrganization) returns (OrganizationEstablished) { }
              |
              |}""".stripMargin
          content must be(expected)

      }
    }
*/

    /*
    "emit an action for a service/gateway context" in { pending }
     */
  }
}
