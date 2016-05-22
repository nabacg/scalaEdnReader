package net.cabworks.edn

import java.util.UUID
import org.scalatest.FunSuite

/**
 * Created by cab on 22/05/2016.
 */
class ReaderTest extends FunSuite {
  def testParser (str : String) = Reader.read(str)

  def uuid: UUID =UUID.randomUUID()
  def uuidFrom(input: String): UUID = UUID.fromString(input)

  val sym = Symbol(_)

  test("integers") {
    assertResult(0) {testParser("0")}
    assertResult(1) {testParser("0001")}
    assertResult(23) {testParser("23")}
    assertResult(1) {testParser("1")}
    assertResult(-43) { testParser("-43")}
  }
  test("floating point") {
    assertResult(53.1) {testParser("53.1")}
    assertResult(1.5) {testParser("1.5")}
    assertResult(0.0005) {testParser("0.0005")}
    assertResult(0.5) {testParser("0.5000")}
    assertResult(-3.14) {testParser("-3.14")}
  }

  test("equality") {
    assertResult(true) {
      val i = testParser("2")
      val d = testParser("2.0000000000000")
      i == d
    }

    assertResult(false) {
      val i = testParser("2")
      val d = testParser("2.0000000000001")
      i == d
    }
  }

  test("nil ") {
    assertResult(null) { testParser("nil")}
  }

  test("booleans") {
    assertResult(true) { testParser("true")}
    assertResult(false) { testParser("false")}
  }

  test("string") {
    assertResult("1") { testParser("\"1\"")}
    assertResult("AA") { testParser("\"AA\"")}
    assertResult("") { testParser("\"\"")}
  }

  test("ratios") {
    assertResult(0.5) { testParser("1/2")}
    assertResult(0.5) { testParser("6/12")}
    assertResult(Set(0.5, 1)) { testParser("#{1/2 42/42}") }
  }

  test("dates aka #inst") {
    val dateString = "2015-07-30T01:23:45.000-00:00"
    assertResult(InstantReader.read(dateString)) { testParser(f"""#inst \"$dateString\"""")}

    val dateStringNoOffset = "1985-04-12T23:20:50.52Z"
    assertResult(InstantReader.read(dateString)) {testParser(f"""#inst \"$dateString\"""")}
  }

  test("#uuid tagged elem") {
    val testUuid = uuid
    assertResult(testUuid) { testParser(s"""#uuid \"${testUuid}\"""") }
    assertResult(uuidFrom("f81d4fae-7dec-11d0-a765-00a0c91e6bf6")) { testParser("#uuid \"f81d4fae-7dec-11d0-a765-00a0c91e6bf6\"")}
  }

  test("List") {
    assertResult(List()){testParser("()")}
    assertResult(List(1,2,3)) {testParser("(1 2 3)")}
    assertResult(List(1,2,3)) {testParser("(1,2,3)")}
  }

  test("vectors ") {
    assertResult(Vector(1)) { testParser("[1]")}
    assertResult(Vector(1,2, 3)) { testParser("[1 2 3]")}
    assertResult(Vector()) { testParser("[]")}
    assertResult(Vector(1,2,3)) {testParser("[1,2,3]")}
  }

  test("maps") {
    assertResult(Map(1 -> 12))(testParser("{ 1 12}") )

    assertResult(Map(1 -> 12, 2 -> 231))( testParser("{ 1 12 2 231}"))
    assertResult(Map())( testParser("{}"))
  }

  test("sets") {
    assertResult(Set(1, 2, 3))(testParser("#{1 2 3}") )

    assertResult(Set())( testParser("#{}"))
  }


  test("keywords and symbols") {
    assertResult(Symbol("a")) { testParser("a") }
    assertResult(Symbol("f")) { testParser("f") }
    assertResult(Symbol("test-namespace/state")) { testParser("test-namespace/state") }

    assertResult(Symbol(":a")) { testParser(":a") }
    assertResult(Symbol("::f")) { testParser("::f") }
    assertResult(Symbol(":test/asd")) { testParser(":test/asd") }

    assertResult(List(":a", ":b", ":c", ":d").map(Symbol.apply)) { testParser("(:a :b :c :d )")}

    assertResult(List("a", "b", "c", "d").map(Symbol.apply)) { testParser("(a b c d )")}
    assertResult(Vector("a", "b", "c", "d").map(Symbol.apply)) { testParser("[a b c d ]")}
  }

  test("arithmetic") {
    assertResult(List(Symbol("+"), 2, 2)) {
      testParser("(+ 2 2)")
    }
    assertResult(List(Symbol("-"), 2, 2)) {
      testParser("(- 2 2)")
    }
    assertResult(List(Symbol("/"), 2, 2)) {
      testParser("(/ 2 2)")
    }
    assertResult(List(Symbol("*"), 2, 2)) {
      testParser("(* 2 2)")
    }
  }

  test("quoted lists") {
    assertResult(List(Symbol("quote"), List(1, 2, 2))) {
      testParser("'(1 2 2)")
    }
    assertResult(List(Symbol("quote"), List(Symbol("+"), 2, 2))) {
      testParser("'(+ 2 2)")
    }

  }

  test("defs") {
    assertResult(List(sym("def"), sym("a"), 1)) {
      testParser("(def a 1)")
    }
  }

  test("fns") {
    assertResult(List(Symbol("fn"), Vector(Symbol("a"), Symbol("b")), List(Symbol("+"), Symbol("a"), Symbol("b")))) {

      testParser("(fn [a b] (+ a b))")
    }
  }

}
