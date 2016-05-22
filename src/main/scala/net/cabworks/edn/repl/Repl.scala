package net.cabworks.edn.repl

import net.cabworks.edn.Reader

import scala.io.StdIn

/**
 * Created by cab on 22/05/2016.
 */
object Repl {

  def replLoop : Unit = {
    val input = StdIn.readLine("Edn>")
    input match {
      case in : String if in.length() > 0 => {
        val result = Reader.read(in)
        println(result)
        replLoop
      }
      case _ => ()
    }
  }

  def main(args: Array[String]) = replLoop
}
