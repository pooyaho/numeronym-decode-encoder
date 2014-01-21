package ir.phsys.coding

object Main extends App {

  import EncoderDecoder._


  override def main(args: Array[String]) = {
    println(encode("Poooooooooooooooooooppooya asdaas"))
    println(encode("Poya"))
    println(encode("Pya"))
    println(encode("Pa"))
    println(encode("P"))

    println(List(1).foldRight("") {
      (a, b) => a + 12 + b
    })
  }

}