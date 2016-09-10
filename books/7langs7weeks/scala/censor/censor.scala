trait Censor {
  def censor(original:String):String = {
    val censor_map = Map(
      "Shoot" -> "Pucky",
      "Darn" -> "Beans"
    )

    var newstring = original;

    censor_map.foreach { item =>
        var (badword, goodword) = item

        newstring = newstring.replaceAll("(?i)" + badword, goodword)
    }

    return newstring
  }
}

class Person(name:String) extends Censor {
  def introduce {
    println(censor("My name is " + name + ", darn..."))
  }

  def curse {
    println(censor("Shoot!"))
  }
}

object Entry {
  def main(args:Array[String]) {
    val lawrence = new Person("Lawrence")
    lawrence.introduce
    lawrence.curse
  }
}
