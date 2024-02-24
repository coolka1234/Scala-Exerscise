import java.lang.reflect.Field

trait Debug {
  def debugVars(): Unit = {
    val fields = getClass.getDeclaredFields

    def printingFields(fields:Array[Field]): Unit = {
      if (!fields.isEmpty)
      {
        val pole = fields.head
        pole.setAccessible(true)
        println("Pole: " + pole.getName + " => " + pole.getType + ", " + pole.get(this))
        printingFields(fields.tail)
      }
    }
    printingFields(fields)
  }

  def debugName(): Unit = {
    val nazwaKlasy = getClass.getSimpleName
    println("Klasa: " + nazwaKlasy)
  }
}

class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}
class Kot(im: String,wi: Int,ko: String)extends Debug{
  var imie:String=im
  var kolor:String=ko
  var wiek:Int=wi
}

@main
def main(): Unit = {
  println("Hello world!")
  val point1 = new Point(300000, 12345)
  val kot1=new Kot("Lapek",5,"Czarny")
  kot1.debugName()
  kot1.debugVars()
  point1.debugName()
  point1.debugVars()

  val point2 = new Point(0, 0)
  val kot2 = new Kot("Kocislaw", 5, "Czarny")
  kot2.debugName()
  kot2.debugVars()
  point2.debugName()
  point2.debugVars()

  val point3 = new Point(-1,-3)
  val kot3 = new Kot("", 100, "Gniady")
  kot3.debugName()
  kot3.debugVars()
  point3.debugName()
  point3.debugVars()
}