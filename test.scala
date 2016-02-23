import scala.collection.mutable.HashMap
import Chemistry.Element
import Chemistry.Compound
import Chemistry.PeriodicTable
object test{
  def main(args:Array[String]){
    var c = new Compound()
    val mystring = "ONeC12(Ag12F(O2)5)12F12"
    //val mystring = "HO2"
    println(mystring)
    c.makeCompound(mystring)
    c.percentComposition() foreach { case(key,value) => print(key + " : " + value + "%")}
  }
}
