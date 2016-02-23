/*
Author: Charlie R. Hicks
Purpose: Solves Chemistry problems
*/

import scala.collection.mutable.Map
import scala.collection.mutable.Stack
import scala.collection.mutable.HashMap
import scala.util.control.Exception._
//The basic utilites to deal with the problems are encapulated here
package Chemistry{
 object Utilities{
    def parseDouble( s:String) = { try{ s.toDouble}  catch { case _  :Throwable => 0.00}}
    def parseInt( s:String) = { try{s.toInt} catch { case _ :Throwable => 0}}
    def isStringNumber(s :String):Boolean = { s forall Character.isDigit}
 }
 //This will be able to accept a valid compound
 class Compound(){
  /Every element symbol intend to improve this later.
  val validElement = "(H|He|Li|Be|B|C|N|O|F|Ne|Na|Mg|Al|Si|P|S|Cl|Ar|K|Ca|Sc|Ti|V|Cr|Mn|Fe|Co|Ni|Cu|Zn|Ga|Ge|As|Se|Br|Kr|Rb|Sr|Y|Zr|Nb|Mo|Tc|Ru|Rh|Pd|Ag|Cd|In|Sn|Sb|Te|I|Xe|Cs|Ba|La|Ce|Pr|Nd|Pm|Sm|Eu|Gd|Tb|Dy|Ho|Er|Tm|Yb|Lu|Hf|Ta|W|Re|Os|Ir|Pt|Au|Hg|Tl|Pb|Bi|Po|At|Rn|Fr|Ra|Ac|Th|Pa|U|Np|Pu|Am|Cm|Bk|Cf|Es|Fm|Md|No|Lr|Rf|Db|Sg|Bh|Hs|Mt|Ds|Rg|Cn|Uut|Uuq|Uup|Uuh|Uus|Uuo)".r
  //Parens, numbers, Uppercase letters followed by 0-2 lowercase
  val chemistryTokens = "([A-Z][a-z]{0,2}|[0-9]+|[(]|[)])".r
  
  val number = "[0-9]+".r
  //Holds the Element symbol and number of each element in the compound
  var elms = new HashMap[String,Int]()
  @throws[Exception]("If String is Invalid")
  //this method parses to see if a string is a valid compound
  def makeCompound(s:String){
      val tokens = (chemistryTokens findAllIn s).toList
      //valid states
      // start - s
      // expecting int - ei
      // not start - ns
      // stack
      var stack = Stack[Int]()
      var elements = Stack[Tuple2[String,Int]]()
      var e = ("placeholder",1)
      var state  = "s"
      for(token <- tokens){
       //start of expression
       if(state == "s"){
        token match {
 	  case validElement(_*) => {
	       e = (token,1)
	       elements.push(e)
	       state = "ns"     
               if(!(stack.isEmpty)){
                 stack.push(stack.pop + 1)
               }
	  }
	  case "(" => { 
            stack.push(0)
          }
          case _ => {println("Invalid" + token + " " + state)}
        }
	}
       
       else if(state == "ei"){
       	token match {
	 case number(_*) => {
	   state = "ns"
	   var tmp = elements.take(stack.head).toArray
	   for(x <- 1 to stack.head) { elements.pop()}
	   for( element <- tmp){
	     elements.push((element._1,element._2*token.toInt))
	   }
	   if(stack.size>1){
             stack.push(stack.pop + stack.pop)
           }
	   else {
             stack.pop
           }
	 }
	 case _ => { println("Invalid" + token + " " + state)}
	}
       }
       else if(state == "ns"){
       	    token match{
        case validElement(_*) => {
          e = (token,1)
	  elements.push(e)
	  if(!(stack.isEmpty)){
	  stack.push(stack.pop()+1)
	  }
	   e = (token,1)
	}
	case number(_*) => {
	  elements.push((elements.head._1,elements.pop()._2*token.toInt))	  
	}
	case "(" => {
	   stack.push(0)
	   state = "s"
	}
	case ")" =>
	{
	  state = "ei"
	}
       }}
       else{
        throw new Exception("Invalid Compound")
}

     }
     elements.foreach( (x: Tuple2[String, Int]) => { elms+=(x._1 -> (x._2 + elms.getOrElse(x._1,0)))})     
  }
  //shorthand for the percent coposition function written below
  def pc(){
      percentComposition()
  }
  //returns a HashMap
  //total gives total molecular weight 
  def percentComposition() = {
      var pce = new HashMap[String,Double]()
      var total = 0.0
      //find the mass for each element
      elms foreach { case (key,value) => pce += ( key -> value * PeriodicTable.getByElementSymbol(key).get.atomicWeight)}
      //sum the total mass in grams
      pce foreach { case(key,value) => total = total + value}
      //I've found that these are the best percisions to give values that add up to 100
      pce foreach { case(key,value) => pce += (key -> (Math.round((value / total) * 1000000)) / 10000.00)}
      //Save total into the HashMap
      //this is done last to prevent any of the previous maps from modifying it. 
      pce += ("Total" -> total)
      pce
  }
  //Returns a HashMap where keys are element names and value is the number of them
  def getElements() =
  {
  
   elms
  }
 }
 //This class has every single attribute an element can possess
 //it is later used in the periodic table singleton below
class Element(atomicNumberp: Int, elementNamep: String, symbolp: String, atomicWeightp: Double, periodp: Int, groupp: Int, phasep: String, mostStableCrystalp: String, typeOfElementp: String, ionicRadiusp: Double, atomicRadiusp: Double, electronegativityp: Double, firstIonizationPotentialp: Double, densityp: Double, meltingPointp: Double, boilingPointp:Double, isotopesp: Int, yearOfDiscoveryp: String, specificHeatCapacityp: String, electronConfigurationp: String, displayRowp: Int, displayColumnp: Int ){
val atomicNumber             = atomicNumberp
val elementName              = elementNamep
val symbol                   = symbolp.trim
val atomicWeight             = atomicWeightp
val period                   = periodp
val group                    = groupp
val phase                    = phasep
val mostStableCrystal        = mostStableCrystalp
val typeOfElement            = typeOfElementp
val ionicRadius              = ionicRadiusp
val atomicRadius             = atomicRadiusp
val electronegativity        = electronegativityp
val firstIonizationPotential = firstIonizationPotentialp
val density                  = densityp
val meltingPoint             = meltingPointp
val boilingPoint             = boilingPointp
val isotopes                 = isotopesp
val yearOfDiscovery          = yearOfDiscoveryp
val specificHeatCapacity     = specificHeatCapacityp
val electronConfiguration    = electronConfigurationp
val displayRow               = displayRowp
val displayColumn            = displayColumnp


 def this(atomicNumber: String, elementName: String, symbol: String, atomicWeight: String, period: String, group: String, phase: String, mostStableCrystal: String, typeOfElement: String, ionicRadius: String, atomicRadius: String, electronegativity: String, firstIonizationPotential: String, density: String, meltingPoint: String, boilingPoint:String, isotopes: String, yearOfDiscovery: String, specificHeatCapacity: String, electronConfiguration: String, displayRow: String, displayColumn: String ){
        this(Utilities.parseInt(atomicNumber),elementName,symbol, Utilities.parseDouble(atomicWeight), Utilities.parseInt(period), Utilities.parseInt(group), phase, mostStableCrystal, typeOfElement, Utilities.parseDouble(ionicRadius), Utilities.parseDouble(atomicRadius), Utilities.parseDouble(electronegativity), Utilities.parseDouble(firstIonizationPotential), Utilities.parseDouble(density), Utilities.parseDouble(meltingPoint), Utilities.parseDouble(boilingPoint), Utilities.parseInt(isotopes), yearOfDiscovery, specificHeatCapacity, electronConfiguration, Utilities.parseInt(displayRow), Utilities.parseInt(displayColumn))

 }
 def printElement() : String = {
  "new Element(" + atomicNumber + ",\"" + elementName + "\",\"" + symbol + "\"," + atomicWeight + "," + period + "," + group + ",\"" + phase + "\",\"" + mostStableCrystal + "\",\"" + typeOfElement + "\"," + ionicRadius + "," + atomicRadius + "," + electronegativity + "," + firstIonizationPotential + "," + density + "," + meltingPoint + "," + boilingPoint + "," + isotopes + ",\"" + yearOfDiscovery + "\",\"" + specificHeatCapacity + "\",\"" + electronConfiguration + "\"," + displayRow + "," + displayColumn + ")"

 }
 }
 //This class will let you fetch by name, symbol, or atomic number
 //You may also request the entire array which is ordered. Therefore you can fetch from the array by Atomic Number - 1 for the array index.
 object PeriodicTable{
 	//Elements array
	//All 118 are accounted for so you can just subtract 1 from the Atomic Number to get the element from this.
 	val elementsA =  Array(new Element(1,"Hydrogen","H",1.00794,1,1,"gas","","Nonmetal",0.012,0.79,2.2,13.5984,8.988E-5,14.175,20.28,3,"1766","14.304","1s1",1,1),new Element(2,"Helium","He",4.002602,1,18,"gas","","Noble Gas",0.0,0.49,0.0,24.5874,1.785E-4,0.0,4.22,5,"1868","5.193","1s2",1,18),new Element(3,"Lithium","Li",6.941,2,1,"solid","bcc","Alkali Metal",0.76,2.1,0.98,5.3917,0.534,453.85,1615.0,5,"1817","3.582","[He] 2s1",2,1),new Element(4,"Beryllium","Be",9.012182,2,2,"solid","hex","Alkaline Earth Metal",0.35,1.4,1.57,9.3227,1.85,1560.15,2742.0,6,"1798","1.825","[He] 2s2",2,2),new Element(5,"Boron","B",10.811,2,13,"solid","rho","Metalloid",0.23,1.2,2.04,8.298,2.34,2573.15,4200.0,6,"1808","1.026","[He] 2s2 2p1",2,13),new Element(6,"Carbon","C",12.0107,2,14,"solid","hex","Nonmetal",0.0,0.91,2.55,11.2603,2.267,3948.15,4300.0,7,"","0.709","[He] 2s2 2p2",2,14),new Element(7,"Nitrogen","N",14.0067,2,15,"gas","","Nonmetal",0.13,0.75,3.04,14.5341,0.0012506,63.29,77.36,8,"1772","1.04","[He] 2s2 2p3",2,15),new Element(8,"Oxygen","O",15.9994,2,16,"gas","","Nonmetal",1.4,0.65,3.44,13.6181,0.001429,50.5,90.2,8,"1774","0.918","[He] 2s2 2p4",2,16),new Element(9,"Fluorine","F",18.9984032,2,17,"gas","","Halogen",1.3,0.57,3.98,17.4228,0.001696,53.63,85.03,6,"1886","0.824","[He] 2s2 2p5",2,17),new Element(10,"Neon","Ne",20.1797,2,18,"gas","","Noble Gas",0.0,0.51,0.0,21.5645,8.999E-4,24.703,27.07,8,"1898","1.03","[He] 2s2 2p6",2,18),new Element(11,"Sodium","Na",22.98976928,3,1,"solid","bcc","Alkali Metal",1.0,2.2,0.93,5.1391,0.971,371.15,1156.0,7,"1807","1.228","[Ne] 3s1",3,1),new Element(12,"Magnesium","Mg",24.305,3,2,"solid","hex","Alkaline Earth Metal",0.72,1.7,1.31,7.6462,1.738,923.15,1363.0,8,"1755","1.023","[Ne] 3s2",3,2),new Element(13,"Aluminum","Al",26.9815386,3,13,"solid","fcc","Metal",0.54,1.8,1.61,5.9858,2.698,933.4,2792.0,8,"1827","0.897","[Ne] 3s2 3p1",3,13),new Element(14,"Silicon","Si",28.0855,3,14,"solid","fcc","Metalloid",0.4,1.5,1.9,8.1517,2.3296,1683.15,3538.0,8,"1824","0.705","[Ne] 3s2 3p2",3,14),new Element(15,"Phosphorus","P",30.973762,3,15,"solid","cub","Nonmetal",0.38,1.2,2.19,10.4867,1.82,317.25,553.0,7,"1669","0.769","[Ne] 3s2 3p3",3,15),new Element(16,"Sulfur","S",32.065,3,16,"solid","orth","Nonmetal",0.37,1.1,2.58,10.36,2.067,388.51,717.8,10,"","0.71","[Ne] 3s2 3p4",3,16),new Element(17,"Chlorine","Cl",35.453,3,17,"gas","","Halogen",1.8,0.97,3.16,12.9676,0.003214,172.31,239.11,11,"1774","0.479","[Ne] 3s2 3p5",3,17),new Element(18,"Argon","Ar",39.948,3,18,"gas","","Noble Gas",0.0,0.88,0.0,15.7596,0.0017837,83.96,87.3,8,"1894","0.52","[Ne] 3s2 3p6",3,18),new Element(19,"Potassium","K",39.0983,4,1,"solid","bcc","Alkali Metal",1.4,2.8,0.82,4.3407,0.862,336.5,1032.0,10,"1807","0.757","[Ar] 4s1",4,1),new Element(20,"Calcium","Ca",40.078,4,2,"solid","fcc","Alkaline Earth Metal",0.99,2.2,1.0,6.1132,1.54,1112.15,1757.0,14,"1808","0.647","[Ar] 4s2",4,2),new Element(21,"Scandium","Sc",44.955912,4,3,"solid","hex","Transition Metal",0.75,2.1,1.36,6.5615,2.989,1812.15,3109.0,15,"1878","0.568","[Ar] 3d1 4s2",4,3),new Element(22,"Titanium","Ti",47.867,4,4,"solid","hex","Transition Metal",0.61,2.0,1.54,6.8281,4.54,1933.15,3560.0,9,"1791","0.523","[Ar] 3d2 4s2",4,4),new Element(23,"Vanadium","V",50.9415,4,5,"solid","bcc","Transition Metal",0.59,1.9,1.63,6.7462,6.11,2175.15,3680.0,9,"1801","0.489","[Ar] 3d3 4s2",4,5),new Element(24,"Chromium","Cr",51.9961,4,6,"solid","bcc","Transition Metal",0.52,1.9,1.66,6.7665,7.15,2130.15,2944.0,9,"1797","0.449","[Ar] 3d5 4s1",4,6),new Element(25,"Manganese","Mn",54.938045,4,7,"solid","bcc","Transition Metal",0.46,1.8,1.55,7.434,7.44,1519.15,2334.0,11,"1774","0.479","[Ar] 3d5 4s2",4,7),new Element(26,"Iron","Fe",55.845,4,8,"solid","bcc","Transition Metal",0.65,1.7,1.83,7.9024,7.874,1808.15,3134.0,10,"","0.449","[Ar] 3d6 4s2",4,8),new Element(27,"Cobalt","Co",58.933195,4,9,"solid","hex","Transition Metal",0.75,1.7,1.88,7.881,8.86,1768.15,3200.0,14,"1735","0.421","[Ar] 3d7 4s2",4,9),new Element(28,"Nickel","Ni",58.6934,4,10,"solid","fcc","Transition Metal",0.69,1.6,1.91,7.6398,8.912,1726.15,3186.0,11,"1751","0.444","[Ar] 3d8 4s2",4,10),new Element(29,"Copper","Cu",63.546,4,11,"solid","fcc","Transition Metal",0.73,1.6,1.9,7.7264,8.96,1357.75,2835.0,11,"","0.385","[Ar] 3d10 4s1",4,11),new Element(30,"Zinc","Zn",65.38,4,12,"solid","hex","Transition Metal",0.74,1.5,1.65,9.3942,7.134,692.88,1180.0,15,"","0.388","[Ar] 3d10 4s2",4,12),new Element(31,"Gallium","Ga",69.723,4,13,"solid","orth","Metal",0.62,1.8,1.81,5.9993,5.907,302.91,2477.0,14,"1875","0.371","[Ar] 3d10 4s2 4p1",4,13),new Element(32,"Germanium","Ge",72.64,4,14,"solid","fcc","Metalloid",0.53,1.5,2.01,7.8994,5.323,1211.45,3106.0,17,"1886","0.32","[Ar] 3d10 4s2 4p2",4,14),new Element(33,"Arsenic","As",74.9216,4,15,"solid","rho","Metalloid",0.58,1.3,2.18,9.7886,5.776,1090.15,887.0,14,"1250","0.329","[Ar] 3d10 4s2 4p3",4,15),new Element(34,"Selenium","Se",78.96,4,16,"solid","hex","Nonmetal",0.5,1.2,2.55,9.7524,4.809,494.15,958.0,20,"1817","0.321","[Ar] 3d10 4s2 4p4",4,16),new Element(35,"Bromine","Br",79.904,4,17,"liq","","Halogen",2.0,1.1,2.96,11.8138,3.122,266.05,332.0,19,"1826","0.474","[Ar] 3d10 4s2 4p5",4,17),new Element(36,"Krypton","Kr",83.798,4,18,"gas","","Noble Gas",0.0,1.0,0.0,13.9996,0.003733,115.93,119.93,23,"1898","0.248","[Ar] 3d10 4s2 4p6",4,18),new Element(37,"Rubidium","Rb",85.4678,5,1,"solid","bcc","Alkali Metal",1.5,3.0,0.82,4.1771,1.532,312.79,961.0,20,"1861","0.363","[Kr] 5s1",5,1),new Element(38,"Strontium","Sr",87.62,5,2,"solid","fcc","Alkaline Earth Metal",1.1,2.5,0.95,5.6949,2.64,1042.15,1655.0,18,"1808","0.301","[Kr] 5s2",5,2),new Element(39,"Yttrium","Y",88.90585,5,3,"solid","hex","Transition Metal",0.9,2.3,1.22,6.2173,4.469,1799.15,3609.0,21,"1794","0.298","[Kr] 4d1 5s2",5,3),new Element(40,"Zirconium","Zr",91.224,5,4,"solid","hex","Transition Metal",0.72,2.2,1.33,6.6339,6.506,2125.15,4682.0,20,"1789","0.278","[Kr] 4d2 5s2",5,4),new Element(41,"Niobium","Nb",92.90638,5,5,"solid","bcc","Transition Metal",0.69,2.1,1.6,6.7589,8.57,2741.15,5017.0,24,"1801","0.265","[Kr] 4d4 5s1",5,5),new Element(42,"Molybdenum","Mo",95.96,5,6,"solid","bcc","Transition Metal",0.65,2.0,2.16,7.0924,10.22,2890.15,4912.0,20,"1778","0.251","[Kr] 4d5 5s1",5,6),new Element(43,"Technetium","Tc",98.0,5,7,"artificial","hex","Transition Metal",0.56,2.0,1.9,7.28,11.5,2473.15,5150.0,23,"1937","","[Kr] 4d5 5s2",5,7),new Element(44,"Ruthenium","Ru",101.07,5,8,"solid","hex","Transition Metal",0.68,1.9,2.2,7.3605,12.37,2523.15,4423.0,16,"1844","0.238","[Kr] 4d7 5s1",5,8),new Element(45,"Rhodium","Rh",102.9055,5,9,"solid","fcc","Transition Metal",0.68,1.8,2.28,7.4589,12.41,2239.15,3968.0,20,"1803","0.243","[Kr] 4d8 5s1",5,9),new Element(46,"Palladium","Pd",106.42,5,10,"solid","fcc","Transition Metal",0.86,1.8,2.2,8.3369,12.02,1825.15,3236.0,21,"1803","0.244","[Kr] 4d10",5,10),new Element(47,"Silver","Ag",107.8682,5,11,"solid","fcc","Transition Metal",1.3,1.8,1.93,7.5762,10.501,1234.15,2435.0,27,"","0.235","[Kr] 4d10 5s1",5,11),new Element(48,"Cadmium","Cd",112.411,5,12,"solid","hex","Transition Metal",0.97,1.7,1.69,8.9938,8.69,594.33,1040.0,22,"1817","0.232","[Kr] 4d10 5s2",5,12),new Element(49,"Indium","In",114.818,5,13,"solid","tet","Metal",0.8,2.0,1.78,5.7864,7.31,429.91,2345.0,34,"1863","0.233","[Kr] 4d10 5s2 5p1",5,13),new Element(50,"Tin","Sn",118.71,5,14,"solid","tet","Metal",0.69,1.7,1.96,7.3439,7.287,505.21,2875.0,28,"","0.228","[Kr] 4d10 5s2 5p2",5,14),new Element(51,"Antimony","Sb",121.76,5,15,"solid","rho","Metalloid",0.76,1.5,2.05,8.6084,6.685,904.05,1860.0,29,"","0.207","[Kr] 4d10 5s2 5p3",5,15),new Element(52,"Tellurium","Te",127.6,5,16,"solid","hex","Metalloid",0.97,1.4,2.1,9.0096,6.232,722.8,1261.0,29,"1782","0.202","[Kr] 4d10 5s2 5p4",5,16),new Element(53,"Iodine","I",126.90447,5,17,"solid","orth","Halogen",2.2,1.3,2.66,10.4513,4.93,386.65,457.4,24,"1811","0.214","[Kr] 4d10 5s2 5p5",5,17),new Element(54,"Xenon","Xe",131.293,5,18,"gas","gas","Noble Gas",0.0,1.2,0.0,12.1298,0.005887,161.45,165.03,31,"1898","0.158","[Kr] 4d10 5s2 5p6",5,18),new Element(55,"Cesium","Cs",132.9054519,6,1,"solid","","Alkali Metal",1.7,3.3,0.79,3.8939,1.873,301.7,944.0,22,"1860","0.242","[Xe] 6s1",6,1),new Element(56,"Barium","Ba",137.327,6,2,"solid","bcc","Alkaline Earth Metal",1.4,2.8,0.89,5.2117,3.594,1002.15,2170.0,25,"1808","0.204","[Xe] 6s2",6,2),new Element(57,"Lanthanum","La",138.90547,6,3,"solid","hex","Lanthanide",1.1,2.7,1.1,5.5769,6.145,1193.15,3737.0,19,"1839","0.195","[Xe] 5d1 6s2",8,3),new Element(58,"Cerium","Ce",140.116,6,19,"solid","fcc","Lanthanide",1.0,2.7,1.12,5.5387,6.77,1071.15,3716.0,19,"1803","0.192","[Xe] 4f1 5d1 6s2",8,4),new Element(59,"Praseodymium","Pr",140.90765,6,20,"solid","hex","Lanthanide",1.0,2.7,1.13,5.473,6.773,1204.15,3793.0,15,"1885","0.193","[Xe] 4f3 6s2",8,5),new Element(60,"Neodymium","Nd",144.242,6,21,"solid","hex","Lanthanide",1.0,2.6,1.14,5.525,7.007,1289.15,3347.0,16,"1885","0.19","[Xe] 4f4 6s2",8,6),new Element(61,"Promethium","Pm",145.0,6,22,"artificial","hex","Lanthanide",0.98,2.6,1.13,5.582,7.26,1204.15,3273.0,14,"1945","","[Xe] 4f5 6s2",8,7),new Element(62,"Samarium","Sm",150.36,6,23,"solid","hex","Lanthanide",0.96,2.6,1.17,5.6437,7.52,1345.15,2067.0,17,"1879","0.197","[Xe] 4f6 6s2",8,8),new Element(63,"Europium","Eu",151.964,6,24,"solid","bcc","Lanthanide",0.95,2.6,1.2,5.6704,5.243,1095.15,1802.0,21,"1901","0.182","[Xe] 4f7 6s2",8,9),new Element(64,"Gadolinium","Gd",157.25,6,25,"solid","hex","Lanthanide",0.94,2.5,1.2,6.1501,7.895,1585.15,3546.0,17,"1880","0.236","[Xe] 4f7 5d1 6s2",8,10),new Element(65,"Terbium","Tb",158.92535,6,26,"solid","hex","Lanthanide",0.92,2.5,1.2,5.8638,8.229,1630.15,3503.0,24,"1843","0.182","[Xe] 4f9 6s2",8,11),new Element(66,"Dysprosium","Dy",162.5,6,27,"solid","hex","Lanthanide",0.91,2.5,1.22,5.9389,8.55,1680.15,2840.0,21,"1886","0.17","[Xe] 4f10 6s2",8,12),new Element(67,"Holmium","Ho",164.93032,6,28,"solid","hex","Lanthanide",0.9,2.5,1.23,6.0215,8.795,1743.15,2993.0,29,"1878","0.165","[Xe] 4f11 6s2",8,13),new Element(68,"Erbium","Er",167.259,6,29,"solid","hex","Lanthanide",0.88,2.5,1.24,6.1077,9.066,1795.15,3503.0,16,"1843","0.168","[Xe] 4f12 6s2",8,14),new Element(69,"Thulium","Tm",168.93421,6,30,"solid","hex","Lanthanide",0.87,2.4,1.25,6.1843,9.321,1818.15,2223.0,18,"1879","0.16","[Xe] 4f13 6s2",8,15),new Element(70,"Ytterbium","Yb",173.054,6,31,"solid","fcc","Lanthanide",0.86,2.4,1.1,6.2542,6.965,1097.15,1469.0,16,"1878","0.155","[Xe] 4f14 6s2",8,16),new Element(71,"Lutetium","Lu",174.9668,6,32,"solid","hex","Lanthanide",0.85,2.3,1.27,5.4259,9.84,1936.15,3675.0,22,"1907","0.154","[Xe] 4f14 5d1 6s2",8,17),new Element(72,"Hafnium","Hf",178.49,6,4,"solid","hex","Transition Metal",0.71,2.2,1.3,6.8251,13.31,2500.15,4876.0,17,"1923","0.144","[Xe] 4f14 5d2 6s2",6,4),new Element(73,"Tantalum","Ta",180.94788,6,5,"solid","bcc","Transition Metal",0.64,2.1,1.5,7.5496,16.654,3269.15,5731.0,19,"1801","0.14","[Xe] 4f14 5d3 6s2",6,5),new Element(74,"Wolfram","W",183.84,6,6,"solid","bcc","Transition Metal",0.62,2.0,2.36,7.864,19.25,3680.15,5828.0,22,"1783","0.132","[Xe] 4f14 5d4 6s2",6,6),new Element(75,"Rhenium","Re",186.207,6,7,"solid","hex","Transition Metal",0.56,2.0,1.9,7.8335,21.02,3453.15,5869.0,21,"1925","0.137","[Xe] 4f14 5d5 6s2",6,7),new Element(76,"Osmium","Os",190.23,6,8,"solid","hex","Transition Metal",0.63,1.9,2.2,8.4382,22.61,3300.15,5285.0,19,"1803","0.13","[Xe] 4f14 5d6 6s2",6,8),new Element(77,"Iridium","Ir",192.217,6,9,"solid","fcc","Transition Metal",0.63,1.9,2.2,8.967,22.56,2716.15,4701.0,25,"1804","0.131","[Xe] 4f14 5d7 6s2",6,9),new Element(78,"Platinum","Pt",195.084,6,10,"solid","fcc","Transition Metal",0.63,1.8,2.28,8.9587,21.46,2045.15,4098.0,32,"1735","0.133","[Xe] 4f14 5d9 6s1",6,10),new Element(79,"Gold","Au",196.966569,6,11,"solid","fcc","Transition Metal",0.85,1.8,2.54,9.2255,19.282,1337.73,3129.0,21,"","0.129","[Xe] 4f14 5d10 6s1",6,11),new Element(80,"Mercury","Hg",200.59,6,12,"liq","","Transition Metal",1.0,1.8,2.0,10.4375,13.5336,234.43,630.0,26,"","0.14","[Xe] 4f14 5d10 6s2",6,12),new Element(81,"Thallium","Tl",204.3833,6,13,"solid","hex","Metal",1.5,2.1,2.04,6.1082,11.85,577.15,1746.0,28,"1861","0.129","[Xe] 4f14 5d10 6s2 6p1",6,13),new Element(82,"Lead","Pb",207.2,6,14,"solid","fcc","Metal",1.2,1.8,2.33,7.4167,11.342,600.75,2022.0,29,"","0.129","[Xe] 4f14 5d10 6s2 6p2",6,14),new Element(83,"Bismuth","Bi",208.9804,6,15,"solid","rho","Metal",1.0,1.6,2.02,7.2856,9.807,544.67,1837.0,19,"1753","0.122","[Xe] 4f14 5d10 6s2 6p3",6,15),new Element(84,"Polonium","Po",210.0,6,16,"solid","cub","Metalloid",2.3,1.5,2.0,8.417,9.32,527.15,1235.0,34,"1898","","[Xe] 4f14 5d10 6s2 6p4",6,16),new Element(85,"Astatine","At",210.0,6,17,"solid","","Noble Gas",0.0,1.4,2.2,9.3,7.0,575.15,610.0,21,"1940","","[Xe] 4f14 5d10 6s2 6p5",6,17),new Element(86,"Radon","Rn",222.0,6,18,"gas","","Alkali Metal",0.0,1.3,0.0,10.7485,0.00973,202.15,211.3,20,"1900","0.094","[Xe] 4f14 5d10 6s2 6p6",6,18),new Element(87,"Francium","Fr",223.0,7,1,"solid","bcc","Alkaline Earth Metal",1.8,0.0,0.7,4.0727,1.87,300.15,950.0,21,"1939","","[Rn] 7s1",7,1),new Element(88,"Radium","Ra",226.0,7,2,"solid","bcc","Actinide",1.4,0.0,0.9,5.2784,5.5,973.15,2010.0,15,"1898","","[Rn] 7s2",7,2),new Element(89,"Actinium","Ac",227.0,7,3,"solid","fcc","Actinide",1.1,0.0,1.1,5.17,10.07,1323.15,3471.0,11,"1899","0.12","[Rn] 6d1 7s2",9,3),new Element(90,"Thorium","Th",232.03806,7,19,"solid","fcc","Actinide",0.97,0.0,1.3,6.3067,11.72,2028.15,5061.0,12,"1828","0.113","[Rn] 6d2 7s2",9,4),new Element(91,"Protactinium","Pa",231.03588,7,20,"solid","orth","Actinide",0.78,0.0,1.5,5.89,15.37,1873.15,4300.0,14,"1917","","[Rn] 5f2 6d1 7s2",9,5),new Element(92,"Uranium","U",238.02891,7,21,"solid","orth","Actinide",0.52,0.0,1.38,6.1941,18.95,1405.15,4404.0,15,"1841","0.116","[Rn] 5f3 6d1 7s2",9,6),new Element(93,"Neptunium","Np",237.0,7,22,"artificial","orth","Actinide",0.75,0.0,1.36,6.2657,20.45,913.15,4273.0,153,"1940","","[Rn] 5f4 6d1 7s2",9,7),new Element(94,"Plutonium","Pu",244.0,7,23,"artificial","mno","Actinide",0.89,0.0,1.28,6.0262,19.84,913.15,3501.0,163,"1940","","[Rn] 5f6 7s2",9,8),new Element(95,"Americium","Am",243.0,7,24,"artificial","hex","Actinide",0.98,0.0,1.3,5.9738,13.69,1267.15,2880.0,133,"1944","","[Rn] 5f7 7s2",9,9),new Element(96,"Curium","Cm",247.0,7,25,"artificial","hex","Actinide",0.97,0.0,1.3,5.9915,13.51,1340.15,3383.0,133,"1944","","",9,10),new Element(97,"Berkelium","Bk",247.0,7,26,"artificial","hex","Actinide",0.95,0.0,1.3,6.1979,14.79,1259.15,983.0,83,"1949","","",9,11),new Element(98,"Californium","Cf",251.0,7,27,"artificial","hex","Actinide",0.93,0.0,1.3,6.2817,15.1,1925.15,1173.0,123,"1950","","",9,12),new Element(99,"Einsteinium","Es",252.0,7,28,"artificial","hex","Actinide",0.0,0.0,1.3,6.42,13.5,1133.15,0.0,123,"1952","","",9,13),new Element(100,"Fermium","Fm",257.0,7,29,"artificial","","Actinide",0.0,0.0,1.3,6.5,0.0,0.0,0.0,103,"1953","","",9,14),new Element(101,"Mendelevium","Md",258.0,7,30,"artificial","","Actinide",0.0,0.0,1.3,6.58,0.0,0.0,0.0,33,"1955","","",9,15),new Element(102,"Nobelium","No",259.0,7,31,"artificial","","Actinide",0.0,0.0,1.3,6.65,0.0,0.0,0.0,73,"1958","","",9,16),new Element(103,"Lawrencium","Lr",262.0,7,32,"artificial","","Actinide",0.0,0.0,0.0,0.0,0.0,0.0,0.0,203,"1961","","",9,17),new Element(104,"Rutherfordium","Rf",261.0,7,4,"artificial","","Transactinide",0.0,0.0,0.0,0.0,18.1,0.0,0.0,0,"1969","","",7,4),new Element(105,"Dubnium","Db",262.0,7,5,"artificial","","Transactinide",0.0,0.0,0.0,0.0,39.0,0.0,0.0,0,"1970","","",7,5),new Element(106,"Seaborgium","Sg",266.0,7,6,"artificial","","Transactinide",0.0,0.0,0.0,0.0,35.0,0.0,0.0,0,"1974","","",7,6),new Element(107,"Bohrium","Bh",264.0,7,7,"artificial","","Transactinide",0.0,0.0,0.0,0.0,37.0,0.0,0.0,0,"1981","","",7,7),new Element(108,"Hassium","Hs",267.0,7,8,"artificial","","Transactinide",0.0,0.0,0.0,0.0,41.0,0.0,0.0,0,"1983","","",7,8),new Element(109,"Meitnerium","Mt",268.0,7,9,"artificial","","Transactinide",0.0,0.0,0.0,0.0,35.0,0.0,0.0,0,"1982","","",7,9),new Element(110,"Darmstadtium ","Ds",271.0,7,10,"artificial","","Transactinide",0.0,0.0,0.0,0.0,0.0,0.0,0.0,0,"","","",7,10),new Element(111,"Roentgenium ","Rg",272.0,7,11,"artificial","","Transactinide",0.0,0.0,0.0,0.0,0.0,0.0,0.0,0,"","","",7,11),new Element(112,"Copernicium ","Cn",285.0,7,12,"artificial","","Transactinide",0.0,0.0,0.0,0.0,0.0,0.0,0.0,0,"","","",7,12),new Element(113,"Ununtrium ","Uut",284.0,7,13,"artificial","","",0.0,0.0,0.0,0.0,0.0,0.0,0.0,0,"","","",7,13),new Element(114,"Ununquadium ","Uuq",289.0,7,14,"artificial","","Transactinide",0.0,0.0,0.0,0.0,0.0,0.0,0.0,0,"","","",7,14),new Element(115,"Ununpentium ","Uup",288.0,7,15,"artificial","","",0.0,0.0,0.0,0.0,0.0,0.0,0.0,0,"","","",7,15),new Element(116,"Ununhexium ","Uuh",292.0,7,16,"artificial","","Transactinide",0.0,0.0,0.0,0.0,0.0,0.0,0.0,0,"","","",7,16),new Element(117,"Ununseptium ","Uus",295.0,7,17,"artificial","","",0.0,0.0,0.0,0.0,0.0,0.0,0.0,0,"","","",7,17),new Element(118,"Ununoctium ","Uuo",294.0,7,18,"artificial","","Noble Gas",0.0,0.0,0.0,0.0,0.0,0.0,0.0,0,"","","",7,18))
	// elements Symbol
	var elementsS:Map[String,Int] = Map()
	// elements Name
	var elementsN:Map[String,Int] =  Map()
	var tmp = elementsA(0)
	for(x <- 0 to 117){
	  tmp = elementsA(x)
	  elementsS += (tmp.symbol -> x)
	  elementsN += (tmp.elementName -> x)
	}
	def getByAtomicNumber(an:Int):Option[Element] = {
	 try{
	  Some(elementsA(an-1))
	 }catch { case _:Throwable => None}
	}
	def getByElementName(en:String):Option[Element] = {
	 try{
	  Some(elementsA(elementsN(en)))
	 }catch{ case _:Throwable => None}
	}
	def getByElementSymbol(es:String):Option[Element] = {
	try{
	 Some(elementsA(elementsS(es)))
	 }catch{ case _:Throwable => None}
	}
	def getTable():Array[Element] = {
	 elementsA
	}
 }
}
