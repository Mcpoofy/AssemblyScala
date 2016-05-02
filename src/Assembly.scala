
import scala.collection.mutable.{ HashMap, Stack }


class Assembly {
  abstract sealed class ARMLine
  //case classes for different possible operations
  case class Math4(dr:String,sr1:String,sr2:String,op:String) extends ARMLine
  case class Math3(dr: String, sr1: String, op: String) extends ARMLine
  case class Bitwise(dr:String,sr1:String,st2:String,op:String) extends ARMLine
  case class Move(dr:String,sr:String,op:String) extends ARMLine
  case class Compare(dr:String, sr:String,op:String) extends ARMLine
  case class StackPush(dr:String, sr:String) extends ARMLine
  case class StackPop(dr:String, sr:String,str1:String) extends ARMLine
  case class RegisterOp(dr:String, sr:String,str1:String) extends ARMLine
  case class Constant(dr:String, sr:Int) extends ARMLine
  case class Branching(op:String, dest:String,link:Int, returnadd:String) extends ARMLine
  case class Labels(str:String, line:Int) extends ARMLine
  case class LabelAddress(str:String, reta:Int) extends ARMLine
  case class Halt() extends ARMLine

  // keep track of which line we are on
  var current: Int = 0
  //getting each line
  var lines = new HashMap[Int, ARMLine]
  //defining a constant
  var constants = new HashMap[String, Int]
  //getting the address of a variable
  var adressesVar = new HashMap[String, Int]
  //storing the refrence of an address
  var adresses = new HashMap[Int, Int]
  //storing the stack
  var stack = new Array[Int](4096)
  //storing the stack counter
  var stackcounter:Int = 4095
  //storing the registers
  var regs = new Array[Int](16)
  // storing the labels starting position
  var labelsStart = new HashMap[String, Int]
  // storing the label return address
  var labelsEnd = new HashMap[String, Int]

  //program counter
  var PC:Int = regs(15)

  // flags
  var N:Int = 0
  var Z:Int = 1
  var C:Int = 0
  var V:Int = 0

  //store the register values as "constants" so the compiler recongizes them
  val r0:String = "r0"
  val r1:String = "r1"
  val r2:String = "r2"
  val r3:String = "r3"
  val r4:String = "r4"
  val r5:String = "r5"
  val r6:String = "r6"
  val r7:String = "r7"
  val r8:String = "r8"
  val r9:String = "r9"
  val r10:String = "r10"
  val r11:String = "r11"
  val r12:String = "r12"
  val r13:String = "r13"
  val r14:String = "r14"
  val r15:String = "r15"
  val R0:String = "r0"
  val R1:String = "r1"
  val R2:String = "r2"
  val R3:String = "r3"
  val R4:String = "r4"
  val R5:String = "r5"
  val R6:String = "r6"
  val R7:String = "r7"
  val R8:String = "r8"
  val R9:String = "r9"
  val R10:String = "r10"
  val R11:String = "r11"
  val R12:String = "r12"
  val R13:String = "r13"
  val R14:String = "r14"
  val R15:String = "r15"

  //make a hashmap to store the register values *also use regs
  val registermap = new HashMap[String,Int]
  makeHashMap()


  //condition code setting based on condition codes
  def AL():Int =
  {
    return 1;
  }
  def NV():Int =
  {
    return 0;
  }
  def EQ():Int =
  {
    if (Z==1) return 1 else return 0
  }
  def NE():Int =
  {
    if (Z==0) return 1 else return 0
  }
  def CS():Int =
  {
    if (C==1) return 1 else return 0
  }
  def CC():Int =
  {
    if (C==0) return 1 else return 0
  }
  def MI():Int =
  {
    if (N==1) return 1 else return 0
  }
  def PL():Int =
  {
    if (N==0) return 1 else return 0
  }
  def VS():Int =
  {
    if (V==1) return 1 else return 0
  }
  def VC():Int =
  {
    if (V==0) return 1 else return 0
  }
  def HI():Int =
  {
    if (C==0 && Z==0) return 1 else return 0
  }
  def LS():Int =
  {
    if (C==0 || Z==1) return 1 else return 0
  }
  def GE():Int =
  {
    if (N==V) return 1 else return 0
  }
  def LT():Int =
  {
    if (N!=V) return 1 else return 0
  }
  def LE():Int =
  {
    if (N==1 || N!=V) return 1 else return 0
  }
  def GT():Int =
  {
    if (Z==0 && N==V) return 1 else return 0
  }
  // parse the input if you do not know if immeidate, register, constant, variable passed in
  private def parseInput(str:String):Int =
  {
    if(str.matches("[0-9]+"))
    {
      return str.toInt
    }
    else if(str.startsWith("#"))
    {
      if(str.substring(1).matches("[0-9]+")) return (str.substring(1)).toInt
      else return  constants(str.substring(1))
    }
    else
    {
      return regs((registermap.getOrElse(str,-1)))
    }
  }
  // set the flags if the operation code used requires to set flags
  // Value 1 is the first value, value2 is the second value given, value3 is the result of the values on some operation
  // N{X} where X is in {N,Z,V,C} is 1 if the flag is modified and 0 if its not
  private def setFlags(value1:Int, value2:Int, value3:Int, nZ:Int, nN:Int, nV:Int, nC:Int)
  {

    if(nN==1) N = if(value3>0) 1 else 0;
    if(nZ==1) Z = if(value3==0) 1 else 0;
    if(nV==1) V = if(value1>>31==value2>>31 && value2>>31!=value3>>31)1 else 0;
    if(nC==1) C =if(value1>>31==value2>>31) 1 else 0;
  }

  //prints the registers from 0 to 15
  private def printRegs(){
    val c = 0
    var i = 0
    regs.foreach {(r: Int) =>
      val bin = r.toBinaryString
      val charsRemaining = 32 - bin.length
      val prefix = if (i < 0) "1" else "0" * charsRemaining
      val label = "R" + i + ": " + (if (i >= 10) "" else " ")
      println(label + prefix + bin)
      i += 1
    }
    println("---------------------------")
  }
  //prints the different possible flags
  private def printFlags(){
    println( "C = " +C +" N = " +N +" Z = " +Z +" V = " +V)
  }
  // puts R{X} and r{X} where x is in [0,15] into a hashmap with the value x as a result
  // useful for converting r{X} to x
  private def makeHashMap()
  {
    (0 to 15).foreach {(c: Int) =>
      registermap.put("r" + c, c)
      registermap.put("r" + c, c)
    }

  }

  // Used to go through and "run" the program after you have parsed each line

  private def gotoLine(line: Int) {


    lines(line) match {

      case Move(dr:String,sr1:String, op:String) =>
      {
        val dest = registermap.getOrElse(dr,-1)
        var r1 = parseInput(sr1)
        op match
        {
          case "movs" =>
            setFlags(regs(dest), 0, regs(dest), 1,1,0,0)
          case "movn" =>
            r1 = ~r1
          case "movns" =>
            r1 = ~r1
            setFlags(regs(dest), 0, regs(dest), 1,1,0,0)
          case _ =>
        }
        regs(dest) = r1;
        printRegs()
        gotoLine(line+1)
      }

      //performing math operations on the registers
      case Math4(dr:String,sr1:String,sr2:String,op:String) => {
        val dest = registermap.getOrElse(dr,-1)
        val r1 = regs((registermap.getOrElse(sr1,-1)))
        var r2 = parseInput(sr2)

        op match
        {
          case "+" => regs(dest) =r1+r2
          case "-" => regs(dest) =r1-r2
          case "/" =>regs(dest) =r1/r2
          case "*" =>regs(dest) =r1*r2;setFlags(r1, r2, regs(dest), 1,1,0,0)
          case "ASR" => regs(dest) =r1 >>> (r2 & 0x000000FF)
          case "ASRS" => regs(dest) =r1 >>> (r2 & 0x000000FF); setFlags(r1, r2, regs(dest), 1,1,0,0)
          case "LSL" =>regs(dest) =r1 << (r2 & 0x000000FF)
          case "LSLS" =>regs(dest) =r1 << (r2 & 0x000000FF); setFlags(r1, r2, regs(dest), 1,1,0,0)
          case "LSR" =>regs(dest) =r1 >> (r2 & 0x000000FF)
          case "LSRS" =>regs(dest) =r1 >> (r2 & 0x000000FF); setFlags(r1, r2, regs(dest), 1,1,0,0)
          case "ROR" =>regs(dest) = Integer.rotateRight(r1, r2 & 0x000000FF)
          case "RORS" =>regs(dest) = Integer.rotateRight(r1, r2 & 0x000000FF); setFlags(r1, r2, regs(dest), 1,1,0,0)
          case _ => println(s"ERROR: no match for Math4($dr, $sr1, $sr2, $op)")
        }

        printRegs()
        gotoLine(line+1)

      }

      case Math3(dr: String, sr1: String, op: String) => {
        val dest = registermap.getOrElse(dr, -1)
        val r1 = registermap.getOrElse(sr1, -1)

        op match {
          case "NOT" => regs(dest) = ~regs(r1)
          case _ => println(s"ERROR: no match for Math3($dr, $sr1, $op)")
        }

        printRegs()
        gotoLine(line + 1)
      }

      //preforming logical operations and storing the bits
      case Bitwise(dr:String,sr1:String,sr2:String,op:String) => {
        val dest = (registermap.getOrElse(dr,-1))
        val r1 = parseInput(sr1)
        val r2 = parseInput(sr2)

        op match
        {
          case "|" => regs(dest)=r2|r1;setFlags(r1, r2, regs(dest), 1,1,0,0)
          case "&" => regs(dest)=r2&r1;setFlags(r1,r2, regs(dest), 1,1,0,0)
          case "^" => regs(dest)=r2^r1;setFlags(r1, r2, regs(dest), 1,1,0,0)
          case "~" => regs(dest) = ~r1
          case "bic" => regs(dest)=r1&(r2^Int.MaxValue) ;setFlags(r1,r2^Int.MaxValue, regs(dest), 1,1,0,0)
          case _ => println(s"ERROR: no match for Bitwise($dr, $sr1, $sr2, $op)")
        }
        printRegs()
        gotoLine(line+1)

      }

      //comparing values and setting the flags
      case Compare(dr:String, sr1:String, op:String) => {


        val dest:Int = (registermap.getOrElse(dr,-1))
        var r1:Int = 0
        r1= parseInput(sr1)

        var tst= regs(dest)&r1
        var cmp = regs(dest)-r1
        var cmp1 = regs(dest)+r1
        var tst1 = regs(dest)^r1
        op match {
          case "cmp" => setFlags(regs(dest), r1, cmp, 1,1,1,1)
          case "cmp1" => setFlags(regs(dest), r1, cmp1, 1,1,1,1)
          case "tst" => setFlags(regs(dest), r1, tst, 1,1,1,1)
          case "teq" =>setFlags(regs(dest), r1, tst1, 1,1,1,1)

        }
        printFlags()
        gotoLine(line+1)
      }

      case StackPush(dr:String, str1:String) =>
      {
        //parsing the input from the push function
        val a:Array[String] = dr.split("-");
        val b:Array[String] = str1.split("-")
        var a1:Int = 0
        var a2:Int = 0
        var b1:Int = 0
        var b2:Int = 0
        if(a.length >1) {
          a1= a(0).substring(1).toInt
          a2 =a(1).substring(1).toInt
        }
        else {
          a1 =  a(0).substring(1).toInt
          a2= a1
        }
        if(b.length >1) {
          b1= b(0).substring(1).toInt
          b2 =b(1).substring(1).toInt
        }
        else {
          b1 =b(0).substring(1).toInt
          b2= b1
        }
        //because the lowest registers are set first
        while(a1 <= a2 || b1<=b2)
        {
          if(a1 < b1 && a1<=a2 || b1>b2)
          {
            stackcounter = stackcounter -1
            stack(stackcounter) = regs(registermap.getOrElse("r"+a1,-1))
            RegisterOp("r"+a1, ""+stackcounter, "str")
            a1 = a1 + 1

          }
          else
          {
            stackcounter = stackcounter -1
            stack(stackcounter) = regs(registermap.getOrElse("r"+b1,-1))
            RegisterOp("r"+b1, ""+stackcounter, "str")
            b1 = b1 + 1

          }
        }

        gotoLine(line+1)
      }
      //pop on the stack
      case StackPop(dr:String, str:String,str1:String) =>
      {
        val dest:Int = (registermap.getOrElse(dr,-1))
        val r1:Int = (registermap.getOrElse(str,-1))
        var r2:Int = (registermap.getOrElse(str1,-1))

        RegisterOp(dr, ""+stackcounter, "ldr")
        regs(dest) = stack(stackcounter)
        stackcounter = stackcounter + 1
        if(str!=""){


          RegisterOp(str, ""+stackcounter, "ldr")
          regs(r1) = stack(stackcounter)
          stackcounter = stackcounter + 1

        }
        if(str1!="PC" && str1!="") {
          RegisterOp(str1, ""+stackcounter, "ldr")
          regs(r2)  = stack(stackcounter)
          stackcounter= stackcounter +1
        }
        gotoLine(line+1)
      }
      //define a Constant
      case Constant(dr:String,str:Int) =>
      {
        constants.put(dr,str)
        gotoLine(line+1)
      }
      //Commands that effect the memory and the registers
      case RegisterOp(dr:String, sr:String, op:String)=>
      {
        var dest = (registermap.getOrElse(dr,-1))
        var source = 0

        val array1 = sr.replaceAll("[^a-zA-Z0-9 ,#]","").split(",")
        op match
        {
          case "adr"=>
            array1.foreach {(elem: String) =>
              if(elem.matches("[a-xA-X]+"))                            source += adressesVar.getOrElse(elem,-1)
              else if(elem.startsWith("#") && !elem.endsWith("[0-9]")) source += constants.getOrElse(elem.substring(1),-1)
              else if(elem =="PC")                                     source += PC
              else if(elem.startsWith("#") && elem.endsWith("[0-9]"))  source += elem.substring(1).toInt
              else                                                     source += regs(registermap.getOrElse(elem,-1))
            }
            regs(dest) = source
          case "ldr" =>
            array1.foreach {(elem: String) =>
              if(elem.matches("[a-xA-X]+"))                                    source += adressesVar.getOrElse(elem,-1)
              else if( elem.matches("[0-9]+"))                                 source += elem.substring(0).toInt
              else if( elem.startsWith("#"))                                   source += elem.substring(1).toInt
              else if(elem =="PC")                                             source += PC
              else if( !elem.matches("^[Rr]") && !elem.endsWith("[0-9]")) source += constants.getOrElse(elem.substring(1),-1)
              else                                                                  source += regs(registermap.getOrElse(elem,-1))
            }
            regs(dest) = adresses.getOrElse(source,-1)
          case "str" =>
            array1.foreach {(elem: String) =>
              if(elem.matches("[a-xA-X]+"))                               source += adressesVar.getOrElse(elem,-1)
              else if(elem =="PC")                                        source += PC
              else if( elem.matches("[0-9]+"))                            source += elem.substring(0).toInt
              else if(elem.startsWith("#"))                               source += elem.substring(1).toInt
              else if(elem.matches("^(R|r)") && !elem.endsWith("[0-9]"))  source += constants.getOrElse(elem.substring(1),-1)
              else                                                        source += regs(registermap.getOrElse(elem,-1))
            }
            adresses.put(source,regs(dest))
          case "ldm" =>
            var writeback1 = 0
            var writeback2 = 0
            if(dr.startsWith("!"))
            {
              dest = regs(registermap.getOrElse(dr.substring(1),-1))
              writeback1 = 1
            }
            if(sr.startsWith("!"))
            {

              writeback2 = 1
            }

            if (writeback2 != 1) {
              array1.zipWithIndex.foreach { case (elem: String, i: Int) =>
                regs(registermap.getOrElse(elem,-1)) = adresses(dest) + 1*i
              }
            }
            if(writeback1 != 1) regs(dest) = adresses(dest) + array1.length


          case "stm" =>
            var writeback1 = 0
            var writeback2 = 0
            if(dr.startsWith("!"))
            {
              dest = regs(registermap.getOrElse(dr.substring(1), -1))
              writeback1 = 1
            }
            if(sr.startsWith("!")) { writeback2 =1 }

            if (writeback2 != 1) {
              array1.zipWithIndex.foreach { case (elem: String, i: Int) =>
                adresses(dest + i) = adresses(regs(registermap.getOrElse(array1(i),-1)))
              }
            }
            if(writeback1!=1) adresses(dest) = adresses(dest) + array1.length


        }
        printRegs()
        gotoLine(line+1)

      }
      // test if you branch (go to the start of the label ) or not
      case Branching(branch:String, op:String,link:Int, branch2:String) =>
      {

        op match
        {
          case "NE" => if(NE()==1)gotoLine(labelsStart(branch))
          case "AL"=>  gotoLine(labelsStart(branch))
          case "EQ"=> if(EQ()==1) gotoLine(labelsStart(branch))
          case "CS"=> if(CS()==1) gotoLine(labelsStart(branch))
          case "MI"=> if(MI()==1) gotoLine(labelsStart(branch))
          case "CC"=> if(CC()==1) gotoLine(labelsStart(branch))
          case "PL"=> if(PL()==1) gotoLine(labelsStart(branch))
          case "HI"=> if(HI()==1) gotoLine(labelsStart(branch))
          case "LS"=> if(LS()==1) gotoLine(labelsStart(branch))
          case "GE"=> if(GE()==1) gotoLine(labelsStart(branch))
          case "LT"=> if(LT()==1) gotoLine(labelsStart(branch))
          case "LE"=> if(LE()==1) gotoLine(labelsStart(branch))
          case "GT"=> if(GT()==1) gotoLine(labelsStart(branch))
          case "NV"=> if(NV()==1) gotoLine(labelsStart(branch))
          case "BX"=> regs(15) = regs(registermap.getOrElse(branch,regs(15)))
        }

        gotoLine(line+1)
      }
      //define the label for the function
      case Labels(dr:String, line:Int) =>
      {
        labelsStart.put(dr,line)

        gotoLine(line+1)
      }
      //define the end of a label to prevent infinite recursion
      case LabelAddress(dr:String, reta:Int) =>
      {
        labelsEnd.put(dr, reta)

        gotoLine(line+1)
      }


      case Halt() =>
      case _ =>
    }
  }


  //The reset is pretty much copy pasted Cases that fit the format of the system





  def $(num:Int):Int = num

  object ASR {
    def apply(dr:String, sr1:String, sr2:String)
    {
      lines(current) = Math4(dr,sr1,sr2,"ASR")
      current +=1
    }
    def apply(dr:String, sr1:String, sr2:Int)
    {
      lines(current) = Math4(dr,sr1,sr2.toString,"ASR")
      current += 1
    }
  }

  object ASRS {
    def apply(dr:String, sr1:String, sr2:String)
    {
      lines(current) = Math4(dr,sr1,sr2,"ASRS")
      current +=1
    }
    def apply(dr:String, sr1:String, sr2:Int)
    {
      lines(current) = Math4(dr,sr1,sr2.toString,"ASRS")
      current += 1
    }
  }

  object LSL {
    def apply(dr:String, sr1:String, sr2:String)
    {
      lines(current) = Math4(dr,sr1,sr2,"LSL")
      current +=1
    }
    def apply(dr:String, sr1:String, sr2:Int)
    {
      lines(current) = Math4(dr,sr1,sr2.toString,"LSL")
      current += 1
    }
  }

  object LSLS {
    def apply(dr:String, sr1:String, sr2:String)
    {
      lines(current) = Math4(dr,sr1,sr2,"LSLS")
      current +=1
    }
    def apply(dr:String, sr1:String, sr2:Int)
    {
      lines(current) = Math4(dr,sr1,sr2.toString,"LSLS")
      current += 1
    }
  }

  object LSR {
    def apply(dr:String, sr1:String, sr2:String)
    {
      lines(current) = Math4(dr,sr1,sr2,"LSR")
      current +=1
    }
    def apply(dr:String, sr1:String, sr2:Int)
    {
      lines(current) = Math4(dr,sr1,sr2.toString,"LSR")
      current += 1
    }
  }

  object LSRS {
    def apply(dr:String, sr1:String, sr2:String)
    {
      lines(current) = Math4(dr,sr1,sr2,"LSRS")
      current +=1
    }
    def apply(dr:String, sr1:String, sr2:Int)
    {
      lines(current) = Math4(dr,sr1,sr2.toString,"LSRS")
      current += 1
    }
  }

  object ROR {
    def apply(dr:String, sr1:String, sr2:String)
    {
      lines(current) = Math4(dr,sr1,sr2,"ROR")
      current +=1
    }
    def apply(dr:String, sr1:String, sr2:Int)
    {
      lines(current) = Math4(dr,sr1,sr2.toString,"ROR")
      current += 1
    }
  }

  object RORS {
    def apply(dr:String, sr1:String, sr2:String)
    {
      lines(current) = Math4(dr,sr1,sr2,"RORS")
      current +=1
    }
    def apply(dr:String, sr1:String, sr2:Int)
    {
      lines(current) = Math4(dr,sr1,sr2.toString,"RORS")
      current += 1
    }
  }

  object DIV {
    def apply(dr:String, sr1:String, sr2:String)
    {
      lines(current) = Math4(dr,sr1,sr2,"/")
      current +=1
    }
    def apply(dr:String, sr1:String, sr2:Int)
    {
      lines(current) = Math4(dr,sr1,sr2.toString,"/")
      current += 1
    }
  }

  object ADD {
    def apply(dr:String, sr1:String, sr2:String)
    {
      lines(current) = Math4(dr,sr1,sr2,"+")
      current +=1
    }
    def apply(dr:String, sr1:String, sr2:Int)
    {
      lines(current) = Math4(dr,sr1,sr2.toString,"+")
      current += 1
    }

  }
  object SUB {
    def apply(dr:String, sr1:String, sr2:String)
    {
      lines(current) = Math4(dr,sr1,sr2,"-")
      current +=1
    }
    def apply(dr:String, sr1:String, sr2:Int)
    {
      lines(current) = Math4(dr,sr1,sr2.toString,"-")
      current += 1
    }

  }
  object MUL {
    def apply(dr:String, sr1:String, sr2:String)
    {
      lines(current) = Math4(dr,sr1,sr2,"*")
      current +=1
    }
    def apply(dr:String, sr1:String, sr2:Int)
    {
      lines(current) = Math4(dr,sr1,sr2.toString,"*")
      current += 1
    }

  }
  object MOV {

    def apply(dr:String,sr1:Int)
    {
      lines(current) = Move(dr,sr1.toString,"mov")
      current += 1
    }
    def apply(dr:String,sr1:String)
    {
      lines(current) = Move(dr,sr1,"mov")
      current += 1
    }
  }
  object MOVS {

    def apply(dr:String,sr1:Int)
    {
      lines(current) = Move(dr,sr1.toString,"movs")
      current += 1
    }
    def apply(dr:String,sr1:String)
    {
      lines(current) = Move(dr,sr1,"movs")
      current += 1
    }
  }
  object MVN {
    def apply(dr:String,sr1:Int)
    {
      lines(current) = Move(dr,sr1.toString,"movn")
      current += 1
    }
    def apply(dr:String,sr1:String)
    {
      lines(current) = Move(dr,sr1,"movn")
      current += 1
    }
  }
  object MVNS {

    def apply(dr:String,sr1:Int)
    {
      lines(current) = Move(dr,sr1.toString,"movns")
      current += 1
    }
    def apply(dr:String,sr1:String)
    {
      lines(current) = Move(dr,sr1,"movns")
      current += 1
    }
  }
  object ORR {

    def apply(dr:String,sr1:String,str2:String)
    {
      lines(current) = Bitwise(dr,sr1,str2,"|")
      current += 1
    }
    def apply(dr:String,sr1:String,str2:Int)
    {
      lines(current) = Bitwise(dr,sr1,str2.toString, "|")
      current += 1
    }
  }

  object AND {
    def apply(dr:String,sr1:String,str2:String)
    {
      lines(current) = Bitwise(dr,sr1,str2,"&")
      current += 1
    }
    def apply(dr:String,sr1:String,str2:Int)
    {
      lines(current) = Bitwise(dr,sr1,str2.toString, "&")
      current += 1
    }
  }

  object EOR {
    def apply(dr:String,sr1:String,str2:String)
    {
      lines(current) = Bitwise(dr,sr1,str2,"^")
      current += 1
    }
    def apply(dr:String,sr1:String,str2:Int)
    {
      lines(current) = Bitwise(dr,sr1,str2.toString, "^")
      current += 1
    }

  }
  object BIC {

    def apply(dr:String,sr1:String,str2:String)
    {
      lines(current) = Bitwise(dr,sr1,str2,"bic")
      current += 1
    }
    def apply(dr:String,sr1:String,str2:Int)
    {
      lines(current) = Bitwise(dr,sr1,str2.toString, "bic")
      current += 1
    }

  }

  object TST {
    def apply(dr:String, sr1:String)
    {
      lines(current) = Compare(dr,sr1,"tst")
      current +=1
    }
    def apply(dr:String, sr1:String, op:Int)
    {
      lines(current) = Compare(dr,sr1,"tst")
      current += 1
    }
  }
  object TEQ {
    def apply(dr:String, sr1:String)
    {
      lines(current) = Compare(dr,sr1,"teq")
      current +=1
    }
    def apply(dr:String, sr1:String, op:Int)
    {
      lines(current) = Compare(dr,sr1,"teq")
      current += 1
    }
  }
  object CMP {

    def apply(dr:String, sr1:String)
    {

      lines(current) = Compare(dr,sr1,"cmp")
      current +=1
    }
    def apply(dr:String, sr1:Int)
    {

      lines(current) = Compare(dr,sr1.toString,"cmp")
      current += 1
    }
  }
  object CMPN {
    def apply(dr:String, sr1:String)
    {
      lines(current) = Compare(dr,sr1,"cmpn")
      current +=1
    }
    def apply(dr:String, sr1:Int)
    {
      lines(current) = Compare(dr,sr1.toString,"cmpn")
      current += 1
    }
  }
  object PUSH {
    def apply(dr:String, sr1:String)
    {
      lines(current) =  StackPush(dr,sr1)
      current +=1
    }
  }
  object POP {
    def apply(dr:String)
    {
      lines(current) = StackPop(dr,"","")
      current +=1
    }
    def apply(dr:String, sr1:String)
    {
      lines(current) = StackPop(dr,sr1,"")
      current +=1
    }
    def apply(dr:String, sr1:String,sr2:String)
    {
      lines(current) = StackPop(dr,sr1,sr2)
      current +=1
    }

  }

  object EQU
  {
    def apply(dr:String, sr1:Int)
    {
      lines(current) = Constant(dr,sr1)
      current +=1
    }
  }

  object ADR
  {
    def apply(dr:String, sr1:String)
    {
      lines(current) = RegisterOp(dr,sr1,"adr")
      current +=1
    }
    def apply(dr:String, sr1:Int)
    {
      lines(current) = RegisterOp(dr,sr1.toString,"adr")
      current +=1
    }
  }

  object LDR
  {
    def apply(dr:String, sr1:String)
    {
      lines(current) = RegisterOp(dr,sr1,"ldr")
      current +=1
    }
    def apply(dr:String, sr1:Int)
    {
      lines(current) = RegisterOp(dr,sr1.toString,"ldr")
      current +=1
    }

  }
  object STR
  {
    def apply(dr:String, sr1:String)
    {
      lines(current) = RegisterOp(dr,sr1,"str")
      current +=1
    }
    def apply(dr:String, sr1:Int)
    {
      lines(current) = RegisterOp(dr,sr1.toString,"str")
      current +=1
    }

  }
  object STM
  {
    def apply(dr:String, sr1:String)
    {
      lines(current) = RegisterOp(dr,sr1,"stm")
      current +=1
    }
    def apply(dr:String, sr1:Int)
    {
      lines(current) = RegisterOp(dr,sr1.toString,"stm")
      current +=1
    }

  }
  object LDM
  {
    def apply(dr:String, sr1:String)
    {
      lines(current) = RegisterOp(dr,sr1,"ldm")
      current +=1
    }
    def apply(dr:String, sr1:Int)
    {
      lines(current) = RegisterOp(dr,sr1.toString,"ldm")
      current +=1
    }

  }
  object B
  {
    def apply(branch:String)
    {
      lines(current) = Branching(branch, "AL",0,branch)
      current +=1
    }
    def apply(branch:String, op:String)
    {
      lines(current) = Branching(branch, op,0,branch)
      current +=1
    }
  }

  object BEQ
  {
    def apply(branch:String)
    {
      lines(current) = Branching(branch, "EQ",1, branch)
      current +=1
    }
  }

  object BNE
  {
    def apply(branch:String)
    {
      lines(current) = Branching(branch, "NE",1, branch)
      current +=1
    }
  }

  object BCS
  {
    def apply(branch:String)
    {
      lines(current) = Branching(branch, "CS",1, branch)
      current +=1
    }
  }

  object BCC
  {
    def apply(branch:String)
    {
      lines(current) = Branching(branch, "CC",1, branch)
      current +=1
    }
  }

  object BMI
  {
    def apply(branch:String)
    {
      lines(current) = Branching(branch, "MI",1, branch)
      current +=1
    }
  }

  object BPL
  {
    def apply(branch:String)
    {
      lines(current) = Branching(branch, "PL",1, branch)
      current +=1
    }
  }

  object BVS
  {
    def apply(branch:String)
    {
      lines(current) = Branching(branch, "VS",1, branch)
      current +=1
    }
  }

  object BVC
  {
    def apply(branch:String)
    {
      lines(current) = Branching(branch, "VC",1, branch)
      current +=1
    }
  }

  object BHI
  {
    def apply(branch:String)
    {
      lines(current) = Branching(branch, "HI",1, branch)
      current +=1
    }
  }

  object BLS
  {
    def apply(branch:String)
    {
      lines(current) = Branching(branch, "LS",1, branch)
      current +=1
    }
  }

  object BGE
  {
    def apply(branch:String)
    {
      lines(current) = Branching(branch, "GE",1, branch)
      current +=1
    }
  }

  object BLT
  {
    def apply(branch:String)
    {
      lines(current) = Branching(branch, "LT",1, branch)
      current +=1
    }
  }

  object BLE
  {
    def apply(branch:String)
    {
      lines(current) = Branching(branch, "LE",1, branch)
      current +=1
    }
  }

  object BGT
  {
    def apply(branch:String)
    {
      lines(current) = Branching(branch, "GT",1, branch)
      current +=1
    }
  }

  object BX
  {
    def apply(branch:String)
    {
      lines(current) = Branching(branch, "BX",1, branch)
      current +=1
    }
  }

  object END
  {
    def apply(branch:String)
    {
      lines(current) = LabelAddress(branch,current)
      current +=1
    }

  }

  object LABEL
  {
    def apply(dr:String)
    {
      lines(current) = Labels(dr,current)
      current +=1
    }
  }

  def HALT(){
    lines(current) = Halt()
    makeHashMap()
    gotoLine(0)
  }
}
