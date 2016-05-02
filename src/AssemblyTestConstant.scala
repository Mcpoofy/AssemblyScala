
import scala.collection.mutable.{ HashMap, Stack }


object ConstantTest extends Assembly {
  def main(args: Array[String]): Unit = {
   
     EQU ("ten", 10)
     EQU ("twelve", 12)
     
     EQU ("ProjectGrade", 70)
     
     
     MOV(r1, "#ten")
     
     MOV(r2, "#twelve")
      
     MOV(r3, "#ProjectGrade")
     
     ADD(r4, r2,r3) 
     
     
     
     MOV(r5, 233)
     MOV(r6, 144)
     LABEL ("Fibanocci")
     MOV(r7,r6)
     SUB(r6, r5, r6)
     
     MOV(r5,r7)
     MOV(r7,0)
     CMP(r5,0)
     BNE ("Fibanocci")
     
     
    
     
     
     
     
     HALT
  }
}
