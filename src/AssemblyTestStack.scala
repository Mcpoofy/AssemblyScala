
import scala.collection.mutable.{ HashMap, Stack }


object StackTest extends Assembly {
  def main(args: Array[String]): Unit = {
   
     MOV (r0, 10)
     MOV (r1, 20)
     
     PUSH(R0, R1)
     
     MOV(r0, 100)
     MOV(r1, 200)
     
     
     POP (R0)
     POP (R1)
     
     MOV(r5, 30)
    
     HALT
  }
}
