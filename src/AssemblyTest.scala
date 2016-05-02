
import scala.collection.mutable.{ HashMap, Stack }


object LoopTest extends Assembly {
  def main(args: Array[String]): Unit = {
   
     MOV (r0, 10)
     MOV (r1, 20)
     
     PUSH(R0, R1)
     
     POP (R2,R3,"PC")
     
     MOV(r5, 30)
    
     HALT
  }
}
