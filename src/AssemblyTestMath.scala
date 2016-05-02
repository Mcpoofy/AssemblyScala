
import scala.collection.mutable.{ HashMap, Stack }


object AssemblyTestMath extends Assembly {
  def main(args: Array[String]): Unit = {
   
    MOV (r1, 10)
    MOV (r0, 5)
    MUL (r2, r1,r0)
    DIV (r3, r1,r0)
    ADD (r4, r1,r0)
    SUB (r5, r1,r0)
    
    
     HALT
  }
}
