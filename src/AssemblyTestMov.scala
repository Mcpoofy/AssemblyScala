
import scala.collection.mutable.{ HashMap, Stack }


object AssemblyTestMov extends Assembly {
  def main(args: Array[String]): Unit = {
   
    MOV (r1, 10)
    MOV (r0, 5)
    MVN (r2, r1)
    MOV (r3, 12)
    STR (r3, 0x20007)
    LDR (r4, 0x20007)
    MOV (r6, 16)
    STR (r6, "{r6,4}")
    LDR (r7, "{16,4}")
    
    
     HALT
  }
}
