
import scala.collection.mutable.{ HashMap, Stack }


object LogicTest extends Assembly {
  def main(args: Array[String]): Unit = {
   
    MOV (r1, 10)
    MOV (r0, 5)
    MOV (r10, 0)
    AND (r2, r1,r0)
    EOR (r3, r1,r0)
    BIC (r4, r0,r10)
    ORR (r5, r1,r0)
    
     HALT
  }
}
