
import scala.collection.mutable.{ HashMap, Stack }


object Loop extends Assembly {
  def main(args: Array[String]): Unit = {
   
    MOV (r1, 10)
    MOV (r0, 5)
    LABEL ("test")
    ADD (r0,r0,1)
    CMP (r0, r1) 
    BLE ("test")
    
    
    
    
     HALT
  }
}
