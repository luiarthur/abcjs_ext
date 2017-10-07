package Music
import scala.scalajs.js
import js.JSConverters._


case class Scale(notes: List[Note], root: String) {
  def transposeWithKey(oldKey:String, newKey:String, usingKeySig:Boolean=false) = {
    Scale(notes.map(_.transposeWithKey(oldKey, newKey, usingKeySig)), newKey)
  }

  def blockChord(intervals: List[Int]): List[List[Note]] = {
    require(intervals.forall{i => 1 <= i && i <= notes.size},
       s"Intervals have to be between 1 and ${notes.size}") 

    val scale = notes.dropRight(1)

    // List of parallel scales
    val parScale = intervals.map{ i => 
      scale.drop(i-1) ++ scale.take(i-1).map{ _.transpose(12) } :+ 
      scale.drop(i-1).head.transpose(12)
    }

    // list of chords of size = `notes.size`
    return parScale.transpose
  }
}
