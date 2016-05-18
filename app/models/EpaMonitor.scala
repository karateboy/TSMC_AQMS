package models

/**
 * @author user
 */
case class EpaMonitor(name:String, id:Int, lat: Double, lng: Double)
object EpaMonitor extends Enumeration{
  val Hsinchu = Value
  val Hukou = Value
  val Zhudong = Value
  val Shalu = Value
  val Xitun = Value
  val Zhongming = Value
  val Shanhua = Value
  val Annan = Value
  val Tainan = Value
  
  val map=Map(
    Hsinchu->EpaMonitor("新竹站", 24,   24.805619, 120.972075),
    Hukou->EpaMonitor("湖口站", 22,     24.900142, 121.038653),
    Zhudong->EpaMonitor("竹東站", 23,   24.740644, 121.088903),
    Shalu->EpaMonitor("沙鹿站", 29,     24.225628, 120.568794),
    Xitun->EpaMonitor("西屯站", 32,     24.162197, 120.616917),
    Zhongming->EpaMonitor("忠明站", 31, 24.151958, 120.641092),
    Shanhua->EpaMonitor("善化站", 44,   23.115097, 120.297142),
    Annan->EpaMonitor("安南站", 45,     23.048197, 120.217500),
    Tainan->EpaMonitor("台南站", 46,    22.984581, 120.202617)
  )
  
  val idMap = map.map(r=>(r._2.id, r._1))
  
  val epaList = values.toList.sorted
}