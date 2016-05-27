package models

/**
 * @author user
 */
case class EpaMonitor(name:String, id:Int, lat: Double, lng: Double)
object EpaMonitor extends Enumeration{
  val Hsinchu = Value("新竹")
  val Hukou = Value("湖口")
  val Zhudong = Value("竹東")
  val Shalu = Value("沙鹿")
  val Xitun = Value("西屯")
  val Zhongming = Value("忠明")
  val Shanhua = Value("善化")
  val Annan = Value("安南")
  val Tainan = Value("臺南")
  
  val map=Map(
    Hsinchu->EpaMonitor("新竹", 24,   24.805619, 120.972075),
    Hukou->EpaMonitor("湖口", 22,     24.900142, 121.038653),
    Zhudong->EpaMonitor("竹東", 23,   24.740644, 121.088903),
    Shalu->EpaMonitor("沙鹿", 29,     24.225628, 120.568794),
    Xitun->EpaMonitor("西屯", 32,     24.162197, 120.616917),
    Zhongming->EpaMonitor("忠明", 31, 24.151958, 120.641092),
    Shanhua->EpaMonitor("善化", 44,   23.115097, 120.297142),
    Annan->EpaMonitor("安南", 45,     23.048197, 120.217500),
    Tainan->EpaMonitor("臺南", 46,    22.984581, 120.202617)
  )
  
  val idMap = map.map(r=>(r._2.id, r._1))
  
  val epaList = values.toList.sorted
}