package models
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api._

case class InstrumentCommand(cmd:String, var name:String, var instId:String)
object InstrumentCommand {
  implicit val write = Json.writes[InstrumentCommand]
  implicit val read = Json.reads[InstrumentCommand]
  
  val AutoCalibration = InstrumentCommand("AutoCalibration", "自動校正", "")
  val ManualZeroCalibration = InstrumentCommand("ManualZeroCalibration", "零點校正", "")
  val ManualSpanCalibration = InstrumentCommand("ManualSpanCalibration", "全幅校正", "")
  val BackToNormal = InstrumentCommand("BackToNormal", "中斷校正", "")
  
  val cmdSeq = Seq(AutoCalibration, ManualZeroCalibration, ManualSpanCalibration, BackToNormal)
  var cmdQueueMap = Map.empty[Monitor.Value, Seq[InstrumentCommand]]
  
  def pushCommand(monitor:Monitor.Value, cmd:InstrumentCommand)={
    val cmdQueue = cmdQueueMap.getOrElse(monitor, Seq.empty[InstrumentCommand])
    val newCmdQueue = cmdQueue:+(cmd)
    cmdQueueMap = cmdQueueMap + (monitor->newCmdQueue)    
  }
  
  def takeCommand(monitor:Monitor.Value)={
    val cmdQueue = cmdQueueMap.getOrElse(monitor, Seq.empty[InstrumentCommand])
    cmdQueueMap -= monitor
    cmdQueue
  }
}