package models
import play.api._
import akka.actor._
import play.api.Play.current
import play.api.libs.concurrent.Akka
import play.api.libs.json._
import akka.actor.{ Actor, ActorRef, Props }
import scala.concurrent.ExecutionContext.Implicits.global
import com.github.nscala_time.time.Imports._
import play.api.libs.ws.WS
import play.api.Play.current
import java.sql.Timestamp

object DataLoggerCollector {
  import models.ModelHelper._

  var count = 0
  def start(host: String)(implicit context: ActorContext) = {
    val actorName = s"DataLoggerCollector_${count}"
    count += 1
    val collector = context.actorOf(Props(classOf[DataLoggerCollector], host), name = actorName)
    Logger.info(s"$actorName is created.")

    collector
  }

  case object StartCollect
  case class CollectHourData(start: Long)
  case class CollectMinData(start: Long)
  case class CollectAlarm(start: Long)
  case class CollectCalibration(start: Long)

  case class MtRecord(mtName: String, value: Float, status: String)
  case class RecordList(time: DateTime, mtDataList: Seq[MtRecord]){
    def toHourRecord(monitor:Monitor.Value)={
      val hr = Record.HourRecord(monitor.toString(), getSqlTimestamp(time))
      for(data <- mtDataList){
        data.mtName match {
          case "SO2"=>
            hr.so2 = Some(data.value)
            hr.so2_stat = Some(data.status)
          case "NOx"=>
            hr.nox = Some(data.value)
            hr.nox_stat = Some(data.status)
          case "NO2"=>
            hr.no2 = Some(data.value)
            hr.no2_stat = Some(data.status)
          case "NO"=>
            hr.no = Some(data.value)
            hr.no_stat = Some(data.status)
          case "CO"=>
            hr.co = Some(data.value)
            hr.co_stat = Some(data.status)
          case "O3"=>
            hr.o3 = Some(data.value)
            hr.o3_stat = Some(data.status)
          case "THC"=>
            hr.thc = Some(data.value)
            hr.thc_stat = Some(data.status)
          case "TS"=>
            hr.s = Some(data.value)
            hr.s_stat = Some(data.status)
          case "CH4"=>
            hr.ch4 = Some(data.value)
            hr.ch4_stat = Some(data.status)            
          case "NMHC"=>
            hr.nmhc = Some(data.value)
            hr.nmhc_stat = Some(data.status)              
          case "NH3"=>
            hr.nh3 = Some(data.value)
            hr.nh3_stat = Some(data.status)            
          case "TSP"=>
            hr.tsp = Some(data.value)
            hr.tsp_stat = Some(data.status)            
          case "PM10"=>
            hr.pm10 = Some(data.value)
            hr.pm10_stat = Some(data.status)
          case "PM25"=>
            hr.pm25 = Some(data.value)
            hr.pm25_stat = Some(data.status)
          case "WD_SPEED"=>
            hr.wind_speed = Some(data.value)
            hr.wind_speed_stat = Some(data.status)
          case "WD_DIR"=>
            hr.wind_dir = Some(data.value)
            hr.wind_dir_stat = Some(data.status)
          case "TEMP"=>
            hr.temp = Some(data.value)
            hr.temp_stat = Some(data.status)
          case "HUMID"=>
            hr.humid = Some(data.value)
            hr.humid_stat = Some(data.status)
          case "PRESS"=>
            hr.air_pressure = Some(data.value)
            hr.air_pressure_stat = Some(data.status)
          case "RAIN"=>
            hr.rain = Some(data.value)
            hr.rain_stat = Some(data.status)
        }
      }
      hr
    }
  }

  implicit val mtRecordReads = Json.reads[MtRecord]
  implicit val recordListReads = Json.reads[RecordList]
}

class DataLoggerCollector(host: String) extends Actor {
  import DataLoggerCollector._
  override def preStart() = {
    import scala.concurrent.duration._
    Akka.system.scheduler.scheduleOnce(Duration(5, SECONDS), self, StartCollect)
  }

  // override postRestart so we don't call preStart and schedule a new message
  override def postRestart(reason: Throwable) = {}

  def receive = handler

  import scala.concurrent._

  def handler(): Receive = {
    case StartCollect =>
      Future {
        blocking {
          val latestHour = Realtime.getLatestRecordTime(TableType.Hour)
          if (latestHour.isDefined)
            self ! CollectHourData(latestHour.get.getTime)
          else
            self ! CollectHourData(0)

          val latestMin = Realtime.getLatestRecordTime(TableType.Min)
          if (latestMin.isDefined)
            self ! CollectMinData(latestHour.get.getTime)
          else
            self ! CollectMinData(0)
        }
      }
    case CollectHourData(start) =>
      val now = DateTime.now().getMillis
      val startDateTime = new DateTime(start)
      if (startDateTime + 1.hour > DateTime.now) {
        //Delay
        val delay = new Duration(startDateTime + 1.hour, DateTime.now)
        import scala.concurrent.duration._
        Akka.system.scheduler.scheduleOnce(Duration(delay.getStandardMinutes, MINUTES), self, CollectHourData(start))
      } else {
        getHourData(start)
      }
    case CollectMinData(start) =>
  }

  def getMinData(start: Long) = {
    val currentTime = DateTime.now().getMillis
    val url = s"http://${host}/MinRecord/$start/$currentTime"
    val f = WS.url(url).get().map {
      response =>
        val epaData = response.json.validate[Seq[RecordList]]
        epaData.fold(
          error => {
            Logger.error(JsError.toFlatJson(error).toString())
            import scala.concurrent.duration._
            Akka.system.scheduler.scheduleOnce(Duration(1, MINUTES), self, CollectMinData(start))
          },
          dataList => {
            for(data <- dataList){
              val hr = data.toHourRecord(Monitor.withName("A013"))
              hr.save(TableType.Min)
            }

            import scala.concurrent.duration._
            Akka.system.scheduler.scheduleOnce(Duration(1, MINUTES), self, CollectMinData(start))
          })

    }
    f onFailure ({
      case ex: Throwable =>
        Logger.error(ex.getMessage)
        import scala.concurrent.duration._
        Akka.system.scheduler.scheduleOnce(Duration(1, MINUTES), self, CollectMinData(start))
    })
  }

  def getHourData(start: Long) = {
    val currentTime = DateTime.now().getMillis
    val url = s"http://${host}/HourRecord/$start/$currentTime"
    val f = WS.url(url).get().map {
      response =>
        val epaData = response.json.validate[Seq[RecordList]]
        epaData.fold(
          error => {
            Logger.error(JsError.toFlatJson(error).toString())
            import scala.concurrent.duration._
            Akka.system.scheduler.scheduleOnce(Duration(1, MINUTES), self, CollectHourData(start))
          },
          dataList => {
            for(data <- dataList){
              val hr = data.toHourRecord(Monitor.withName("A013"))
              hr.save(TableType.Hour)
            }
            import scala.concurrent.duration._
            Akka.system.scheduler.scheduleOnce(Duration(1, MINUTES), self, CollectHourData(start))
          })
    }

    f onFailure ({
      case ex: Throwable =>
        Logger.error(ex.getMessage)
        import scala.concurrent.duration._
        Akka.system.scheduler.scheduleOnce(Duration(1, MINUTES), self, CollectHourData(start))
    })
  }

}