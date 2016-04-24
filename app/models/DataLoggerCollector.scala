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

object DataLoggerCollector {
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

  case class MtRecord(mtName: String, value: Double, status: String)
  case class RecordList(time: DateTime, mtDataList: Seq[MtRecord])

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
          data => {
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
          data => {
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