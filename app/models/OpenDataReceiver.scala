package models

import akka.actor.{ Actor, ActorLogging, Props, ActorRef }
import javax.xml.ws.Holder
import play.api.Play.current
import play.api.libs.concurrent.Akka
import play.api._
import play.api.libs.ws._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import akka.actor.actorRef2Scala
import scala.concurrent.ExecutionContext.Implicits.global
import scalikejdbc._
import models.ModelHelper._

object OpenDataReceiver {
  val props = Props[OpenDataReceiver]
  case object GetEpaHourData

  var receiver: ActorRef = _
  def startup() = {
    receiver = Akka.system.actorOf(props, name = "openDataReceiver")
    Logger.info(s"OpenData receiver starts")
  }
}

import java.util.Date
case class HourData(
  SiteId:        String,
  SiteName:      String,
  ItemId:        String,
  ItemName:      String,
  ItemEngName:   String,
  ItemUnit:      String,
  MonitorDate:   Date,
  MonitorValues: Seq[Double])

class OpenDataReceiver extends Actor with ActorLogging {
  import OpenDataReceiver._
  import com.github.nscala_time.time.Imports._
  val timer = {
    import scala.concurrent.duration._
    Akka.system.scheduler.schedule(Duration(5, SECONDS), Duration(1, HOURS), receiver, GetEpaHourData)
  }

  import scala.xml._
  def getEpaHourData(start: DateTime, end: DateTime) {
    Logger.info(s"get EPA data start=${start.toString()} end=${end.toString()}")
    val limit = 1000
    def parser(node: Elem) = {
      import scala.xml.Node
      import scala.collection.mutable.Map
      val recordMap = Map.empty[EpaMonitor.Value, Map[DateTime, Map[MonitorType.Value, Double]]]

      def filter(dataNode: Node) = {
        val monitorDateOpt = dataNode \ "MonitorDate"
        val mDate = DateTime.parse(s"${monitorDateOpt.text.trim()}", DateTimeFormat.forPattern("YYYY-MM-dd"))
        start <= mDate && mDate < end
      }

      def processData(dataNode: Node) {
        val siteName = dataNode \ "SiteName"
        val itemId = dataNode \ "ItemId"
        val monitorDateOpt = dataNode \ "MonitorDate"

        try {
          //Filter interested EPA monitor
          if (EpaMonitor.nameMap.contains(siteName.text.trim()) &&
            MonitorType.eapIdMap.contains(itemId.text.trim().toInt)) {
            val epaMonitor = EpaMonitor.withName(siteName.text.trim())
            val monitorType = MonitorType.eapIdMap(itemId.text.trim().toInt)
            val mDate = DateTime.parse(s"${monitorDateOpt.text.trim()}", DateTimeFormat.forPattern("YYYY-MM-dd"))

            val monitorNodeValueSeq =
              for (v <- 0 to 23) yield {
                val monitorValue = try {
                  Some((dataNode \ "MonitorValue%02d".format(v)).text.trim().toDouble)
                } catch {
                  case x: Throwable =>
                    None
                }
                (mDate + v.hour, monitorValue)
              }

            val timeMap = recordMap.getOrElseUpdate(epaMonitor, Map.empty[DateTime, Map[MonitorType.Value, Double]])
            for { (mDate, mtValueOpt) <- monitorNodeValueSeq } {
              val mtMap = timeMap.getOrElseUpdate(mDate, Map.empty[MonitorType.Value, Double])
              for (mtValue <- mtValueOpt)
                mtMap.put(monitorType, mtValue)
            }
          }
        } catch {
          case x: Throwable =>
            Logger.error("failed", x)
        }
      }

      val data = node \ "Data"

      val qualifiedData = data.filter(filter)

      qualifiedData.map { processData }

      val updateCounts =
        for {
          monitorMap <- recordMap
          monitor = monitorMap._1
          timeMaps = monitorMap._2
          dateTime <- timeMaps.keys.toList.sorted
          mtValue <- timeMaps(dateTime)
        } yield {
          val MStation = EpaMonitor.map(monitor)
          val MItem = MonitorType.map(mtValue._1).epa_mapping.get
          val MDate: java.sql.Timestamp = dateTime
          DB localTx { implicit session =>
            sql"""
              UPDATE dbo.hour_data
              SET MValue = ${mtValue._2}
              WHERE MStation=${MStation.id} and MDate=${MDate} and MItem=${MItem};
    
              IF(@@ROWCOUNT = 0)
              BEGIN
                INSERT INTO dbo.hour_data (MStation, MDate, MItem, MValue)
                VALUES(${MStation.id}, ${MDate}, ${MItem}, ${mtValue._2})
              END
            """.update.apply
          }
        }

      updateCounts.sum
    }

    def getData(skip: Int) {
      val url = s"https://opendata.epa.gov.tw/webapi/api/rest/datastore/355000000I-000027/?format=xml&limit=${limit}&offset=${skip}&orderby=MonitorDate%20desc&token=00k8zvmeJkieHA9w13JvOw"
      val retFuture =
        WS.url(url).get().map {
          response =>
            try {
              parser(response.xml)
            } catch {
              case ex: Exception =>
                Logger.error(ex.toString())
                throw ex
            }
        }
      val ret = waitReadyResult(retFuture)
      Logger.info(s"EPA ${ret} records have been upserted.")
      
      if (ret < limit) {
        SystemConfig.setEpaLast(end)
      } else
        getData(skip + limit)
    }

    getData(0)
  }

  def receive = {
    case GetEpaHourData =>
      val start = SystemConfig.getEpaLast - 7.day
      val end = DateTime.now().withMillisOfDay(0)
      if (start < end) {
        getEpaHourData(start, end)
      }
  }

  override def postStop = {
    timer.cancel()
  }

}

