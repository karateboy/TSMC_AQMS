package controllers

import java.io.FileOutputStream

import play.api._
import play.api.mvc._
import play.api.Logger
import models._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import javax.inject._

import play.api.i18n._

case class LatestRecordTime(time: Long)

case class MtRecord(mtName: String, value: Double, status: String)

case class RecordList(time: Long, mtDataList: Seq[MtRecord])

case class CalibrationJSON(monitorType: String, startTime: Long, endTime: Long, zero_val: Option[Double],
                           span_std: Option[Double], span_val: Option[Double]) {
  def zero_dev = zero_val map {
    Math.abs(_)
  }

  def span_dev = {
    if (span_val.isDefined && span_std.isDefined)
      Some(Math.abs(span_val.get - span_std.get))
    else
      None
  }

  def span_dev_ratio =
    if (span_dev.isDefined && span_std.isDefined && span_std.get != 0)
      Some(span_dev.get / span_std.get)
    else
      None
}

case class Alarm2JSON(time: Long, src: String, level: Int, info: String)

class DataLogger extends Controller {
  implicit val latestRecordTimeWrite = Json.writes[LatestRecordTime]
  implicit val mtRecordRead = Json.reads[MtRecord]
  implicit val RecordListRead = Json.reads[RecordList]
  implicit val CalibrationRead = Json.reads[CalibrationJSON]

  def getRecordRange(tabType: TableType.Value)(monitorStr: String) = Action {
    val monitor = Monitor.withName(monitorStr)
    val timeOpt = models.Realtime.getLatestMonitorRecordTime(tabType, monitor)

    val latestRecordTime = timeOpt.map {
      time =>
        LatestRecordTime(time.getMillis)
    }.getOrElse(LatestRecordTime(0))

    Ok(Json.toJson(latestRecordTime))
  }

  def getHourRecordRange = getRecordRange(TableType.Hour) _

  def getMinRecordRange = getRecordRange(TableType.Min) _

  def exportCSV(monitor: Monitor.Value)(recordList: RecordList) = {
    import scala.collection.mutable.StringBuilder
    val sb = new StringBuilder
    val tm = new DateTime(recordList.time)
    sb.append("Site,")
    sb.append("Date,")
    for (r <- recordList.mtDataList) {
      r.mtName match {
        case mt: String =>
          sb.append(mt + ", Status,")
      }
    }
    sb.deleteCharAt(sb.length - 1)
    sb.append("\n")
    sb.append(monitor.toString + ",")
    sb.append(tm.toString("YYYY-MM-dd HH:mm:ss") + ",")
    for (r <- recordList.mtDataList) {
      r.mtName match {
        case _: String =>
          sb.append(r.value.toFloat)
          sb.append(",")
          sb.append(r.status)
          sb.append(",")
      }
    }
    sb.deleteCharAt(sb.length - 1)
    sb.append("\n")
    sb.toString()
  }

  def toHourRecord(monitor: Monitor.Value)(recordList: RecordList) = {
    import java.sql.Timestamp
    val tm = new Timestamp(recordList.time)
    val hr = Record.HourRecord(monitor.toString(), tm)
    for (r <- recordList.mtDataList) {
      r.mtName match {
        case "SO2" =>
          hr.so2 = Some(r.value.toFloat)
          hr.so2_stat = Some(r.status)
        case "NOx" =>
          hr.nox = Some(r.value.toFloat)
          hr.nox_stat = Some(r.status)
        case "NO2" =>
          hr.no2 = Some(r.value.toFloat)
          hr.no2_stat = Some(r.status)
        case "NO" =>
          hr.no = Some(r.value.toFloat)
          hr.no_stat = Some(r.status)
        case "CO" =>
          hr.co = Some(r.value.toFloat)
          hr.co_stat = Some(r.status)
        case "CO2" =>
          Logger.warn("CO2 is not supported!")
        case "O3" =>
          hr.o3 = Some(r.value.toFloat)
          hr.o3_stat = Some(r.status)
        case "THC" =>
          hr.thc = Some(r.value.toFloat)
          hr.thc_stat = Some(r.status)
        case "TS" =>
          hr.s = Some(r.value.toFloat)
          hr.s_stat = Some(r.status)
        case "CH4" =>
          hr.ch4 = Some(r.value.toFloat)
          hr.ch4_stat = Some(r.status)
        case "NMHC" =>
          hr.nmhc = Some(r.value.toFloat)
          hr.nmhc_stat = Some(r.status)
        case "NH3" =>
          hr.nh3 = Some(r.value.toFloat)
          hr.nh3_stat = Some(r.status)
        case "TSP" =>
          hr.tsp = Some(r.value.toFloat)
          hr.tsp_stat = Some(r.status)
        case "PM10" =>
          hr.pm10 = Some(r.value.toFloat)
          hr.pm10_stat = Some(r.status)
        case "PM25" =>
          hr.pm25 = Some(r.value.toFloat)
          hr.pm25_stat = Some(r.status)
        case "WD_SPEED" =>
          hr.wind_speed = Some(r.value.toFloat)
          hr.wind_speed_stat = Some(r.status)
        case "WD_DIR" =>
          hr.wind_dir = Some(r.value.toFloat)
          hr.wind_dir_stat = Some(r.status)
        case "TEMP" =>
          hr.temp = Some(r.value.toFloat)
          hr.temp_stat = Some(r.status)
        case "HUMID" =>
          hr.humid = Some(r.value.toFloat)
          hr.humid_stat = Some(r.status)
        case "PRESS" =>
          hr.air_pressure = Some(r.value.toFloat)
          hr.air_pressure_stat = Some(r.status)
        case "RAIN" =>
          hr.rain = Some(r.value.toFloat)
          hr.rain_stat = Some(r.status)
      }
    }
    hr
  }

  def insertDataRecord(tabType: TableType.Value)(monitorStr: String) = Action(BodyParsers.parse.json) {
    implicit request =>
      val monitor = Monitor.withName(monitorStr)
      val result = request.body.validate[Seq[RecordList]]
      result.fold(err => {
        Logger.error(JsError(err).toString())
        BadRequest(Json.obj("ok" -> false, "msg" -> JsError(err).toString().toString()))
      },
        recordListSeq => {
          val hrList = recordListSeq.map {
            toHourRecord(monitor)
          }
          hrList.foreach { hr =>
            try {
              hr.save(tabType)
            } catch {
              case ex: Throwable =>
                Logger.error("Failed to insert=>", ex)
            }
          }
          //Export
          import play.api.Play.current
          val path = if (tabType == TableType.Hour)
            current.path.getAbsolutePath + "/export/hour/"
          else
            current.path.getAbsolutePath + "/export/minute/"

          try{
            recordListSeq map {
              recordList =>
                import java.io.FileOutputStream
                val time = new DateTime(recordList.time)
                val csvStr = exportCSV(monitor)(recordList)
                val fileName = s"${monitor.toString}_${time.toString("YYMMddHHmm")}.csv"
                val os = new FileOutputStream(path + fileName)
                os.write(csvStr.getBytes("UTF-8"))
                os.close()
            }
          }catch{
            case ex:Throwable=>
              Logger.error("failed to export csv", ex)
          }

          Ok(Json.obj("ok" -> true))
        })
  }

  def insertHourRecord = insertDataRecord(TableType.Hour) _

  def insertMinRecord = insertDataRecord(TableType.Min) _

  def getCalibrationRange(monitorStr: String) = Action {
    val monitor = Monitor.withName(monitorStr)
    val timeOpt = Calibration.getLatestMonitorRecordTime(monitor)
    val latestRecordTime = timeOpt.map {
      time =>
        LatestRecordTime(time.getMillis)
    }.getOrElse(LatestRecordTime(0))

    Ok(Json.toJson(latestRecordTime))
  }

  import Calibration._

  def toCalibrationItem(json: CalibrationJSON)(monitorStr: String) = {
    val monitor = Monitor.withName(monitorStr)
    val mtCode = mapMonitorToMtCode(json.monitorType)
    val mt = MonitorType.withName(mtCode)

    CalibrationItem(monitor, mt,
      new DateTime(json.startTime), new DateTime(json.endTime), json.span_std.map {
        _.toFloat
      },
      Some(0), json.zero_val.map {
        _.toFloat
      },
      json.zero_dev.map {
        _.toFloat
      }, Some(0),
      json.span_std.map {
        _.toFloat
      }, json.span_val.map {
        _.toFloat
      },
      json.span_dev.map {
        _.toFloat
      }, json.span_dev_ratio.map {
        _.toFloat * 100
      })
  }

  def insertCalibrationRecord(monitorStr: String) = Action(BodyParsers.parse.json) {
    implicit request =>
      val monitor = Monitor.withName(monitorStr)
      val result = request.body.validate[Seq[CalibrationJSON]]
      result.fold(err => {
        Logger.error(JsError(err).toString())
        BadRequest(Json.obj("ok" -> false, "msg" -> JsError(err).toString().toString()))
      },
        recordListSeq => {
          val calibrationList = recordListSeq.map {
            toCalibrationItem(_)(monitorStr)
          }
          calibrationList.foreach { calibration =>
            try {
              calibration.save
            } catch {
              case ex: Throwable =>
                Logger.error("Failed to insert calibration.", ex)
            }
          }
          Ok(Json.obj("ok" -> true))
        })
  }

  def mapMonitorToMtCode(mtName: String) = {
    mtName match {
      case "SO2" =>
        "A222"
      case "NOx" =>
        "A223"
      case "NO2" =>
        "A293"
      case "NO" =>
        "A283"
      case "CO" =>
        "A224"
      case "O3" =>
        "A225"
      case "THC" =>
        "A226"
      case "TS" =>
        "A221"
      case "CH4" =>
        "A286"
      case "NMHC" =>
        "A296"
      case "NH3" =>
        "A235"
      case "TSP" =>
        "A213"
      case "PM10" =>
        "A214"
      case "PM25" =>
        "A215"
      case "WD_SPEED" =>
        "C211"
      case "WD_DIR" =>
        "C212"
      case "TEMP" =>
        "C214"
      case "HUMID" =>
        "C215"
      case "PRESS" =>
        "C216"
      case "RAIN" =>
        "C213"
    }
  }

  def getAlarmRange(monitorStr: String) = Action {
    val monitor = Monitor.withName(monitorStr)
    val timeOpt = Alarm2.getLatestAlarmTime(monitor)
    val latestRecordTime = timeOpt.map {
      time =>
        LatestRecordTime(time.getMillis)
    }.getOrElse(LatestRecordTime(0))

    Ok(Json.toJson(latestRecordTime))
  }

  def toAlarm2(json: Alarm2JSON)(monitorStr: String) = {
    val monitor = Monitor.withName(monitorStr)
    Alarm2(monitor, new DateTime(json.time), json.src, json.level, json.info)
  }

  def insertAlarmRecord(monitorStr: String) = Action(BodyParsers.parse.json) {
    implicit request =>
      implicit val ar2JsonRead = Json.reads[Alarm2JSON]

      val monitor = Monitor.withName(monitorStr)
      val result = request.body.validate[Seq[Alarm2JSON]]
      result.fold(err => {
        Logger.error(JsError(err).toString())
        BadRequest(Json.obj("ok" -> false, "msg" -> JsError(err).toString().toString()))
      },
        alarm2JsonSeq => {
          val alarm2Seq = alarm2JsonSeq.map {
            toAlarm2(_)(monitorStr)
          }
          Alarm2.insertAlarmSeq(alarm2Seq)
          Ok(Json.obj("ok" -> true))
        })
  }

  def getInstrumentStatusRange(monitorStr: String) = Action {
    val monitor = Monitor.withName(monitorStr)
    val timeOpt = InstrumentStatus.getLatestTime(monitor)
    val latestRecordTime = timeOpt.map {
      time =>
        LatestRecordTime(time.getMillis)
    }.getOrElse(LatestRecordTime(0))

    Ok(Json.toJson(latestRecordTime))
  }

  import InstrumentStatus._

  def insertInstrumentStatusRecord(monitorStr: String) = Action(BodyParsers.parse.json) {
    implicit request =>
      import InstrumentStatus._
      val monitor = Monitor.withName(monitorStr)
      val result = request.body.validate[Seq[InstrumentStatusJSON]]
      result.fold(err => {
        Logger.error(JsError(err).toString())
        BadRequest(Json.obj("ok" -> false, "msg" -> JsError(err).toString().toString()))
      },
        instrumentStatusSeq => {
          InstrumentStatus.insert(monitor, instrumentStatusSeq)
          Ok(Json.obj("ok" -> true))
        })
  }

  def getInstrumentStatusTypeIds(monitorStr: String) = Action {
    val monitor = Monitor.withName(monitorStr)
    val instrumentStatusTypeMapOpt = Monitor.map(monitor).instrumentStatusTypeMapOpt
    val instrumentStatusTypeIds = instrumentStatusTypeMapOpt.map { istMap =>
      istMap.map { map =>
        map.instrumentId + map.statusTypeSeq.mkString("")
      }.mkString("")
    }.getOrElse("")

    Ok(Json.toJson(instrumentStatusTypeIds))
  }

  def updateInstrumentStatusTypeMap(monitorStr: String) = Action(BodyParsers.parse.json) {
    implicit request =>
      import Monitor._
      val monitor = Monitor.withName(monitorStr)
      val result = request.body.validate[Seq[InstrumentStatusTypeMap]]
      result.fold(err => {
        Logger.error(JsError(err).toString())
        BadRequest(Json.obj("ok" -> false, "msg" -> JsError(err).toString().toString()))
      },
        newMap => {
          val newMonitor = Monitor.map(monitor).updateInstrumentStatusTypeMap(Some(newMap.toList))
          Monitor.updateInstrumentStatusTypeMap(newMonitor)
          Ok(Json.obj("ok" -> true))
        })
  }

}