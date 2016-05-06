package controllers
import play.api._
import play.api.mvc._
import play.api.Logger
import models._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import play.api.libs.json._
import play.api.libs.functional.syntax._

case class LatestRecordTime(time: Long)
case class MtRecord(mtName: String, value: Double, status: String)
case class RecordList(time: Long, mtDataList: Seq[MtRecord])
case class CalibrationJSON(monitorType: String, startTime: Long, endTime: Long, zero_val: Double, 
                       span_std: Double, span_val: Double){
  def zero_dev = Math.abs(zero_val)
  def span_dev = Math.abs(span_val - span_std)
  def span_dev_ratio = span_dev / span_std
}

object DataLogger extends Controller {
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
          val hrList = recordListSeq.map { toHourRecord(monitor) }
          hrList.foreach { hr => hr.save(tabType) }
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
  def toCalibrationItem(json:CalibrationJSON)(monitorStr: String)={
    val monitor = Monitor.withName(monitorStr)
    val mtCode = mapMonitorToMtCode(json.monitorType)
    val mt = MonitorType.withName(mtCode)

    CalibrationItem(monitor, mt,
                             new DateTime(json.startTime), new DateTime(json.endTime), json.span_std.toFloat, 
                             0, json.zero_val.toFloat,
                             json.zero_dev.toFloat, 0,
                             json.span_std.toFloat, json.span_val.toFloat, 
                             json.span_dev.toFloat, json.span_dev_ratio.toFloat)
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
          val calibrationList = recordListSeq.map { toCalibrationItem(_)(monitorStr) } 
          calibrationList.foreach { _.save }
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
}