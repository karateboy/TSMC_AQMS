package models
import scalikejdbc._
import java.sql.Date
import java.sql.Timestamp
import scalikejdbc._
import play.api._
import com.github.nscala_time.time.Imports._
import com.github.nscala_time.time._
import models.ModelHelper._

object Calibration {

  case class CalibrationItem(monitor: Monitor.Value, monitorType: MonitorType.Value,
                             startTime: DateTime, endTime: DateTime, span: Option[Float], z_std: Option[Float], z_val: Option[Float],
                             zd_val: Option[Float], zd_pnt: Option[Float],
                             s_std: Option[Float], s_sval: Option[Float], sd_val: Option[Float], sd_pnt: Option[Float]) {
    def save() = {
      DB localTx {
        implicit session =>
          sql"""
            INSERT INTO Calibration
           ([DP_NO]
           ,[M_ITEM]
           ,[S_DateTime]
           ,[E_DateTime]
           ,[SPAN]
           ,[Z_STD]
           ,[Z_VAL]
           ,[ZD_VAL]
           ,[ZD_PNT]
           ,[S_STD]
           ,[S_SVAL]
           ,[SD_VAL]
           ,[SD_PNT]
           ,[CHK])
     VALUES
           (${monitor.toString}
           ,${monitorType.toString}
           ,${startTime: java.sql.Timestamp}
           ,${endTime: java.sql.Timestamp}
           ,$span
           ,$z_std
           ,$z_val
           ,$zd_val
           ,$zd_pnt
           ,$s_std
           ,$s_sval
           ,$sd_val
           ,$sd_pnt
           ,null)
            """.update.apply
      }
    }

    def success = {
      passStandard(z_val, MonitorType.map(monitorType).zd_law) &&
        passStandard(sd_pnt, MonitorType.map(monitorType).sd_law)
    }

    def canCalibrate = {
      z_val.isDefined &&
        s_std.isDefined && s_std.get != 0 &&
        s_sval.isDefined && s_sval.get != 0
    }

    def calibrate(valueOpt: Option[Float]) = {
      if (canCalibrate)
        for {
          value <- valueOpt
          zero <- z_val
          standard_span <- s_std
          calibration_span <- s_sval
        } yield (value - zero) * (standard_span / calibration_span)
      else
        valueOpt
    }
  }

  def mapper(rs: WrappedResultSet) = {
    val monitor = Monitor.withName(rs.string(1))
    val monitorType = MonitorType.withName(rs.string(2).replace("A4", "A2"))
    val startTime = rs.timestamp(3)
    val endTime = rs.timestamp(4)
    val span = rs.floatOpt(5)
    val z_std = rs.floatOpt(6)
    val z_val = rs.floatOpt(7)
    val zd_val = rs.floatOpt(8)
    val zd_pnt = rs.floatOpt(9)
    val s_std = rs.floatOpt(10)
    val s_sval = rs.floatOpt(11)
    val sd_val = rs.floatOpt(12)
    val sd_pnt = rs.floatOpt(13)
    CalibrationItem(monitor, monitorType, startTime, endTime, span, z_std, z_val, zd_val, zd_pnt, s_std, s_sval, sd_val, sd_pnt)
  }

  def calibrationQueryReport(monitor: Monitor.Value, start: Timestamp, end: Timestamp) = {
    DB readOnly { implicit session =>
      sql"""
      SELECT *
      FROM Calibration
      Where DP_NO=${monitor.toString} and S_DateTime >= ${start} and S_DateTime < ${end}
      Order by S_DateTime
      """.map { mapper }.list.apply
    }
  }

  def calibrationSummary(monitor: Monitor.Value, start: Timestamp, end: Timestamp) = {
    val reports = calibrationQueryReport(monitor, start, end)
    val pairs =
      for (r <- reports)
        yield r.monitorType -> r

    Map(pairs: _*).values.toList.sortBy { item => item.startTime }
  }

  def calibrationMonthly(monitor: Monitor.Value, monitorType: MonitorType.Value, start: DateTime) = {
    val end = start + 1.month
    val mtStr = monitorType.toString()
    val report =
      DB readOnly { implicit session =>
        sql"""
      SELECT *
      FROM Calibration
      Where DP_NO=${monitor.toString} and S_DateTime >= ${start} and S_DateTime < ${end} and M_ITEM = ${mtStr}
      Order by S_DateTime
      """.map { mapper }.list.apply

      }
    val pairs =
      for { r <- report } yield {
        r.startTime.toString("d") -> r
      }
    Map(pairs: _*)
  }

  def getLatestMonitorRecordTime(m: Monitor.Value) = {
    DB readOnly { implicit session =>
      sql"""
      SELECT TOP 1 S_DateTime
      FROM Calibration
      WHERE DP_NO = ${m.toString}
      ORDER BY S_DateTime  DESC
      """.map { r => r.timestamp(1) }.single.apply
    }
  }

  def passStandard(vOpt: Option[Float], stdOpt: Option[Float]) = {
    val retOpt =
      for {
        v <- vOpt
        std <- stdOpt
      } yield if (Math.abs(v) < Math.abs(std))
        true
      else
        false

    retOpt.fold(true)(v => v)
  }

  def getDailyCalibrationMap(monitor: Monitor.Value, date: DateTime) = {
    val begin = (date - 5.day).toDate
    val end = (date + 1.day).toDate

    val calibrationList =
      DB readOnly { implicit session =>
        sql"""
      SELECT *
      FROM Calibration
      Where DP_NO=${monitor.toString} and S_DateTime between ${begin} and ${end}
      Order by S_DateTime
      """.map { mapper }.list.apply

      }

    import scala.collection.mutable._
    val resultMap = Map.empty[MonitorType.Value, ListBuffer[(DateTime, Calibration.CalibrationItem)]]
    for (item <- calibrationList.filter { _.success } if item.monitorType != MonitorType.A293) {
      val lb = resultMap.getOrElseUpdate(item.monitorType, ListBuffer.empty[(DateTime, Calibration.CalibrationItem)])
      lb.append((item.endTime, item))
    }

    resultMap.map(kv => kv._1 -> kv._2.toList).toMap
    /*
    val map = calibrationList.filter { _.success }.map { cali => cali.monitorType -> cali }.toMap

    //Remove NO2
    map - MonitorType.A293
    * 
    */
  }

  def canCalibrate(mt: MonitorType.Value)(implicit date: DateTime, calibrationMap: Map[MonitorType.Value, List[(Imports.DateTime, Calibration.CalibrationItem)]]) = {
    calibrationMap.contains(mt) &&
      findCalibration(calibrationMap(mt)).isDefined
  }

  def findCalibration(calibrationList: List[(DateTime, Calibration.CalibrationItem)])(implicit date: DateTime, calibrationMap: Map[MonitorType.Value, List[(Imports.DateTime, Calibration.CalibrationItem)]]) = {
    val candidate = calibrationList.takeWhile(p => p._1 < date)
    if (candidate.length == 0)
      None
    else
      Some(candidate.last)
  }

  def doCalibrate(mt: MonitorType.Value)(implicit v: Option[Float], date: DateTime, calibrationMap: Map[MonitorType.Value, List[(Imports.DateTime, Calibration.CalibrationItem)]]) = {
    findCalibration(calibrationMap(mt)).get._2.calibrate(v)
  }

  //A293 => NO2, A296=>NMHC
  val interpolatedMonitorTypes = List(MonitorType.A293, MonitorType.A296)

  def mapMonitorTypeToMtCode(mt: MonitorType.Value) = {
    import MonitorType._
    mt match {
      case A222 => "SO2"
      case A223 => "NOx"
      case A293 => "NO2"
      case A283 => "NO"
      case A224 => "CO"
      case A225 => "O3"
      case A226 => "THC"
      case A221 => "TS"
      case A286 => "CH4"
      case A296 => "NMHC"
      case A235 => "NH3"
      case A213 => "TSP"
      case A214 => "PM10"
      case A215 => "PM25"
      case C211 => "WD_SPEED"
      case C212 => "WD_DIR"
      case C214 => "TEMP"
      case C215 => "HUMID"
      case C216 => "PRESS"
      case C213 => "RAIN"
    }
  }

  def exportDailyCalibrationCSV(monitor: Monitor.Value, day: DateTime) {

    val calibrationList = Calibration.calibrationQueryReport(monitor, day, day + 1.day)
    import scala.collection.mutable.StringBuilder
    val sb = new StringBuilder
    sb.append("Site,")
    sb.append("MonitorType,")
    sb.append("StartTime,")
    sb.append("EndTime,")
    sb.append("ZeroValue,")
    sb.append("SpanStd,")
    sb.append("SpanValue")
    sb.append("\n")

    for (cali <- calibrationList) {
      sb.append(monitor.toString + ",")
      sb.append(mapMonitorTypeToMtCode(cali.monitorType))
      sb.append(",")
      sb.append(cali.startTime.toString("YYYY-MM-dd HH:mm:ss") + ",")
      sb.append(cali.endTime.toString("YYYY-MM-dd HH:mm:ss") + ",")
      sb.append(cali.z_val.getOrElse(0f))
      sb.append(",")
      sb.append(cali.span.getOrElse(0f))
      sb.append(",")
      sb.append(cali.sd_val.getOrElse(0f))
      sb.append("\n")
    }

    import java.io.FileOutputStream
    import play.api.Play.current
    val path = current.path.getAbsolutePath + "/export/calibration/"
    val fileName = s"${monitor.toString}_${day.toString("YYMMdd")}.csv"
    val os = new FileOutputStream(path + fileName)
    os.write(sb.toString.getBytes("UTF-8"))
    os.close()
  }

}