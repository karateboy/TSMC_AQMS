package models
import scalikejdbc._
import java.sql.Date
import java.sql.Timestamp
import scalikejdbc._
import play.api._
import com.github.nscala_time.time.Imports._
import com.github.nscala_time.time._
import models.ModelHelper._
import models.Record._

case class RealtimeStatus(data: Map[Monitor.Value, Map[MonitorType.Value, (Option[Float], Option[String])]])
//case class SixSecRecord(c911: Array[(Option[Float], Option[String])], c912: Array[(Option[Float], Option[String])])
case class WeatherStat(windSpeed: Option[Float], windDir: Option[Float])
case class AqiReport(aqi: Option[Float], sub_map: Map[AQI.Value, (Option[Float], Option[Float])])

object AQI extends Enumeration {
  val O3_8hr = Value
  val O3 = Value
  val pm25 = Value
  val pm10 = Value
  val CO_8hr = Value
  val SO2 = Value
  val SO2_24hr = Value
  val NO2 = Value
  def desc = Map(
    O3_8hr -> "臭氧\n(ppm)\n八小時平均值",
    O3 -> "臭氧\n(ppm)\n小時平均值",
    pm25 -> "PM2.5\n(μg/m3)\n平均值",
    pm10 -> "PM10\n(μg/m3 )\n平均值",
    CO_8hr -> "CO\n(ppm)\n8小時平均值",
    SO2 -> "SO2\n(ppb)\n小時平均值",
    SO2_24hr -> "SO2\n(ppb)\n24小時平均值",
    NO2 -> "NO2\n(ppb)\n小時平均值")
  def mtMap = Map(
    O3_8hr -> MonitorType.A225,
    O3 -> MonitorType.A225,
    pm25 -> MonitorType.A215,
    pm10 -> MonitorType.A214,
    CO_8hr -> MonitorType.A224,
    SO2 -> MonitorType.A222,
    SO2_24hr -> MonitorType.A222,
    NO2 -> MonitorType.A293)

  val realtimeList = List(O3_8hr, O3, pm25, pm10, CO_8hr, SO2, SO2_24hr, NO2)
  val dailyList = List(O3_8hr, O3, pm25, pm10, CO_8hr, SO2, NO2)
  
  def o3_8AQI(ov: Option[Float]) = {
    if (ov.isEmpty || ov.get / 1000 > 0.2f)
      None
    else
      Some {
        val v = ov.get / 1000

        if (v <= 0.054) {
          v / 0.054f * 50
        } else if (v <= 0.070f) {
          51 + (v - 0.055f) / (0.070f - 0.055f) * 50
        } else if (v <= 0.085f) {
          101 + (v - 0.071f) / (0.085f - 0.071f) * 50
        } else if (v <= 0.105f) {
          151 + (v - 0.086f) / (0.105f - 0.086f) * 50
        } else {
          201 + (v - 0.106f) / (0.2f - 0.106f) * 100
        }
      }
  }

  def o3AQI(ov: Option[Float]) = {
    if (ov.isEmpty || ov.get / 1000 < 0.125f)
      None
    else
      Some {
        val v = ov.get / 1000

        if (v <= 0.164f) {
          101 + (v - 0.125f) / (0.164f - 0.125f) * 50
        } else if (v <= 0.204f) {
          151 + (v - 0.165f) / (0.204f - 0.165f) * 50
        } else if (v <= 0.404f) {
          201 + (v - 0.205f) / (0.404f - 0.205f) * 100
        } else if (v <= 0.504f) {
          301 + (v - 0.405f) / (0.504f - 0.405f) * 100
        } else {
          401 + (v - 0.505f) / (0.604f - 0.505f) * 100
        }
      }
  }

  def pm25AQI(ov: Option[Float]) = {
    if (ov.isEmpty)
      None
    else
      Some {
        val v = ov.get

        if (v <= 15.4f) {
          v / 15.4f * 50
        } else if (v <= 35.4f) {
          51 + (v - 15.5f) / (35.4f - 15.5f) * 50
        } else if (v <= 54.4f) {
          101 + (v - 35.5f) / (54.4f - 35.5f) * 50
        } else if (v <= 150.4f) {
          151 + (v - 54.5f) / (150.4f - 54.5f) * 50
        } else if (v <= 250.4f) {
          201 + (v - 150.5f) / (250.4f - 150.5f) * 100
        } else if (v <= 350.4f) {
          301 + (v - 250.5f) / (350.4f - 250.5f) * 10
        } else {
          401 + (v - 350.5f) / (500.4f - 350.5f) * 100
        }
      }
  }

  def pm10AQI(ov: Option[Float]) = {
    if (ov.isEmpty)
      None
    else
      Some {
        val v = ov.get

        if (v <= 54f) {
          v / 54f * 50
        } else if (v <= 125f) {
          51 + (v - 55f) / (125f - 55f) * 50
        } else if (v <= 254f) {
          101 + (v - 126f) / (254f - 126f) * 50
        } else if (v <= 354f) {
          151 + (v - 255f) / (354f - 255f) * 50
        } else if (v <= 424f) {
          201 + (v - 355f) / (424f - 355f) * 100
        } else if (v <= 504f) {
          301 + (v - 425f) / (504f - 425f) * 100
        } else {
          401 + (v - 505f) / (604f - 505f) * 100
        }
      }
  }

  def co_8AQI(ov: Option[Float]) = {
    if (ov.isEmpty)
      None
    else
      Some {
        val v = ov.get

        if (v <= 4.4f) {
          v / 4.4f * 50
        } else if (v <= 9.4f) {
          51 + (v - 4.5f) / (9.4f - 4.5f) * 50
        } else if (v <= 12.4f) {
          101 + (v - 9.5f) / (12.4f - 9.5f) * 50
        } else if (v <= 15.4f) {
          151 + (v - 12.5f) / (15.4f - 12.5f) * 50
        } else if (v <= 30.4f) {
          201 + (v - 15.5f) / (30.4f - 15.5f) * 100
        } else if (v <= 40.4f) {
          301 + (v - 30.5f) / (40.4f - 30.4f) * 100
        } else {
          401 + (v - 40.5f) / (604f - 50.4f) * 100
        }
      }
  }

  def so2AQI(ov: Option[Float]) = {
    if (ov.isEmpty || ov.get >= 186)
      None
    else
      Some {
        val v = ov.get

        if (v <= 35f) {
          v / 35f * 50
        } else if (v <= 75f) {
          51 + (v - 36f) / (75f - 36f) * 50
        } else {
          101 + (v - 76f) / (185f - 76f) * 50
        }
      }
  }

  def so2_24AQI(ov: Option[Float]) = {
    if (ov.isEmpty || ov.get < 186)
      None
    else
      Some {
        val v = ov.get

        if (v <= 304f) {
          151 + (v - 186f) / (304f - 186f) * 50
        } else if (v <= 604f) {
          201 + (v - 305f) / (604f - 305f) * 100
        } else if (v <= 804f) {
          301 + (v - 605f) / (804f - 605f) * 100
        } else {
          401 + (v - 805f) / (1004f - 805f) * 100
        }
      }
  }

  def no2AQI(ov: Option[Float]) = {
    if (ov.isEmpty)
      None
    else
      Some {
        val v = ov.get

        if (v <= 53f) {
          v / 53f * 50
        } else if (v <= 100f) {
          51 + (v - 54f) / (100f - 54f) * 50
        } else if (v <= 360f) {
          101 + (v - 101f) / (360f - 101f) * 50
        } else if (v <= 649f) {
          151 + (v - 361f) / (649f - 361f) * 50
        } else if (v <= 1249f) {
          201 + (v - 650f) / (1249f - 650f) * 100
        } else if (v <= 1649f) {
          301 + (v - 1250f) / (1649f - 1250f) * 100
        } else {
          401 + (v - 1650f) / (2049f - 1650f) * 100
        }
      }
  }

  import Realtime._

  def getRealtimeAQI(lastHour: DateTime)(implicit session: DBSession = AutoSession) = {
    val result =
      for {
        m <- Monitor.mvList
      } yield {
        m -> getMonitorRealtimeAQI(m, lastHour)
      }
    Map(result: _*)
  }

  def getMonitorRealtimeAQI(monitor: Monitor.Value, thisHour: DateTime)(implicit session: DBSession = AutoSession) = {
    val o3 = getMonitorTypeAvg(monitor, MonitorType.withName("A225"), thisHour, thisHour + 1.hour, 1)
    val o3_8 = getMonitorTypeAvg(monitor, MonitorType.withName("A225"), thisHour - 7.hour, thisHour + 1.hour, 6)
    val pm10_12 = getMonitorTypeAvg(monitor, MonitorType.withName("A214"), thisHour - 11.hour, thisHour + 1.hour, 6)
    val pm10_4 = getMonitorTypeAvg(monitor, MonitorType.withName("A214"), thisHour - 3.hour, thisHour + 1.hour, 1)
    val pm10 = for (v1 <- pm10_12; v2 <- pm10_4) yield (v1 + v2) / 2

    val pm25_12 = getMonitorTypeAvg(monitor, MonitorType.withName("A215"), thisHour - 11.hour, thisHour + 1.hour, 6)
    val pm25_4 = getMonitorTypeAvg(monitor, MonitorType.withName("A215"), thisHour - 3.hour, thisHour + 1.hour, 1)
    val pm25 = for (v1 <- pm25_12; v2 <- pm25_4) yield (v1 + v2) / 2

    val co_8 = getMonitorTypeAvg(monitor, MonitorType.withName("A224"), thisHour - 7.hour, thisHour + 1.hour, 6)
    val so2 = getMonitorTypeAvg(monitor, MonitorType.withName("A222"), thisHour, thisHour + 1.hour, 1)
    val so2_24 = getMonitorTypeAvg(monitor, MonitorType.withName("A222"), thisHour - 23, thisHour + 1.hour, 1)
    val no2 = getMonitorTypeAvg(monitor, MonitorType.withName("A293"), thisHour, thisHour + 1.hour, 1)

    val result = Map[AQI.Value, (Option[Float], Option[Float])](
      AQI.O3_8hr -> (o3_8, o3_8AQI(o3_8)),
      AQI.O3 -> (o3, o3AQI(o3)),
      AQI.pm25 -> (pm25, pm25AQI(pm25)),
      AQI.pm10 -> (pm10, pm10AQI(pm10)),
      AQI.CO_8hr -> (co_8, co_8AQI(co_8)),
      AQI.SO2 -> (so2, so2AQI(so2)),
      AQI.SO2_24hr -> (so2_24, so2_24AQI(so2_24)),
      AQI.NO2 -> (no2, no2AQI(no2)))
    val sub_aqi = result.values.map(_._2)
    val aqi = sub_aqi.toList.max

    (aqi, result)
  }

  def getMonitorDailyAQI(monitor: Monitor.Value, thisDay: DateTime) = {
    val dayReport =
      getDailyReport(monitor, thisDay, MonitorType.aqiList)

    val mapPair =
      for {
        mtRecord <- dayReport.typeList
      } yield {
        val mt = mtRecord.monitorType
        val dataList = mtRecord.dataList map (d => (d._2, d._3))
        mt -> dataList
      }

    val dailyMap = mapPair.toMap

    getMonitorDailyAQIfromMap(0, dailyMap)
  }

  def getEpaDailyAQI(monitor: EpaMonitor.Value, current: DateTime)(implicit session: DBSession = AutoSession) = {
    val pm25 = getEpaMTypeMax(getEpaHourRecord(monitor, MonitorType.withName("A215"), current, current + 1.day), 16)
    val pm10 = getEpaMTypeMax(getEpaHourRecord(monitor, MonitorType.withName("A214"), current, current + 1.day), 16)
    val so2 = getEpaMTypeMax(getEpaHourRecord(monitor, MonitorType.withName("A222"), current, current + 1.day), 16)
    val no2 = getEpaMTypeMax(getEpaHourRecord(monitor, MonitorType.withName("A293"), current, current + 1.day), 16)
    
    val o3 = getEpaMTypeMax(getEpaHourRecord(monitor, MonitorType.withName("A225"), current, current + 1.day), 16)
    val o3_8 = getEpa8HourAvgMax(getEpaHourRecord(monitor, MonitorType.withName("A225"), current, current + 1.day), current, current + 1.day)    
    val co_8 = getEpa8HourAvgMax(getEpaHourRecord(monitor, MonitorType.withName("A224"), current, current + 1.day), current, current + 1.day) 

    val result = Map[AQI.Value, (Option[Float], Option[Float])](
      AQI.O3_8hr -> (o3_8, o3_8AQI(o3_8)),
      AQI.O3 -> (o3, o3AQI(o3)),
      AQI.pm25 -> (pm25, pm25AQI(pm25)),
      AQI.pm10 -> (pm10, pm10AQI(pm10)),
      AQI.CO_8hr -> (co_8, co_8AQI(co_8)),
      AQI.SO2 -> (so2, so2AQI(so2)),
      AQI.NO2 -> (no2, no2AQI(no2)))
    val sub_aqi = result.values.map(_._2)
    val aqi = sub_aqi.toList.max

    AqiReport(aqi, result)
  }

  def getMonitorDailyAQIfromMap(dayStartHour: Int,
                                map: Map[MonitorType.Value, List[(Option[Float], Option[String])]]) = {
    def getMonitorTypeAvg(mt: MonitorType.Value,
                          start: Int, end: Int, validMin: Int) = {
      val records = map(mt).slice(start, end)
      val validValues = records.filter(statusFilter(MonitorStatusFilter.ValidData)).map(_._1.get)
      val total = validValues.length
      if (total < validMin)
        None
      else {
        val sum = validValues.sum
        Some(sum / total)
      }
    }
    
    def getMonitorTypeMax(mt: MonitorType.Value,
                          start: Int, end: Int, validMin: Int) = {
      val records = map(mt).slice(start, end)
      val validValues = records.filter(statusFilter(MonitorStatusFilter.ValidData)).map(_._1.get)
      val total = validValues.length
      if (total < validMin)
        None
      else
        Some(validValues.max)
    }

    def getMonitorType8HourAvgMax(mt: MonitorType.Value, start: Int, end: Int) = {
      def get8hrAvg(data: List[(Option[Float], Option[String])]) = {
        val validValues = data.filter(statusFilter(MonitorStatusFilter.ValidData)).map(_._1.get)
        val total = validValues.length
        if (total < 6)
          None
        else
          Some(validValues.max)
      }

      val records = map(mt).slice(start, end)
      val movingAvg =
        for {
          start <- 0 to records.length - 8
          data = records.slice(start, start + 8)
        } yield get8hrAvg(data)

      movingAvg.max
    }

    val pm25 = getMonitorTypeMax(MonitorType.withName("A215"), dayStartHour, dayStartHour + 24, 16)
    val pm10 = getMonitorTypeMax(MonitorType.withName("A214"), dayStartHour, dayStartHour + 24, 16)
    val so2 = getMonitorTypeMax(MonitorType.withName("A222"), dayStartHour, dayStartHour + 24, 16)
    val no2 = getMonitorTypeMax(MonitorType.withName("A293"), dayStartHour, dayStartHour + 24, 16)
    
    val o3 = getMonitorTypeMax(MonitorType.withName("A225"), dayStartHour, dayStartHour + 24, 16)
    val o3_8 = getMonitorType8HourAvgMax(MonitorType.withName("A225"), dayStartHour, dayStartHour + 24)    
    val co_8 = getMonitorType8HourAvgMax(MonitorType.withName("A224"), dayStartHour, dayStartHour + 24)

    val result = Map[AQI.Value, (Option[Float], Option[Float])](
      AQI.O3_8hr -> (o3_8, o3_8AQI(o3_8)),
      AQI.O3 -> (o3, o3AQI(o3)),
      AQI.pm25 -> (pm25, pm25AQI(pm25)),
      AQI.pm10 -> (pm10, pm10AQI(pm10)),
      AQI.CO_8hr -> (co_8, co_8AQI(co_8)),
      AQI.SO2 -> (so2, so2AQI(so2)),
      AQI.NO2 -> (no2, no2AQI(no2)))
    val sub_aqi = result.values.map(_._2)
    val aqi = sub_aqi.toList.max

    AqiReport(aqi, result)
  }

  def getRealtimeAqiTrend(m: Monitor.Value, start: DateTime, end: DateTime) = {
    import Record._
    val duration = new Duration(start, end)
    val dayReports =
      for (delta <- 0 to duration.getStandardDays.toInt) yield {
        getDailyReport(m, start + delta.day, MonitorType.aqiList)
      }
    val lastDayReport = getDailyReport(m, start - 1.day, MonitorType.aqiList)

    val totalReport = dayReports.foldLeft(lastDayReport)((d1, d2) =>
      {
        val zipList = d1.typeList.zip(d2.typeList)
        val newTlist = zipList.map { z =>
          MonitorTypeRecord(z._1.monitorType, z._1.dataList ++ z._2.dataList, z._1.stat)

        }
        DailyReport(newTlist)
      })

    val mapPair =
      for {
        mtRecord <- totalReport.typeList
      } yield {
        val mt = mtRecord.monitorType
        val dataList = mtRecord.dataList map (d => (d._2, d._3))
        mt -> dataList
      }

    val dailyMap = mapPair.toMap

    val timePsiPair =
      for {
        hr <- 24 to (24 + duration.getStandardDays.toInt * 24)
      } yield {
        start + (hr - 24).hour -> getMonitorRealtimeAQIfromMap(hr, dailyMap)._1
      }
    timePsiPair.filter(_._2.isDefined).map(p => p._1 -> p._2.get).toMap
  }

  def getMonitorRealtimeAQIfromMap(thisHour: Int,
                                   map: Map[MonitorType.Value, List[(Option[Float], Option[String])]]) = {
    def getMonitorTypeAvg(mt: MonitorType.Value,
                          start: Int, end: Int, validMin: Int) = {
      val records = map(mt).slice(start, end)
      val validValues = records.filter(statusFilter(MonitorStatusFilter.ValidData)).map(_._1.get)
      val total = validValues.length
      if (total < validMin)
        None
      else {
        val sum = validValues.sum
        Some(sum / total)
      }
    }

    val o3 = getMonitorTypeAvg(MonitorType.withName("A225"), thisHour, thisHour + 1, 1)
    val o3_8 = getMonitorTypeAvg(MonitorType.withName("A225"), thisHour - 7, thisHour + 1, 6)
    val pm10_12 = getMonitorTypeAvg(MonitorType.withName("A214"), thisHour - 11, thisHour + 1, 6)
    val pm10_4 = getMonitorTypeAvg(MonitorType.withName("A214"), thisHour - 3, thisHour + 1, 1)
    val pm10 = for (v1 <- pm10_12; v2 <- pm10_4) yield (v1 + v2) / 2

    val pm25_12 = getMonitorTypeAvg(MonitorType.withName("A215"), thisHour - 11, thisHour + 1, 6)
    val pm25_4 = getMonitorTypeAvg(MonitorType.withName("A215"), thisHour - 3, thisHour + 1, 1)
    val pm25 = for (v1 <- pm25_12; v2 <- pm25_4) yield (v1 + v2) / 2

    val co_8 = getMonitorTypeAvg(MonitorType.withName("A224"), thisHour - 7, thisHour + 1, 6)
    val so2 = getMonitorTypeAvg(MonitorType.withName("A222"), thisHour, thisHour + 1, 1)
    val so2_24 = getMonitorTypeAvg(MonitorType.withName("A222"), thisHour - 23, thisHour + 1, 1)
    val no2 = getMonitorTypeAvg(MonitorType.withName("A293"), thisHour, thisHour + 1, 1)

    val result = Map[AQI.Value, (Option[Float], Option[Float])](
      AQI.O3_8hr -> (o3_8, o3_8AQI(o3_8)),
      AQI.O3 -> (o3, o3AQI(o3)),
      AQI.pm25 -> (pm25, pm25AQI(pm25)),
      AQI.pm10 -> (pm10, pm10AQI(pm10)),
      AQI.CO_8hr -> (co_8, co_8AQI(co_8)),
      AQI.SO2 -> (so2, so2AQI(so2)),
      AQI.SO2_24hr -> (so2_24, so2_24AQI(so2_24)),
      AQI.NO2 -> (no2, no2AQI(no2)))
    val sub_aqi = result.values.map(_._2)
    val aqi = sub_aqi.toList.max

    (aqi, result)
  }

  def getEpaRealtimeAQI(monitor: EpaMonitor.Value, thisHour: DateTime)(implicit session: DBSession = AutoSession) = {
    val o3 = getEpaMTypeAvg(getEpaHourRecord(monitor, MonitorType.withName("A225"), thisHour, thisHour + 1.hour), 1)
    val o3_8 = getEpaMTypeAvg(getEpaHourRecord(monitor, MonitorType.withName("A225"), thisHour - 7.hour, thisHour + 1.hour), 6)
    val pm10_12 = getEpaMTypeAvg(getEpaHourRecord(monitor, MonitorType.withName("A214"), thisHour - 11.hour, thisHour + 1.hour), 6)
    val pm10_4 = getEpaMTypeAvg(getEpaHourRecord(monitor, MonitorType.withName("A214"), thisHour - 3.hour, thisHour + 1.hour), 1)
    val pm10 = for (v1 <- pm10_12; v2 <- pm10_4) yield (v1 + v2) / 2

    val pm25_12 = getEpaMTypeAvg(getEpaHourRecord(monitor, MonitorType.withName("A215"), thisHour - 11.hour, thisHour + 1.hour), 6)
    val pm25_4 = getEpaMTypeAvg(getEpaHourRecord(monitor, MonitorType.withName("A215"), thisHour - 3.hour, thisHour + 1.hour), 1)
    val pm25 = for (v1 <- pm25_12; v2 <- pm25_4) yield (v1 + v2) / 2

    val co_8 = getEpaMTypeAvg(getEpaHourRecord(monitor, MonitorType.withName("A224"), thisHour - 7.hour, thisHour + 1.hour), 6)
    val so2 = getEpaMTypeAvg(getEpaHourRecord(monitor, MonitorType.withName("A222"), thisHour, thisHour + 1.hour), 1)
    val so2_24 = getEpaMTypeAvg(getEpaHourRecord(monitor, MonitorType.withName("A222"), thisHour - 23, thisHour + 1.hour), 1)
    val no2 = getEpaMTypeAvg(getEpaHourRecord(monitor, MonitorType.withName("A293"), thisHour, thisHour + 1.hour), 1)

    val result = Map[AQI.Value, (Option[Float], Option[Float])](
      AQI.O3_8hr -> (o3_8, o3_8AQI(o3_8)),
      AQI.O3 -> (o3, o3AQI(o3)),
      AQI.pm25 -> (pm25, pm25AQI(pm25)),
      AQI.pm10 -> (pm10, pm10AQI(pm10)),
      AQI.CO_8hr -> (co_8, co_8AQI(co_8)),
      AQI.SO2 -> (so2, so2AQI(so2)),
      AQI.SO2_24hr -> (so2_24, so2_24AQI(so2_24)),
      AQI.NO2 -> (no2, no2AQI(no2)))
    val sub_aqi = result.values.map(_._2)
    val aqi = sub_aqi.toList.max

    (aqi, result)
  }

  def getMonitorMonthlyAQI(monitor: Monitor.Value, start: DateTime) = {
    val days = getPeriods(start, start + 1.month, 1.day)
    for (day <- days)
      yield getMonitorDailyAQI(monitor, day)
  }

}

object Realtime {
  def getRealtimeMinStatus(current: DateTime, privilege: Privilege) = {

    DB readOnly { implicit session =>
      val tab_name = Record.getTabName(TableType.Min)
      val hrs =
        sql"""
              SELECT *
              FROM ${tab_name}
              WHERE M_DateTime = ${current}
             """.map { Record.mapper }.list.apply

      val rt_result =
        for { m <- privilege.allowedMonitors } yield {
          import scala.collection.mutable.Map
          val hrMap: Map[Monitor.Value, HourRecord] = Map()

          for (hr <- hrs) {
            val m = Monitor.withName(hr.name)
            if (!hrMap.contains(m))
              hrMap += (m -> hr)
          }

          val hr = hrMap.getOrElse(m, emptyRecord(Monitor.map(m).id, current))
          val type_record = monitorTypeProject2
            .map(
              t => (t._1 -> t._2(hr)))
          (m -> type_record)
        }
      Map(rt_result: _*)
    }
  }

  def statusFilter(msf: MonitorStatusFilter.Value)(data: (Option[Float], Option[String])): Boolean = {
    if (data._2.isEmpty)
      return false

    val stat = data._2.get
    MonitorStatusFilter.isMatched(msf, stat)
  }

  def getMonitorTypeAvg(monitor: Monitor.Value, monitorType: MonitorType.Value,
                        start: DateTime, end: DateTime, validMin: Int)(implicit session: DBSession = AutoSession) = {
    val records = getHourRecords(monitor, start, end)
    val typeValues = records.map { hr => monitorTypeProject2(monitorType)(hr) }
    val duration = new Duration(start, end)
    val nHour = duration.getStandardHours
    val validValues = typeValues.filter(statusFilter(MonitorStatusFilter.ValidData)).map(_._1.get)
    val total = validValues.length
    if (total < validMin)
      None
    else {
      val sum = validValues.sum
      Some(sum / total)
    }
  }

  def getMonitorTypeAvg(records: List[HourRecord], monitorType: MonitorType.Value, validMin: Int) = {
    val typeValues = records.map { hr => monitorTypeProject2(monitorType)(hr) }
    val validValues = typeValues.filter(statusFilter(MonitorStatusFilter.ValidData)).map(_._1.get)
    val total = validValues.length
    if (total < validMin)
      None
    else {
      Some(validValues.sum / total)
    }
  }

  def getEpaMTypeAvg(records: List[EpaHourRecord], validMin: Int) = {
    val values = records.map { _.value }
    if (values.length < validMin)
      None
    else {
      Some(values.sum / values.length)
    }
  }

  def getMonitorTypeMax(records: List[HourRecord], monitorType: MonitorType.Value) = {
    val typeValues = records.map { hr => monitorTypeProject2(monitorType)(hr) }
    val validValues = typeValues.filter(statusFilter(MonitorStatusFilter.ValidData)).map(_._1.get)
    val total = validValues.length
    if (total == 0)
      None
    else {
      Some(validValues.max)
    }
  }

  def getEpaMTypeMax(records: List[EpaHourRecord], validMin: Int) = {
    val values = records.map { _.value }
    if (values.length < validMin)
      None
    else {
      Some(values.max)
    }
  }

  def getMonitorType8HourAvgMax(monitor: Monitor.Value, monitorType: MonitorType.Value, start: DateTime, end: DateTime)(implicit session: DBSession = AutoSession) = {
    def EightHourAvg(start: DateTime): List[Option[Float]] = {
      if (start + 8.hour >= end)
        Nil
      else
        getMonitorTypeAvg(monitor, monitorType, start, start + 8.hour, 8) :: EightHourAvg(start + 1.hours)
    }

    val avgs = EightHourAvg(start)

    avgs.max
  }

  def getEpa8HourAvgMax(records: List[EpaHourRecord], start: DateTime, end: DateTime) = {
    def EightHourAvg(start: DateTime): List[Option[Float]] = {
      if (start + 8.hour >= end)
        Nil
      else {
        val recs = records.filter { r => r.time >= start && r.time < start + 8.hour }
        getEpaMTypeMax(recs, 8) :: EightHourAvg(start + 1.hours)
      }
    }

    val avgs = EightHourAvg(start)

    avgs.max
  }

  def pm10PSI(ov: Option[Float]) = {
    if (ov.isEmpty)
      None
    else
      Some {
        val v = ov.get
        if (v >= 0 && v <= 50) {
          v
        } else if (v <= 100) {
          50 + (v - 50) * 50 / 100
        } else if (v <= 350) {
          100 + (v - 150) * 100 / (350 - 150)
        } else if (v <= 420) {
          200 + (v - 350) * 100 / (420 - 350)
        } else if (v <= 500) {
          300 + (v - 420) * 100 / (500 - 420)
        } else {
          400 + (v - 500) * 100 / 100
        }
      }
  }

  def so2PSI(ov: Option[Float]) = {
    if (ov.isEmpty)
      None
    else
      Some {
        val v = ov.get

        if (v <= 30) {
          v / 30 * 50
        } else if (v <= 140) {
          50 + (v - 30) * 50 / (140 - 30)
        } else if (v <= 300) {
          100 + (v - 140) * 100 / (300 - 140)
        } else if (v <= 600) {
          200 + (v - 300) * 100 / (600 - 300)
        } else if (v <= 800) {
          300 + (v - 600) * 100 / (800 - 600)
        } else {
          400 + (v - 800) * 100 / (1000 - 800)
        }
      }
  }

  def coPSI(ov: Option[Float]) = {
    if (ov.isEmpty)
      None
    else
      Some {
        val v = ov.get

        if (v <= 4.5) {
          v / 4.5f * 50f
        } else if (v <= 9) {
          (50 + (v - 4.5) * 50 / (9 - 4.5)).toFloat
        } else if (v <= 15) {
          100 + (v - 9) * 100 / (15 - 9)
        } else if (v <= 30) {
          200 + (v - 15) * 100 / (30 - 9)
        } else if (v <= 40) {
          300 + (v - 30) * 100 / (40 - 30)
        } else {
          400 + (v - 40) * 100 / (50 - 40)
        }
      }
  }

  def o3PSI(ov: Option[Float]) = {
    if (ov.isEmpty)
      None
    else
      Some {
        val v = ov.get

        if (v <= 60) {
          v / 60 * 50
        } else if (v <= 120) {
          50 + (v - 60) * 50 / (120 - 60)
        } else if (v <= 200) {
          100 + (v - 120) * 100 / (200 - 120)
        } else if (v <= 400) {
          200 + (v - 200) * 100 / (400 - 200)
        } else if (v <= 500) {
          300 + (v - 400) * 100 / (500 - 400)
        } else {
          400 + (v - 500) * 100 / (600 - 500)
        }
      }
  }

  def no2PSI(ov: Option[Float]) = {
    if (ov.isEmpty)
      None
    else
      Some {
        val v = ov.get

        if (v < 600) {
          0
        } else if (v <= 1200) {
          200 + (v - 600) * 100 / (1200 - 600)
        } else if (v <= 1600) {
          300 + (v - 1200) * 100 / (1600 - 1200)
        } else {
          400 + (v - 1600) * 100 / (2000 - 1600)
        }
      }
  }

  def getMonitorRealtimePm25(monitor: Monitor.Value, lastHour: DateTime)(implicit session: DBSession = AutoSession) = {
    val pm25_12Opt = getMonitorTypeAvg(monitor, MonitorType.withName("A215"), lastHour - 11.hour, lastHour + 1.hour, 8)
    val pm25_4Opt = getMonitorTypeAvg(monitor, MonitorType.withName("A215"), lastHour - 3.hour, lastHour + 1.hour, 3)

    for {
      pm25_12 <- pm25_12Opt
      pm25_4 <- pm25_4Opt
    } yield Math.round(pm25_12 / 2 + pm25_4 / 2)
  }

  def getMonitorRealtimePSIfromMap(thisHour: Int,
                                   map: Map[MonitorType.Value, List[(Option[Float], Option[String])]]) = {
    def getMonitorTypeAvg(mt: MonitorType.Value,
                          start: Int, end: Int, validMin: Int) = {
      val records = map(mt).slice(start, end)
      val validValues = records.filter(statusFilter(MonitorStatusFilter.ValidData)).map(_._1.get)
      val total = validValues.length
      if (total < validMin)
        None
      else {
        val sum = validValues.sum
        Some(sum / total)
      }
    }

    val pm10_12 = getMonitorTypeAvg(MonitorType.A214, thisHour - 11, thisHour + 1, 8)
    val pm10_4 = getMonitorTypeAvg(MonitorType.A214, thisHour - 3, thisHour + 1, 3)
    val pm10 = if (pm10_12.isDefined && pm10_4.isDefined)
      Some((pm10_12.get + pm10_4.get) / 2)
    else
      None

    val so2_24 = getMonitorTypeAvg(MonitorType.A222, thisHour - 23, thisHour + 1, 16)
    val co_8 = getMonitorTypeAvg(MonitorType.A224, thisHour - 7, thisHour + 1, 6)
    val o3 = getMonitorTypeAvg(MonitorType.A225, thisHour, thisHour + 1, 1)
    val no2 = getMonitorTypeAvg(MonitorType.A293, thisHour, thisHour + 1, 1)
    val result = Map[MonitorType.Value, (Option[Float], Option[Float])](
      MonitorType.withName("A214") -> (pm10, pm10PSI(pm10)),
      MonitorType.withName("A222") -> (so2_24, so2PSI(so2_24)),
      MonitorType.withName("A224") -> (co_8, coPSI(co_8)),
      MonitorType.withName("A225") -> (o3, o3PSI(o3)),
      MonitorType.withName("A293") -> (no2, no2PSI(no2)))
    val sub_psi = result.values.map(_._2)
    val psi = sub_psi.toList.max

    (psi, result)

  }

  def getMonitorRealtimePSI(monitor: Monitor.Value, thisHour: DateTime)(implicit session: DBSession = AutoSession) = {
    val pm10_12 = getMonitorTypeAvg(monitor, MonitorType.withName("A214"), thisHour - 11.hour, thisHour + 1.hour, 8)
    val pm10_4 = getMonitorTypeAvg(monitor, MonitorType.withName("A214"), thisHour - 3.hour, thisHour + 1.hour, 3)
    val pm10 = if (pm10_12.isDefined && pm10_4.isDefined)
      Some((pm10_12.get + pm10_4.get) / 2)
    else
      None

    val so2_24 = getMonitorTypeAvg(monitor, MonitorType.withName("A222"), thisHour - 23.hour, thisHour + 1.hour, 16)
    val co_8 = getMonitorTypeAvg(monitor, MonitorType.withName("A224"), thisHour - 7.hour, thisHour + 1.hour, 6)
    val o3 = getMonitorTypeAvg(monitor, MonitorType.withName("A225"), thisHour, thisHour + 1.hour, 1)
    val no2 = getMonitorTypeAvg(monitor, MonitorType.withName("A293"), thisHour, thisHour + 1.hour, 1)
    val result = Map[MonitorType.Value, (Option[Float], Option[Float])](
      MonitorType.withName("A214") -> (pm10, pm10PSI(pm10)),
      MonitorType.withName("A222") -> (so2_24, so2PSI(so2_24)),
      MonitorType.withName("A224") -> (co_8, coPSI(co_8)),
      MonitorType.withName("A225") -> (o3, o3PSI(o3)),
      MonitorType.withName("A293") -> (no2, no2PSI(no2)))
    val sub_psi = result.values.map(_._2)
    val psi = sub_psi.toList.max

    (psi, result)
  }

  def getEpaRealtimePSI(monitor: EpaMonitor.Value, lastHour: DateTime)(implicit session: DBSession = AutoSession) = {

    val pm10_12 = getEpaMTypeAvg(getEpaHourRecord(monitor, MonitorType.withName("A214"), lastHour - 11.hour, lastHour + 1.hour), 8)
    val pm10_4 = getEpaMTypeAvg(getEpaHourRecord(monitor, MonitorType.withName("A214"), lastHour - 3.hour, lastHour + 1.hour), 3)
    val pm10 = if (pm10_12.isDefined && pm10_4.isDefined)
      Some((pm10_12.get + pm10_4.get) / 2)
    else
      None

    val so2_24 = getEpaMTypeAvg(getEpaHourRecord(monitor, MonitorType.withName("A222"), lastHour - 23.hour, lastHour + 1.hour), 16)
    val co_8 = getEpaMTypeAvg(getEpaHourRecord(monitor, MonitorType.withName("A224"), lastHour - 7.hour, lastHour + 1.hour), 6)
    val o3 = getEpaMTypeAvg(getEpaHourRecord(monitor, MonitorType.withName("A225"), lastHour, lastHour + 1.hour), 1)
    val no2 = getEpaMTypeAvg(getEpaHourRecord(monitor, MonitorType.withName("A293"), lastHour, lastHour + 1.hour), 1)
    val result = Map[MonitorType.Value, (Option[Float], Option[Float])](
      MonitorType.withName("A214") -> (pm10, pm10PSI(pm10)),
      MonitorType.withName("A222") -> (so2_24, so2PSI(so2_24)),
      MonitorType.withName("A224") -> (co_8, coPSI(co_8)),
      MonitorType.withName("A225") -> (o3, o3PSI(o3)),
      MonitorType.withName("A293") -> (no2, no2PSI(no2)))
    val sub_psi = result.values.map(_._2)
    val psi = sub_psi.toList.max

    (psi, result)
  }
  case class PsiReport(psi: Option[Float], sub_map: Map[MonitorType.Value, (Option[Float], Option[Float])])

  def getMonitorMonthlyPSI(monitor: Monitor.Value, start: DateTime) = {
    val days = getPeriods(start, start + 1.month, 1.day)
    for (day <- days)
      yield getMonitorDailyPSI(monitor, day)
  }

  def getMonitorDailyPSIfromMap(dayStartHour: Int,
                                map: Map[MonitorType.Value, List[(Option[Float], Option[String])]]) = {
    def getMonitorTypeAvg(mt: MonitorType.Value,
                          start: Int, end: Int, validMin: Int) = {
      val records = map(mt).slice(start, end)
      val validValues = records.filter(statusFilter(MonitorStatusFilter.ValidData)).map(_._1.get)
      val total = validValues.length
      if (total < validMin)
        None
      else {
        val sum = validValues.sum
        Some(sum / total)
      }
    }
    def getMonitorTypeMax(mt: MonitorType.Value,
                          start: Int, end: Int, validMin: Int) = {
      val records = map(mt).slice(start, end)
      val validValues = records.filter(statusFilter(MonitorStatusFilter.ValidData)).map(_._1.get)
      val total = validValues.length
      if (total < validMin)
        None
      else
        Some(validValues.max)
    }

    def getMonitorType8HourAvgMax(mt: MonitorType.Value, start: Int, end: Int) = {
      def get8hrAvg(data: List[(Option[Float], Option[String])]) = {
        val validValues = data.filter(statusFilter(MonitorStatusFilter.ValidData)).map(_._1.get)
        val total = validValues.length
        if (total < 6)
          None
        else
          Some(validValues.max)
      }

      val records = map(mt).slice(start, end)
      val movingAvg =
        for {
          start <- 0 to records.length - 8
          data = records.slice(start, start + 8)
        } yield get8hrAvg(data)

      movingAvg.max
    }

    val pm10_24 = getMonitorTypeAvg(MonitorType.withName("A214"), dayStartHour, dayStartHour + 24, 16)
    val so2_24 = getMonitorTypeAvg(MonitorType.withName("A222"), dayStartHour, dayStartHour + 24, 16)
    val o3 = getMonitorTypeMax(MonitorType.withName("A225"), dayStartHour, dayStartHour + 24, 16)
    val no2 = getMonitorTypeMax(MonitorType.withName("A293"), dayStartHour, dayStartHour + 24, 16)

    val co_8 = getMonitorType8HourAvgMax(MonitorType.withName("A224"), dayStartHour, dayStartHour + 24)

    val result = Map[MonitorType.Value, (Option[Float], Option[Float])](
      MonitorType.withName("A214") -> (pm10_24, pm10PSI(pm10_24)),
      MonitorType.withName("A222") -> (so2_24, so2PSI(so2_24)),
      MonitorType.withName("A224") -> (co_8, coPSI(co_8)),
      MonitorType.withName("A225") -> (o3, o3PSI(o3)),
      MonitorType.withName("A293") -> (no2, no2PSI(no2)))
    val sub_psi = result.values.map(_._2)
    val psi = sub_psi.toList.max

    PsiReport(psi, result)
  }

  def getMonitorDailyPSI(monitor: Monitor.Value, thisDay: DateTime) = {
    val dayReport =
      getDailyReport(monitor, thisDay, MonitorType.psiList)

    val mapPair =
      for {
        mtRecord <- dayReport.typeList
      } yield {
        val mt = mtRecord.monitorType
        val dataList = mtRecord.dataList map (d => (d._2, d._3))
        mt -> dataList
      }

    val dailyMap = mapPair.toMap

    getMonitorDailyPSIfromMap(0, dailyMap)
  }

  def getEpaDailyPSI(monitor: EpaMonitor.Value, current: DateTime)(implicit session: DBSession = AutoSession) = {
    val pm10_24 = getEpaMTypeAvg(getEpaHourRecord(monitor, MonitorType.withName("A214"), current, current + 1.day), 16)
    val so2_24 = getEpaMTypeAvg(getEpaHourRecord(monitor, MonitorType.withName("A222"), current, current + 1.day), 16)
    val o3 = getEpaMTypeMax(getEpaHourRecord(monitor, MonitorType.withName("A225"), current, current + 1.day), 16)
    val no2 = getEpaMTypeMax(getEpaHourRecord(monitor, MonitorType.withName("A293"), current, current + 1.day), 16)

    val co_8 = getEpa8HourAvgMax(getEpaHourRecord(monitor, MonitorType.withName("A224"), current, current + 1.day), current, current + 1.day)
    val result = Map[MonitorType.Value, (Option[Float], Option[Float])](
      MonitorType.withName("A214") -> (pm10_24, pm10PSI(pm10_24)),
      MonitorType.withName("A222") -> (so2_24, so2PSI(so2_24)),
      MonitorType.withName("A224") -> (co_8, coPSI(co_8)),
      MonitorType.withName("A225") -> (o3, o3PSI(o3)),
      MonitorType.withName("A293") -> (no2, no2PSI(no2)))
    val sub_psi = result.values.map(_._2)
    val psi = sub_psi.toList.max

    PsiReport(psi, result)
  }

  def getPsiLevel(v: Float) = {
    if (v <= 50)
      "PSI1"
    else if (v <= 100)
      "PSI2"
    else if (v <= 199)
      "PSI3"
    else if (v <= 299)
      "PSI4"
    else
      "PSI5"
  }

  def getAqiLevel(v: Float) = {
    if (v <= 50)
      "AQI1"
    else if (v <= 100)
      "AQI2"
    else if (v <= 150)
      "AQI3"
    else if (v <= 200)
      "AQI4"
    else if (v <= 300)
      "AQI5"
    else
      "AQI6"
  }

  def getRealtimePSI(lastHour: DateTime, monitorList: List[Monitor.Value] = Monitor.mvList)(implicit session: DBSession = AutoSession) = {
    val result =
      for {
        m <- monitorList
      } yield {
        m -> getMonitorRealtimePSI(m, lastHour)
      }
    Map(result: _*)
  }

  def getDailyPsiReport(m: Monitor.Value, start: DateTime) = {
    import Record._
    val lastDayReport = getDailyReport(m, start - 1.day, MonitorType.psiList)
    val todayReport = getDailyReport(m, start, MonitorType.psiList)
    val mapPair =
      for {
        mtRecords <- lastDayReport.typeList.zipWithIndex
        mtRecord = mtRecords._1
        mtIdx = mtRecords._2
      } yield {
        val mt = mtRecord.monitorType
        val dataList = mtRecord.dataList ++ todayReport.typeList(mtIdx).dataList
        assert(mt == todayReport.typeList(mtIdx).monitorType)
        assert(dataList.length == 48)

        mt -> dataList.map(d => (d._2, d._3))
      }

    val dailyMap = mapPair.toMap

    for {
      hr <- 24 to 48
    } yield {
      getMonitorRealtimePSIfromMap(hr, dailyMap)
    }
  }

  def getRealtimePsiTrend(m: Monitor.Value, start: DateTime, end: DateTime) = {
    import Record._
    val duration = new Duration(start, end)
    val dayReports =
      for (delta <- 0 to duration.getStandardDays.toInt) yield {
        getDailyReport(m, start + delta.day, MonitorType.psiList)
      }
    val lastDayReport = getDailyReport(m, start - 1.day, MonitorType.psiList)

    val totalReport = dayReports.foldLeft(lastDayReport)((d1, d2) =>
      {
        val zipList = d1.typeList.zip(d2.typeList)
        val newTlist = zipList.map { z =>
          MonitorTypeRecord(z._1.monitorType, z._1.dataList ++ z._2.dataList, z._1.stat)

        }
        DailyReport(newTlist)
      })

    val mapPair =
      for {
        mtRecord <- totalReport.typeList
      } yield {
        val mt = mtRecord.monitorType
        val dataList = mtRecord.dataList map (d => (d._2, d._3))
        mt -> dataList
      }

    val dailyMap = mapPair.toMap

    val timePsiPair =
      for {
        hr <- 24 to (24 + duration.getStandardDays.toInt * 24)
      } yield {
        start + (hr - 24).hour -> getMonitorRealtimePSIfromMap(hr, dailyMap)._1
      }
    timePsiPair.filter(_._2.isDefined).map(p => p._1 -> p._2.get).toMap
  }

  def getPm25Level(v: Int) = {
    val ceil_level = List(11, 23, 35, 41, 47, 53, 58, 64, 70).zipWithIndex

    val ceil_levelOpt = ceil_level.find(c_l =>
      v <= c_l._1)

    val levelOpt = ceil_levelOpt.map(_._2 + 1)
    levelOpt.getOrElse(10)
  }

  def getRealtimePm25(lastHour: DateTime)(implicit session: DBSession = AutoSession) = {
    val result =
      for {
        m <- Monitor.mvList
      } yield {
        m -> getMonitorRealtimePm25(m, lastHour).map { pm25 => (pm25, getPm25Level(pm25)) }
      }
    Map(result: _*)
  }

  def getLatestRecordTime(tabType: TableType.Value)(implicit session: DBSession = AutoSession) = {
    val tab_name = Record.getTabName(tabType)
    sql"""
      SELECT TOP 1 M_DateTime
      FROM ${tab_name}
      ORDER BY M_DateTime  DESC
      """.map { r => r.timestamp(1) }.single.apply
  }

  def getLatestMonitorRecordTime(tabType: TableType.Value, m: Monitor.Value)(implicit session: DBSession = AutoSession) = {
    val tab_name = Record.getTabName(tabType)
    sql"""
      SELECT TOP 1 M_DateTime
      FROM ${tab_name}
      WHERE DP_NO = ${m.toString}
      ORDER BY M_DateTime  DESC
      """.map { r => r.timestamp(1) }.single.apply
  }

  def getRealtimeMonitorValueMap(mt: MonitorType.Value, current: Timestamp)(implicit session: DBSession = AutoSession) = {
    val datetime = current.toDateTime
    val tab = Record.getTabName(TableType.Min)
    val records = sql"""
      SELECT *
      FROM ${tab}
      WHERE M_DateTime = ${current}
      """.map { Record.mapper }.list.apply

    val kvs =
      for { r <- records } yield {
        val t = Record.monitorTypeProject2(mt)(r)
        Monitor.withName(r.name) -> t
      }

    Map(kvs: _*)
  }

  def getRealtimeMonitorStatusMap(current: Timestamp)(implicit session: DBSession = AutoSession) = {
    val datetime = current.toDateTime
    val tab = Record.getTabName(TableType.Min)
    val records = sql"""
      SELECT *
      FROM ${tab}
      WHERE M_DateTime = ${current}
      """.map { Record.mapper }.list.apply

    val kvs =
      for { r <- records } yield {
        val monitor = Monitor.withName(r.name)
        val statusPairs =
          for (mt <- Monitor.map(monitor).monitorTypes) yield {
            mt -> Record.monitorTypeProject2(mt)(r)._2
          }

        val statusMap = Map(statusPairs: _*)
        monitor -> statusMap
      }

    Map(kvs: _*)
  }

  def getRealtimeWeatherMap(current: Timestamp)(implicit session: DBSession = AutoSession) = {
    val datetime = current.toDateTime
    val records = sql"""
      SELECT *
      FROM MinRecord
      WHERE M_DateTime = ${current}
      """.map { Record.mapper }.list.apply

    val kvs =
      for { r <- records } yield {
        Monitor.withName(r.name) -> WeatherStat(r.wind_speed, r.wind_dir)
      }

    Map(kvs: _*)
  }
}