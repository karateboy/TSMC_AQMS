package models
import java.sql.Date
import java.sql.Timestamp
import scalikejdbc._
import play.api._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import models._
import play.api.i18n._

case class Alarm2(monitor: Monitor.Value, time: DateTime,
                  src: String, level: Int, info: String, ticketNo: Option[Long] = None, id:Long = 0)

object Alarm2 {
  object Level {
    val INFO = 1
    val WARN = 2
    val ERR = 3
    val map = Map(INFO -> "資訊", WARN -> "警告", ERR -> "嚴重")
  }
  val alarmLevelList = Level.INFO to Level.ERR

  def Src(mt: MonitorType.Value) = s"T:$mt"
  def Src() = "S:System"

  def getSrcForDisplay(src: String)(implicit messages:Messages) = {
    val part = src.split(':')
    if (part.length >= 2) {
      val srcType = part(0) match {
        case "S" =>
          "系統"
        case "I" =>
          "設備:" + part(1)
        case "T" =>
          try {
            "測項:" + MonitorType.map(MonitorType.withName(part(1))).desp
          } catch {
            case ex: Throwable =>
              "測項:" + part(1)
          }
      }
      srcType
    }else{
      Logger.warn(s"Unexpected alarm src $src")
      src
    }
  }

  def mapper(rs: WrappedResultSet)={
    Alarm2(monitor=Monitor.withName(rs.string("monitor")), 
        time = rs.timestamp("time"), 
        src = rs.string("src"), 
        level = rs.int("important"), 
        info = rs.string("info"), 
        ticketNo = rs.longOpt("ticketNo"), 
        id = rs.long("id"))
  }
  
  def getAlarm(monitors: Seq[Monitor.Value], start: DateTime, end: DateTime)(implicit session: DBSession = AutoSession) = {
    val mStr = SQLSyntax.createUnsafely(monitors.mkString("('", "','", "')"))
    val startT: Timestamp = start
    val endT: Timestamp = end
    assert(start <= end)

    sql"""
        Select *
        From alarm
        Where monitor in ${mStr} and time>=${startT} and time<${end}
        ORDER BY time ASC
        """.map {mapper}.list.apply
  }

  def getMaxId(implicit session: DBSession = AutoSession) = {
    sql"""
      SELECT max(id)
      FROM Alarm
      """.map { rs => rs.longOpt(1)}.single.apply.get
  }
  
  def getAlarmAfterId(id:Long)(implicit session: DBSession = AutoSession) = {
    sql"""
        Select *
        From alarm
        Where id > $id
        ORDER BY time ASC
        """.map {mapper}.list.apply    
  }
  
  def getAlarmByLevel(monitors: Seq[Monitor.Value], level: Int, start: DateTime, end: DateTime)(implicit session: DBSession = AutoSession) = {
    val mStr = SQLSyntax.createUnsafely(monitors.mkString("('", "','", "')"))
    val startT: Timestamp = start
    val endT: Timestamp = end

    sql"""
        Select *
        From alarm
        Where monitor in ${mStr} and time>=${startT} and time<${end} and important >= $level
        ORDER BY time ASC
        """.map {mapper}.list.apply
  }

  def getAlarmBySrcFilter(monitors: Seq[Monitor.Value], srcFilter: Seq[String], start: DateTime, end: DateTime)(implicit session: DBSession = AutoSession) = {
    val mStr = SQLSyntax.createUnsafely(monitors.mkString("('", "','", "')"))
    val startT: Timestamp = start
    val endT: Timestamp = end
    assert(start <= end)
    val sfilter = SQLSyntax.createUnsafely(srcFilter.mkString("('", "','", "')"))
    sql"""
        Select *
        From alarm
        Where monitor in ${mStr} and time>=${startT} and time<${end} and src in ${sfilter}
        ORDER BY time ASC
        """.map {mapper}.list.apply
  }

  def getAlarmByLikeFilter(monitors: Seq[Monitor.Value], srcFilter: String, start: DateTime, end: DateTime)(implicit session: DBSession = AutoSession) = {
    val mStr = SQLSyntax.createUnsafely(monitors.mkString("('", "','", "')"))
    val startT: Timestamp = start
    val endT: Timestamp = end
    assert(start <= end)

    sql"""
        Select *
        From alarm
        Where monitor in ${mStr} and time>=${startT} and time<${end} and src like ${srcFilter}
        ORDER BY time ASC
        """.map {mapper}.list.apply
  }

  def insertAlarm(ar: Alarm2) = {
    def checkExist() = {
      import java.sql.Timestamp
      val time: Timestamp = ar.time

      DB readOnly { implicit session =>
        sql"""
          Select *
          From alarm
          Where monitor = ${ar.monitor.toString} and time = ${time} and src = ${ar.src}          
          """.map(mapper).single.apply
      }
    }
    val arOpt = checkExist()
    if (arOpt.isDefined)
      0
    else {
      DB localTx { implicit session =>
        sql"""
    INSERT INTO [dbo].[Alarm]
           ([monitor]
           ,[time]
           ,[src]
           ,[important]
           ,[info]
           ,[ticketNo])
     VALUES
           (${ar.monitor.toString}
           ,${ar.time: Timestamp}
           ,${ar.src}
           ,${ar.level}
           ,${ar.info}
           ,${ar.ticketNo}
           )
      """.update.apply
      }
    }
  }

  def getLatestAlarmTime(m: Monitor.Value) = {
    DB readOnly { implicit session =>
      sql"""
      SELECT TOP 1 time
      FROM Alarm
      WHERE monitor = ${m.toString}
      ORDER BY time  DESC
      """.map { r => r.timestamp(1) }.single.apply
    }
  }

  def insertAlarmSeq(arList: Seq[Alarm2]) {
    val batchList = arList.map { a =>
      Seq('monitor -> a.monitor.toString,
        'time -> (a.time: Timestamp), 'src -> a.src, 'level -> a.level, 'info -> a.info)
    }

    DB localTx { implicit session =>
      sql"""
    INSERT INTO [dbo].[Alarm]
           ([monitor]
           ,[time]
           ,[src]
           ,[important]
           ,[info]
           ,[ticketNo])
     VALUES
           ({monitor}
           ,{time}
           ,{src}
           ,{level}
           ,{info}
           , NULL
           )
        
        """
        .batchByName(batchList: _*)
        .apply()
    }
  }
}