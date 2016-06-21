package models
import scalikejdbc._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import play.api._
import play.api.libs.json._

case class Status(key: String, value: Double)
case class InstrumentStatusMap(time: DateTime, statusMap: Map[String, Double])
case class InstrumentStatusJSON(time: Long, instID: String, statusList: List[Status]){
  def toInstrumentStatus=InstrumentStatus(new DateTime(time), instID, statusList)
  def toStatusMap={
    val map = statusList.map { s => s.key->s.value }.toMap
    InstrumentStatusMap(new DateTime(time), map)
  }
}
case class InstrumentStatus(time: DateTime, instID: String, statusList: List[Status]) {
  def excludeNaN = {
    val validList = statusList.filter { s => !(s.value.isNaN() || s.value.isInfinite() || s.value.isNegInfinity) }
    InstrumentStatus(time, instID, validList)
  }
  def toJSON = {
    val validList = statusList.filter { s => !(s.value.isNaN() || s.value.isInfinite() || s.value.isNegInfinity) }
    InstrumentStatusJSON(time.getMillis, instID, validList)
  }
}

object InstrumentStatus {
  implicit val stRead = Json.reads[Status]
  implicit val isRead = Json.reads[InstrumentStatus]
  implicit val stWrite = Json.writes[Status]
  implicit val isWrite = Json.writes[InstrumentStatus]
  implicit val jsonWrite = Json.writes[InstrumentStatusJSON]
  implicit val jsonRead = Json.reads[InstrumentStatusJSON]

  def query(monitor: Monitor.Value, instrumentID: String, start: DateTime, end: DateTime)(implicit session: DBSession = AutoSession) = {
    import java.sql.Timestamp
    val startT:Timestamp = start
    val endT:Timestamp = end
    DB readOnly {
      implicit session =>
        sql"""
        SELECT [instrumentID],[time],[json]
        FROM InstrumentStatus
        WHERE monitor = ${monitor.toString} and instrumentID = ${instrumentID} and time >= $startT and time < $endT
        """.map { rs =>
          val json = rs.string(3)
          Json.parse(json).validate[InstrumentStatusJSON].get }.list.apply
    }
  }
  
  def getLatestTime(m: Monitor.Value) = {
    DB readOnly { implicit session =>
      sql"""
      SELECT TOP 1 time
      FROM InstrumentStatus
      WHERE monitor = ${m.toString}
      ORDER BY time  DESC
      """.map { r => r.timestamp(1) }.single.apply
    }
  }

  def insert(monitor:Monitor.Value, jsonSeq: Seq[InstrumentStatusJSON])(implicit session: DBSession = AutoSession)={
    import java.sql.Timestamp
    val monitorName = monitor.toString
    val batchList = jsonSeq.map{is=>
      val jsonStr = Json.toJson(is).toString
      Seq('monitor->monitorName, 'instrumentID->is.instID, 'time->(new Timestamp(is.time)), 'json->jsonStr)
    }
    
    DB localTx{
      implicit session=>
        sql"""
          INSERT INTO [dbo].[InstrumentStatus]
           ([monitor]
           ,[instrumentID]
           ,[time]
           ,[json])
     VALUES
           ({monitor}
           ,{instrumentID}
           ,{time}
           ,{json})
          """.batchByName(batchList: _*)
        .apply()
    }
  }
  
  def format(v:Option[Double])={
    if(v.isDefined)
      s"%.2f".format(v.get)
    else
      "-"
  }
}