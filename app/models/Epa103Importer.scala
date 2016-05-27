package models
import play.api._
import akka.actor._
import com.github.nscala_time.time.Imports._
import play.api.Play.current
import play.api.libs.concurrent.Akka
import ModelHelper._
import play.api.libs.ws._
import play.api.libs.ws.ning.NingAsyncHttpClientConfigBuilder
import scala.concurrent.ExecutionContext.Implicits.global
import scalikejdbc._
import org.apache.poi.openxml4j.opc._
import org.apache.poi.xssf.usermodel._
import org.apache.poi.ss.usermodel._

object Epa103Importer {
  def importData(path: String) = {
    val worker = Akka.system.actorOf(Props[Epa103Importer], name = "epaImporter" + (Math.random() * 1000).toInt)
    worker ! ImportEpa(path)
  }

  import java.io.File
  import java.io.FileFilter
  def listAllFiles(dir: String) = {
    new java.io.File(dir).listFiles.filter(_.getName.endsWith(".xls"))
  }
}

case class ImportEpa(path: String)

class Epa103Importer extends Actor {

  def receive = {
    case ImportEpa(path) =>
      val files = Epa103Importer.listAllFiles(path)
      for (f <- files) {
        importEpaData(f)
        f.delete()
      }

      Logger.info("Finish import 103")
      self ! PoisonPill
  }

  import java.io.File
  import java.io.FileInputStream
  def importEpaData(f: File) {
    Logger.debug(s"Import ${f.getAbsolutePath}")
    import scala.collection.mutable.ListBuffer
    val wb = WorkbookFactory.create(new FileInputStream(f));
    val sheet = wb.getSheetAt(0)

    var rowN = 1
    var finish = false
    val seqData = ListBuffer.empty[Seq[Any]]
    do {
      var row = sheet.getRow(rowN)
      if (row == null)
        finish = true
      else {
        try {
          val dateStr = row.getCell(0).getStringCellValue
          val site = row.getCell(1).getStringCellValue
          val siteId =
            if (site == "台西")
              EpaMonitor.map(EpaMonitor.withName("臺西")).id
            else
              EpaMonitor.map(EpaMonitor.withName(site)).id

          val mt = row.getCell(2).getStringCellValue
          val itemId = EpaMonitorType.itemIdMap(mt)
          val values =
            for (col <- 3 to 3 + 23) yield {
              try {
                val v = row.getCell(col).getNumericCellValue
                Some(v.toFloat)
              } catch {
                case _: Throwable =>
                  {
                    try {
                      val valStr = row.getCell(col).getStringCellValue
                      if (valStr.isEmpty())
                        None
                      else if (valStr.equalsIgnoreCase("NR"))
                        Some(0f)
                      else {
                        try {
                          Some(valStr.toFloat)
                        } catch {
                          case _: Throwable =>
                            None
                        }
                      }
                    } catch {
                      case _: Throwable =>
                        None
                    }
                  }
              }
            }

          def appendHr(value: Option[Float], offset: Int) = {
            val ts: java.sql.Timestamp = DateTime.parse(dateStr, DateTimeFormat.forPattern("YYYY/MM/dd")) + offset.hour
            seqData.append(Seq(siteId, ts, itemId, value))
          }

          for (v <- values.zipWithIndex if v._1.isDefined)
            appendHr(v._1, v._2)

        } catch {
          case ex: Throwable =>
            Logger.error(ex.getMessage)
            Logger.error("Ignore this row")
        }

        rowN += 1
      }
    } while (!finish)

    wb.close()

    DB autoCommit { implicit session =>
      sql"""
        INSERT INTO [dbo].[hour_data]
        ([MStation]
           ,[MDate]
           ,[MItem]
           ,[MValue])
        values (
        ?,?,?,?)
        """
        .batch(seqData.toList: _*)
        .apply()
    }
  }
}