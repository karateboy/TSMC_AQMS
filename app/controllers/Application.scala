package controllers

import play.api._
import play.api.mvc._
import play.api.Logger
import models._
import models.Realtime._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.json.Json
import play.api.Play.current
import play.api.data._
import play.api.data.Forms._
import play.api.libs.ws._
import play.api.libs.ws.ning.NingAsyncHttpClientConfigBuilder
import scala.concurrent.Future
import PdfUtility._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import javax.inject._
import play.api.i18n._

case class EditData(id: String, data: String)
case class EpaRealtimeData(
  siteName: String,
  county: String,
  psi: String,
  so2: String,
  co: String,
  o3: String,
  pm10: String,
  pm25: String,
  no2: String,
  windSpeed: String,
  windDir: String,
  publishTime: String)

class Application @Inject() (val messagesApi: MessagesApi) extends Controller with I18nSupport {

  def index = Security.Authenticated {
    implicit request =>
      val userInfoOpt = Security.getUserinfo(request)
      if (userInfoOpt.isEmpty) {
        Forbidden("Invalid access!")
      } else {
        val userInfo = userInfoOpt.get
        val user = User.getUserById(userInfo.id).get
        val group = Group.getGroup(userInfo.groupID).get
        Ok(views.html.index(Messages("system.name"), user, userInfo, group.privilege))
      }
  }

  def monitor(monitor: String) = Security.Authenticated {
    implicit request =>
      val m = Monitor.withName(monitor)
      Ok(views.html.monitor(m))
  }

  case class MonitorInfo(mt: Seq[MonitorType.Value], imgUrl: String, equipments: List[Equipment], location: Seq[Double])
  implicit val equipmentWrite = Json.writes[Equipment]
  implicit val mInfoWrite = Json.writes[MonitorInfo]

  def getMonitorInfo(monitorStr: String) = Security.Authenticated {
    implicit request =>
      val m = Monitor.withName(monitorStr)
      val mCase = Monitor.map(m)
      val info = MonitorInfo(mCase.monitorTypes, mCase.url, Equipment.map.getOrElse(m, List.empty),
        Seq(mCase.lat, mCase.lng))

      Ok(Json.toJson(info))
  }

  def getMonitorBios(monitorStr: String) = Security.Authenticated {
    implicit request =>
      val m = Monitor.withName(monitorStr)
      val bios = MonitorBios.map(m)

      Ok(views.html.monitorBios(m, bios))
  }

  def setMonitorTypes(monitorStr: String) = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      val monitor = Monitor.withName(monitorStr)
      val monitorTypes = request.body.validate[Seq[MonitorType.Value]]

      monitorTypes.fold(
        error => {
          Logger.error(JsError.toJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toJson(error).toString()))
        },
        mt => {
          Monitor.updateMonitorTypes(monitor, mt)
          Ok(Json.obj("ok" -> true))
        })
  }

  def getInternalStd(monitorStr: String, mtStr: String) = Security.Authenticated {
    implicit request =>
      val m = Monitor.withName(monitorStr)
      val mt = MonitorType.withName(mtStr)
      val std = Monitor.map(m).getStdInternal(mt)

      Ok(Json.obj(
        if (std.isDefined)
          "std" -> std.get
        else
          "std" -> "-"))
  }

  def setInternalStd(monitorStr: String, monitorTypeStr: String, stdStr: String) = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      import java.lang.Float
      val m = Monitor.withName(monitorStr)
      val monitorType = MonitorType.withName(monitorTypeStr)
      val oldCase = Monitor.map(m)

      val newStd =
        if (stdStr == "-") {
          oldCase.monitorTypeStds.filter { _.id != monitorType }
        } else {
          val stdValue = Float.parseFloat(stdStr)
          oldCase.getNewStd(monitorType, stdValue)
        }

      val newCase = Monitor(oldCase.id, oldCase.name, oldCase.lat, oldCase.lng, oldCase.url,
        oldCase.autoAudit, oldCase.monitorTypes, newStd, oldCase.instrumentStatusTypeMapOpt)
      Monitor.updateStdInternal(newCase)

      Ok(Json.obj("ok" -> true))
  }

  def setMonitorImgUrl(monitorStr: String) = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      val monitor = Monitor.withName(monitorStr)
      val imgUrlResult = request.body.validate[String]

      imgUrlResult.fold(
        error => {
          Logger.error(JsError.toJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toJson(error).toString()))
        },
        imgUrl => {
          Monitor.updateImgUrl(monitor, imgUrl)
          Ok(Json.obj("ok" -> true))
        })
  }

  def setMonitorLocation(monitorStr: String) = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      val monitor = Monitor.withName(monitorStr)
      val locationResult = request.body.validate[Seq[Double]]

      locationResult.fold(
        error => {
          Logger.error(JsError.toJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toJson(error).toString()))
        },
        location => {
          Monitor.updateLocation(monitor, location(0), location(1))
          Logger.info(s"location set to $location")
          Ok(Json.obj("ok" -> true))
        })
  }

  def monitorTypeConfig = Security.Authenticated {
    implicit request =>
      val autoAuditNormal = SystemConfig.getConfig(SystemConfig.AutoAuditAsNormal, "False").toBoolean
      val applyCalibration = SystemConfig.getApplyCalibration
      Ok(views.html.monitorTypeConfig(autoAuditNormal, applyCalibration))
  }

  def setAutoAuditNormal(booleanStr: String) = Security.Authenticated {
    implicit request =>
      SystemConfig.setConfig(SystemConfig.AutoAuditAsNormal, booleanStr)
      Ok(Json.obj("ok" -> true))
  }

  def setApplyCalibration(booleanStr: String) = Security.Authenticated {
    implicit request =>
      SystemConfig.setApplyCalibration(booleanStr.toBoolean)
      Ok(Json.obj("ok" -> true))
  }

  def saveMonitorTypeConfig() = Security.Authenticated {
    implicit request =>
      try {
        val mtForm = Form(
          mapping(
            "id" -> text,
            "data" -> text)(EditData.apply)(EditData.unapply))

        val mtData = mtForm.bindFromRequest.get
        val mtInfo = mtData.id.split(":")
        val mt = MonitorType.withName(mtInfo(0))

        MonitorType.updateMonitorType(mt, mtInfo(1), mtData.data)

        Ok(mtData.data)
      } catch {
        case ex: Throwable =>
          Logger.error(ex.getMessage, ex)
          BadRequest(ex.getMessage)
      }
  }

  def setInstrumentThreshold() = Security.Authenticated {
    implicit request =>
      try {
        val mtForm = Form(
          mapping(
            "id" -> text,
            "data" -> text)(EditData.apply)(EditData.unapply))

        val threshold = mtForm.bindFromRequest.get

        val v =
          if (threshold.data.length() == 0 || threshold.data == "-")
            None
          else
            Some(threshold.data.toFloat)

        InstrumentThreshold.setValue(threshold.id, v)

        Ok(threshold.data)
      } catch {
        case ex: Throwable =>
          Logger.error(ex.getMessage, ex)
          BadRequest(ex.getMessage)
      }
  }
  def newEquipment = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      val newEquipmentResult = request.body.validate[Equipment]

      newEquipmentResult.fold(
        error => {
          Logger.error(JsError.toJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toJson(error).toString()))
        },
        param => {
          try {
            Equipment.create(param)
          } catch {
            case e: Exception =>
              Logger.error(e.getMessage, e)
              BadRequest(Json.obj("ok" -> false, "msg" -> e.toString()))
          }

          Ok(Json.obj("ok" -> true))
        })
  }

  def updateEquipment() = Security.Authenticated {
    implicit request =>
      try {
        val mtForm = Form(
          mapping(
            "id" -> text,
            "data" -> text)(EditData.apply)(EditData.unapply))

        val mtData = mtForm.bindFromRequest.get
        val ids = mtData.id.split(":")

        Equipment.update(ids, mtData.data)

        Ok(mtData.data)
      } catch {
        case ex: Throwable =>
          Logger.error(ex.getMessage, ex)
          BadRequest(ex.getMessage)
      }
  }

  def deleteEquipment(idStr: String) = Security.Authenticated {
    val ids = idStr.split(":")
    Equipment.delete(Monitor.withName(ids(0)), ids(1))
    Ok(Json.obj("ok" -> true))
  }

  def monitorStatusConfig = Security.Authenticated {
    implicit request =>
      Ok(views.html.monitorStatusConfig())
  }

  def saveMonitorStatusConfig() = Security.Authenticated {
    implicit request =>
      try {
        val mtForm = Form(
          mapping(
            "id" -> text,
            "data" -> text)(EditData.apply)(EditData.unapply))

        val msData = mtForm.bindFromRequest.get

        MonitorStatus.update(msData.id, msData.data)

        Ok(msData.data)
      } catch {
        case ex: Throwable =>
          Logger.error(ex.getMessage, ex)
          BadRequest(ex.getMessage)
      }
  }

  def recordValidation = Security.Authenticated {
    implicit request =>
      Ok(views.html.recordValidation())
  }

  def recordValidationHtml(startStr: String) = Security.Authenticated {
    implicit request =>
      val start = DateTime.parse(startStr)
      val nextDay = start + 1.day
      val end =
        if (nextDay > DateTime.now)
          DateTime.now
        else
          nextDay

      val report = Record.getRecordValidationReport(start, end)

      Ok(views.html.recordValidationReport(start, end, report))
  }

  implicit val epaRealtimeDataRead: Reads[EpaRealtimeData] =
    ((__ \ "SiteName").read[String] and
      (__ \ "County").read[String] and
      (__ \ "PSI").read[String] and
      (__ \ "SO2").read[String] and
      (__ \ "CO").read[String] and
      (__ \ "O3").read[String] and
      (__ \ "PM10").read[String] and
      (__ \ "PM2.5").read[String] and
      (__ \ "NO2").read[String] and
      (__ \ "WindSpeed").read[String] and
      (__ \ "WindDirec").read[String] and
      (__ \ "PublishTime").read[String])(EpaRealtimeData.apply _)

  import play.api.libs.concurrent.Execution.Implicits.defaultContext
  def realtimeEpaRecord = Security.Authenticated.async {
    implicit request =>
      val url = "http://opendata.epa.gov.tw/ws/Data/AQX/?$orderby=SiteName&$skip=0&$top=1000&format=json"
      WS.url(url).get().map {
        response =>
          try {
            val epaData = response.json.validate[Seq[EpaRealtimeData]]
            epaData.fold(
              error => {
                Logger.error(JsError.toJson(error).toString())
                Ok(views.html.epaRealtimeData(url, Seq.empty[EpaRealtimeData]))
              },
              data => {
                Ok(views.html.epaRealtimeData(url, data))
              })

          } catch {
            case ex: Exception =>
              Logger.error(ex.getMessage, ex)
              Ok(views.html.epaRealtimeData(url, Seq.empty[EpaRealtimeData]))
          }
      }
  }

  def userManagement() = Security.Authenticated {
    implicit request =>
      val userInfoOpt = Security.getUserinfo(request)
      if (userInfoOpt.isEmpty)
        Forbidden("No such user!")
      else {
        val userInfo = userInfoOpt.get
        val user = User.getUserById(userInfo.id).get
        val (userList, groupList) =
          if (!user.isAdmin)
            (List[User](), List[Group]())
          else
            (User.getAllUsers, Group.getAllGroups)

        Ok(views.html.userManagement(userInfo, user, userList, groupList))
      }
  }

  import models.User._
  implicit val userParamRead = Json.reads[User]

  def newUser = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      val newUserParam = request.body.validate[User]

      newUserParam.fold(
        error => {
          Logger.error(JsError.toJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toJson(error).toString()))
        },
        param => {
          User.newUser(param)
          Ok(Json.obj("ok" -> true))
        })
  }

  def deleteUser(id: Int) = Security.Authenticated {
    implicit request =>
      val userInfoOpt = Security.getUserinfo(request)
      val userInfo = userInfoOpt.get

      Ticket.transferTickets(id, userInfo.id)
      User.deleteUser(id)
      Ok(Json.obj("ok" -> true))
  }

  def getUser(id: Int) = Security.Authenticated {
    Ok("")
  }

  def updateUser(id: Int) = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      val userParam = request.body.validate[User]

      userParam.fold(
        error => {
          Logger.error(JsError.toJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toJson(error).toString()))
        },
        param => {
          User.updateUser(param)
          Ok(Json.obj("ok" -> true))
        })
  }

  def getAllUsers = Security.Authenticated {
    val users = User.getAllUsers()
    implicit val userWrites = Json.writes[User]

    Ok(Json.toJson(users))
  }

  def groupManagement() = Security.Authenticated {
    implicit request =>
      val userInfoOpt = Security.getUserinfo(request)
      if (userInfoOpt.isEmpty)
        Forbidden("No such user!")
      else {
        val userInfo = userInfoOpt.get
        if (!userInfo.isAdmin) {
          Forbidden("Only administrator can manage group!")
        } else {
          val group = Group.getGroup(userInfo.groupID)
          val groupList = Group.getAllGroups()
          Ok(views.html.groupManagement(userInfo, group.get, groupList))
        }
      }
  }

  import Privilege._
  implicit val groupParamRead = Json.reads[Group]
  def newGroup = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>

      val newGroupParam = request.body.validate[Group]

      newGroupParam.fold(
        error => {
          Logger.error(JsError.toJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toJson(error).toString()))
        },
        param => {
          Group.newGroup(param)
          Ok(Json.obj("ok" -> true))
        })
  }

  def deleteGroup(id: Int) = Security.Authenticated {
    implicit request =>
      if (User.getCountGroupID(id) != 0)
        Ok(Json.obj("ok" -> false, "reason" -> "該群組正被使用"))
      else {
        Group.deleteGroup(id)
        Ok(Json.obj("ok" -> true))
      }
  }

  def updateGroup(id: Int) = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      Logger.info("updateGroup")
      val groupParam = request.body.validate[Group]

      groupParam.fold(
        error => {
          Logger.error(JsError.toJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toJson(error).toString()))
        },
        param => {
          Group.updateGroup(param)
          Ok(Json.obj("ok" -> true))
        })
  }

  def getAllGroups = Security.Authenticated {
    val groups = Group.getAllGroups()
    implicit val groupWrites = Json.writes[Group]

    Ok(Json.toJson(groups))
  }

  def manualAudit = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      Ok(views.html.history("/HistoryQueryReport/true/", group.privilege, true))
  }

  case class ManualAudit(monitor: Monitor.Value, monitorType: MonitorType.Value, time: Long, status: String, reason: Option[String])
  case class ManualAuditList(list: Seq[ManualAudit])
  def manualAuditApply(recordTypeStr: String) = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val user = User.getUserById(userInfo.id).get
      val tabType = TableType.withName(recordTypeStr)
      implicit val manualAuditReads = Json.reads[ManualAudit]
      implicit val manualAuditListReads = Json.reads[ManualAuditList]
      val manualAuditList = request.body.validate[ManualAuditList]
      manualAuditList.fold(
        error => {
          Logger.error(JsError.toJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toJson(error).toString()))
        },
        manualAuditList => {
          for (ma <- manualAuditList.list.reverse) {
            val now = DateTime.now
            val tagInfo = MonitorStatus.getTagInfo(ma.status)
            if (tagInfo.statusType == StatusType.Manual) {
              val log = ManualAuditLog(tabType, ma.monitor, new DateTime(ma.time), ma.monitorType, now, ma.status, user.name, ma.reason)
              try {
                val logOpt = ManualAuditLog.getLog(tabType, ma.monitor, new DateTime(ma.time), ma.monitorType)
                if (logOpt.isEmpty)
                  ManualAuditLog.newLog(log)
                else
                  ManualAuditLog.updateLog(log)
              } catch {
                case ex: Exception =>

              }
            } else if (tagInfo.statusType == StatusType.Internal) {
              ManualAuditLog.deleteLog(tabType, ma.monitor, new DateTime(ma.time), ma.monitorType)
            }

            Record.updateRecordStatus(tabType, ma.monitor, ma.monitorType, ma.time, ma.status)
          }

          Ok(Json.obj("ok" -> true))
        })
  }

  def manualAuditQuery() = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get

      Ok(views.html.manualAuditQuery(group.privilege))
  }

  def manualAuditQueryReport(monitorStr: String, startStr: String, endStr: String, outputTypeStr: String) = Security.Authenticated {
    val monitorStrArray = monitorStr.split(':')
    val monitors = monitorStrArray.map { Monitor.withName }
    val start = DateTime.parse(startStr)
    val end = DateTime.parse(endStr) + 1.day
    val outputType = OutputType.withName(outputTypeStr)

    val records = ManualAuditLog.getLogs(TableType.Hour, monitors, start, end)

    val output = views.html.manualAuditQueryReport(start, end, records)
    val title = "人工註記查詢"
    outputType match {
      case OutputType.html =>
        Ok(output)
      case OutputType.pdf =>
        Ok.sendFile(creatPdfWithReportHeader(title, output),
          fileName = _ =>
            play.utils.UriEncoding.encodePathSegment(title + start.toString("YYMMdd") + "_" + end.toString("MMdd") + ".pdf", "UTF-8"))
    }

  }

  def auditConfig() = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get

      Ok(views.html.auditConfig(group.privilege))
  }

  def getMonitorAuditConfig(monitorStr: String) = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      val m = Monitor.withName(monitorStr)

      val autoAudit = Monitor.map(m).autoAudit

      Ok(Json.toJson(autoAudit))
  }

  def setMonitorAuditConfig(monitorStr: String) = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      val monitor = Monitor.withName(monitorStr)
      val autoAuditResult = request.body.validate[AutoAudit]

      autoAuditResult.fold(
        error => {
          Logger.error(JsError.toJson(error).toString())
          BadRequest(Json.obj("ok" -> false, "msg" -> JsError.toJson(error).toString()))
        },
        autoAudit => {
          Monitor.updateMonitorAutoAudit(monitor, autoAudit)
          Ok(Json.obj("ok" -> true))
        })
  }

  def instrument() = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get

      Ok(views.html.instrument(group.privilege))
  }

  def instrumentReport(monitorStr: String, instrumentId: String, startStr: String, endStr: String, outputTypeStr: String) = Security.Authenticated {
    val monitor = Monitor.withName(monitorStr)
    val instStatusTypeMap = if (Monitor.map(monitor).instrumentStatusTypeMapOpt.isDefined)
      Monitor.map(monitor).instrumentStatusTypeMapOpt
        .get.map(map => map.instrumentId -> map.statusTypeSeq).toMap
    else
      Map.empty[String, Seq[InstrumentStatusType]]

    val statusTypeMap =
      instStatusTypeMap.getOrElse(instrumentId, Seq.empty[InstrumentStatusType]).map { st => st.key -> st }.toMap

    val start = DateTime.parse(startStr)
    val end = DateTime.parse(endStr) + 1.day
    val outputType = OutputType.withName(outputTypeStr)
    val records = InstrumentStatus.query(monitor, instrumentId, start, end)
    val statusMapList = records.map { _.toStatusMap }
    val keySet = statusMapList.map { _.statusMap.keySet }.foldRight(Set.empty[String])((a, b) => a ++ b)
    val output = views.html.InstrumentStatusReport(monitor, instrumentId, (keySet & statusTypeMap.keySet).toList,
      statusTypeMap, statusMapList, start, end)
    val title = "儀器狀態"

    outputType match {
      case OutputType.html =>
        Ok(output)
      case OutputType.pdf =>
        Ok.sendFile(creatPdfWithReportHeader(title, output),
          fileName = _ =>
            play.utils.UriEncoding.encodePathSegment(Monitor.map(monitor).name + title + start.toString("YYYYMMdd") + "_" + end.toString("MMdd") + ".pdf", "UTF-8"))
    }
  }

  val path = current.path.getAbsolutePath + "/importEPA/"

  def importEpa103 = Action {
    Epa103Importer.importData(path)
    Ok(s"匯入 $path")
  }

  def importEpa100 = Action {
    Epa100Importer.importData(path)
    Ok(s"匯入 $path")
  }

  case class MonitorName(id: String, name: String, instrumentList: Seq[String])
  def monitorInstrumentList() = Security.Authenticated {
    implicit request =>
      val userInfoOpt = Security.getUserinfo(request)
      if (userInfoOpt.isEmpty) {
        Forbidden("Invalid access!")
      } else {
        val userInfo = userInfoOpt.get
        val user = User.getUserById(userInfo.id).get
        val group = Group.getGroup(userInfo.groupID).get
        val myList = Monitor.myMvList(group.privilege) map { v =>
          MonitorName(v.toString, Monitor.map(v).name, Monitor.getInstrumentList(v))
        }

        implicit val write = Json.writes[MonitorName]
        Ok(Json.toJson(myList))
      }
  }
  
  def instrumentCmdView = Security.Authenticated {
    Ok(views.html.instrumentCmd())
  }

  def getInstrumentCmdList = Security.Authenticated {
    Ok(Json.toJson(InstrumentCommand.cmdSeq))
  }
  
  def getPendingInstrumentCmd(monitorStr: String) = Action {
    val monitor = Monitor.withName(monitorStr)
    val cmdSeq = InstrumentCommand.takeCommand(monitor)
    Ok(Json.toJson(cmdSeq))
  }

  def postInstrumentCmd(monitorStr: String) = Security.Authenticated(BodyParsers.parse.json) {
    implicit request =>
      val monitor = Monitor.withName(monitorStr)
      val ret = request.body.validate[InstrumentCommand]
      ret.fold(err => {
        Logger.error(JsError.toJson(err).toString())
        BadRequest(JsError.toJson(err).toString())
      },
        cmd => {
          InstrumentCommand.pushCommand(monitor, cmd)
          Ok(Json.obj("ok" -> true))
        })
  }
}
