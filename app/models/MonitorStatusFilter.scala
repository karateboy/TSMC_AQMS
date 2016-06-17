package models

/**
 * @author user
 */
object MonitorStatusFilter extends Enumeration {
  val All = Value("all")
  val Normal = Value("normal")
  val Over = Value("over")
  val Calbrating = Value("calbrating")
  val Calbration_Fail = Value("calbration_failed")
  val InvalidData = Value("invalid")
  val OverInternal = Value("over_internal")
  val DataLost = Value("data_lost")
  val ValidData = Value("valid")

  val map = Map(
    All -> "全部",
    Normal -> "正常量測值",
    Over -> "超過預設高值",
    Calbrating -> "校正中",
    Calbration_Fail -> "校正失敗",
    InvalidData -> "無效數據",
    OverInternal -> "校正偏移(超過內控值)",
    DataLost -> "遺失數據",
    ValidData -> "有效數據")

  val statusMap = Map(
    All -> MonitorStatus.NORMAL_STAT,
    Normal -> MonitorStatus.NORMAL_STAT,
    Over -> MonitorStatus.OVER_STAT,
    Calbrating -> MonitorStatus.CALBRATION_STAT,
    Calbration_Fail -> MonitorStatus.CALBRATION_FAILED,
    InvalidData -> MonitorStatus.INVALID_DATA,
    OverInternal -> MonitorStatus.CALBRATION_DIVERSION_STAT,
    DataLost -> MonitorStatus.DATA_LOSS_STAT,
    ValidData -> MonitorStatus.NORMAL_STAT)
    
  def isMatched(msf: MonitorStatusFilter.Value, stat: String) = {
    msf match {
      case MonitorStatusFilter.All =>
        true

      case MonitorStatusFilter.Normal =>
        MonitorStatus.isNormal(stat)

      case MonitorStatusFilter.Over =>
        MonitorStatus.isOver(stat)

      case MonitorStatusFilter.Calbrating =>
        MonitorStatus.isCalbrating(stat)

      case MonitorStatusFilter.Calbration_Fail =>
        MonitorStatus.isCalbrationFailed(stat)

      case MonitorStatusFilter.InvalidData =>
        MonitorStatus.isInvalidData(stat)

      case MonitorStatusFilter.OverInternal =>
        MonitorStatus.isOverInternal(stat)
        
      case MonitorStatusFilter.DataLost =>
        MonitorStatus.isDataLost(stat)
        
      case MonitorStatusFilter.ValidData =>
        MonitorStatus.isValid(stat)
    }
  }
}