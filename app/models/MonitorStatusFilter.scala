package models

/**
 * @author user
 */
object MonitorStatusFilter extends Enumeration {
  val All = Value("all")
  val Normal = Value("normal")
  val ValidData = Value("valid")

  val map = Map(
    All -> "全部",
    Normal -> "正常量測值",
    ValidData -> "有效數據")

  val statusMap = Map(
    All -> MonitorStatus.NORMAL_STAT,
    Normal -> MonitorStatus.NORMAL_STAT,
    ValidData -> MonitorStatus.NORMAL_STAT)
    
  def isMatched(msf: MonitorStatusFilter.Value, stat: String) = {
    msf match {
      case MonitorStatusFilter.All =>
        true

      case MonitorStatusFilter.Normal =>
        MonitorStatus.isNormal(stat)
        
      case MonitorStatusFilter.ValidData =>
        MonitorStatus.isValid(stat)
    }
  }
}