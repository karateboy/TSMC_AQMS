package models

/**
 * @author user
 */
object MonitorStatusFilter extends Enumeration {
  val ValidData = Value("valid")
  val All = Value("all")

  val map = Map(
    All -> "全部",
    ValidData -> "有效數據")

  val statusMap = Map(
    All -> MonitorStatus.NORMAL_STAT,
    ValidData -> MonitorStatus.NORMAL_STAT)
    
  def isMatched(msf: MonitorStatusFilter.Value, stat: String) = {
    msf match {
      case MonitorStatusFilter.All =>
        true
        
      case MonitorStatusFilter.ValidData =>
        MonitorStatus.isValid(stat)
    }
  }
}