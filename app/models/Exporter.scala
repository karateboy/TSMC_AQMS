package models
import play.api._
import akka.actor._
import com.github.nscala_time.time.Imports._
import play.api.Play.current
import models.Realtime._
import ModelHelper._

object Exporter{
  case object Export  
}

class Exporter extends Actor{
  import Exporter._
  import Calibration._
  import controllers.DataLogger
  def receive = {
    case Export=>
      val yesterday = DateTime.lastDay().withMillisOfDay(0)
      for(m <- Monitor.mvList){
        exportDailyCalibrationCSV(m, yesterday)
      }
  }
}