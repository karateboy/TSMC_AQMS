package models
import play.api._
import akka.actor._
import com.github.nscala_time.time.Imports._
import play.api.Play.current
import models.Realtime._
import ModelHelper._

object Exporter{
  case object Export  
  
  def secondTo8Am = {
    val today8am = DateTime.now.withMillisOfDay(0).withHourOfDay(8)
    
    if(DateTime.now() < today8am){
      val duration = new Duration(DateTime.now(), today8am)
      duration.getStandardSeconds
    }else{
      val tomorrow8am = DateTime.tomorrow.withMillisOfDay(0).withHourOfDay(8)
      val duration = new Duration(DateTime.now(), tomorrow8am)
      duration.getStandardSeconds
    }      
  }
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