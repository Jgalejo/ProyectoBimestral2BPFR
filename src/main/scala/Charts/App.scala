package ec.edu.utpl.computación.pfr.pi
package Charts

package ec.edu.utpl.computación.pfr.pi
package Estadísticas_descriptivas

import com.github.tototoshi.csv.*
import org.nspl.*
import org.nspl.awtrenderer.*
import org.nspl.data.HistogramData

import java.io.File

implicit object CustomFormat extends DefaultCSVFormat {
  override val delimiter: Char = ';'
}

object App {
  @main
  def pintegra() = {
    val pathDataFile: String = "C:\\Users\\agrab\\Documents\\ArchivoPIntegrador/dsAlineacionesXTorneo.csv"
    val reader = CSVReader.open(new File(pathDataFile))
    //val contentFile: List [List[String]] = reader.all()
    val contentFile: List[Map[String, String]] = reader.allWithHeaders()
    reader.close()
    println(contentFile.take(2))
    println(s"Fila:${contentFile.length} y Columnas: ${contentFile(1).keys.size}")



    charting(contentFile)

    def charting(data: List[Map[String, String]]): Unit = {
      val listNroShirt: List[Double] = data
        .filter(row => row("squads_position_name") == "forward" && row("squads_shirt_number") != "0")
        .map(row => row("squads_shirt_number").toDouble)


      val histForwardShirtNumber = xyplot(HistogramData(listNroShirt, 10) -> bar())(
        par
          .xlab("Shirt number")
          .ylab("freq.")
          .main("Forward shirt number")
      )

      pngToFile(new File("C:\\Users\\agrab\\Documents\\ArchivoPIntegrador\\grafico.png"), histForwardShirtNumber.build, 1000)
    }

  }
}



/*
import com.github.tototoshi.csv._
import org.nspl._
import org.nspl.awtrenderer._
import java.io.File

//Crear un objeto implicito
implicit object CustomFormat extends DefaultCSVFormat {
  override val delimiter: Char = ';'
}

object App {
  @main //Transforma en metodo ejecutable
  def pintegra() = {
    // -----------------------------------------------------------------------------------------------------------------
    val pathDataFile = "C://Users//agrab//Documents//ArchivoPIntegrador//dsPartidosYGoles.csv"
    //Crear el reader
    val reader = CSVReader.open(new File(pathDataFile))
    val contentFile: List[Map[String, String]] = reader.allWithHeaders()
    reader.close()

    def datosgrafica(data: List[Map[String, String]]): List[(String, Double)] =
      data
        .map(row => (
          row("tournaments_tournament_name"),
          row("matches_match_id"),
          row("matches_home_team_score"),
          row("matches_away_team_score")
        ))
        .distinct
        .map(t4 => (t4._1, t4._3.toDouble + t4._4.toDouble))
        .groupBy(_._1)
        .map(t2 => (t2._1, t2._2.map(_._2).sum))
        .toList
        .sortBy(_._1)

    val goles: List[(String, Double)] = datosgrafica(contentFile)

    def charting(): Unit = {
      val data = goles.map { case (str, dbl) => (str, Seq(dbl)) }
      val ds = org.nspl.data.Static.fromDataAndNames(data, Seq("Goles"))
      val graficagoles = xyplot(ds -> bar())(
        par
          .ylab("freq.")
          .xlab("Mundiales")
          .main("Goles por Mundial")
      )
      pngToFile(new File("C://Users//agrab//Documents//ArchivoPIntegrador//graficaGoles.png"), graficagoles.build, 1000)
    }


    charting()
  }
}


 */





