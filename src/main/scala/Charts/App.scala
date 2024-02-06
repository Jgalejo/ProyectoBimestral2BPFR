package ec.edu.utpl.computación.pfr.pi
package Charts

import org.nspl.data._
import com.github.tototoshi.csv.*
import org.nspl._
import org.nspl.awtrenderer._
import org.nspl.dataSource
import org.nspl._
import org.nspl.data.HistogramData
import java.io.File
import org.nspl.data._
import org.nspl.DataRenderer
import org.nspl.bar
import java.time.LocalDate
import java.time.format.DateTimeFormatter

implicit object CustomFormat extends DefaultCSVFormat {
  override val delimiter: Char = ';'
}

object App {
  @main
  def pintegra() = {
    
    val pathDataFile: String = "C:\\Users\\agrab\\Documents\\ArchivoPIntegrador/dsAlineacionesXTorneo.csv"
    val pathDataFile2: String = "C:\\Users\\agrab\\Documents\\ArchivoPIntegrador/dsPartidosYGoles.csv"
    val reader1 = CSVReader.open(new File(pathDataFile))
    val reader2 = CSVReader.open(new File(pathDataFile2))
    val contentFile: List[Map[String, String]] = reader1.allWithHeaders()
    val contentFile2: List[Map[String, String]] = reader2.allWithHeaders()
    reader1.close()
    reader2.close()

    
    charting(contentFile)

    def charting(data: List[Map[String, String]]): Unit = {
      val dateFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd")
      val birthYears = contentFile.filter(row => row("players_birth_date") != "" && row("players_birth_date") != "not available").map(row => LocalDate.parse(row("players_birth_date"), dateFormat).getYear.toDouble)
      val histogramData = xyplot(HistogramData(birthYears, 10) -> bar())(
        par
          .xlab("Años")
          .ylab("Frq")
          .main("Años de nacimiento mas cómunes en los mundiales")
      )
      pngToFile(new File("C:\\Users\\agrab\\Documents\\ArchivoPIntegrador\\histograma1.png"), histogramData.build, 1000)
    }

    charting2(contentFile)

    def charting2(data: List[Map[String, String]]) =
      val dorsalesjugador = data
        .filter(row => row("players_defender") == "1" && row("squads_shirt_number") != "0")
        .map(row => row("squads_shirt_number").toDouble)
        .take(50)



      val plotBx = boxplot(dorsalesjugador)(par)

      pngToFile(new File("C:\\Users\\agrab\\Documents\\ArchivoPIntegrador\\grafico2.png"), plotBx.build, 1000)

        // Calcula la frecuencia de los goles por minuto
        val goalsFrequency = contentFile2
          .filter(_("goals_minute_regulation") != "NA")
          .filter(_("matches_tournament_id") == "WC-2022")
          .map(row => row("goals_minute_regulation").toDouble)
          .groupBy(identity)
          .view.mapValues(_.size)
          .toSeq


      val plot8 = xyplot(
        goalsfrecuency -> bar(horizontal = false,
          width = 0.5,
          fill = Color.gray2)
      )(
        par
          .xlab("x axis label")
          .ylab("y axis label")
          .xlim(Some(1d -> 5))
      )
      show(plot8)









      // Genera el diagrama de barras
     //)
  }


}


/*
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
*/





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


/*
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
   */
/*
    def charting1(data: List[Map[String, String]]): Unit = {
      val dorsalesgoleadores = data
        .map(row => (
          row("squads_player_id"),
          row("squads_shirt_number"),
          row("goals_player_id").toInt
        ))
        .groupBy(_._2) // Agrupar por número de dorsal
        .mapValues(_.map(_._3).sum) // Sumar los goles de cada dorsal
        .toList
        .sortWith(_._2 > _._2) // Ordenar por número de goles
      println(dorsalesgoleadores)


    }
    charting1(contentFile2)

*/




