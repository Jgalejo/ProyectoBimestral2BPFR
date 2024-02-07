package ec.edu.utpl.computación.pfr.pi
package Charts

import org.nspl._
import org.nspl.data._
import org.nspl.saddle._
import org.saddle._
import com.github.tototoshi.csv.*
import org.nspl.awtrenderer._
import org.nspl.dataSource
import org.nspl.data.HistogramData
import java.io.File
import org.nspl.DataRenderer
import org.nspl.bar
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import org.nspl.LegendConfig
import org.nspl.saddle.rasterplotFromFrame

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
          .main("Años de nacimiento de futbolistas mas cómunes en los mundiales")
      )
      pngToFile(new File("C:\\Users\\agrab\\Documents\\ArchivoPIntegrador\\Graficas\\Grafica1.png"), histogramData.build, 1000)
    }

    
    charting2(contentFile2)

    def charting2(data: List[Map[String, String]]): Unit = {
      val goalsFrequency: List[(Double, Double)] = data
        .filter(_("goals_minute_regulation") != "NA")
        .filter(_("matches_tournament_id") == "WC-2022")
        .map(_("goals_minute_regulation").toDouble)
        .groupBy(identity)
        .view // Utilizar una vista para mejorar la eficiencia en el cálculo de valores
        .mapValues(_.size.toDouble)
        .toList
        .sortBy(-_._2) // Ordenar en orden descendente por frecuencia
        .take(5)
        .map { case (minuto, frecuencia) => (minuto, frecuencia) } // Intercambiar minuto y frecuencia


      val plot8 = xyplot(
        goalsFrequency -> line())(
        par
          .xlab("Minutos")
          .ylab("Goles")
          .main("Linea de Goles en el Mundial Qatar 2022")
      )
      pngToFile(new File("C:\\Users\\agrab\\Documents\\ArchivoPIntegrador\\Graficas\\Grafica3.png"), plot8.build, 1000)
    }

    charting3(contentFile)

    def charting3(data: List[Map[String, String]]): Unit = {
      val dorslaesgoat: List[(Double, Double)] = data
        .filter(row => (row("players_given_name") == "Lionel" && row("players_family_name") == "Messi") ||
          (row("players_given_name") == "Cristiano" && row("players_family_name") == "Ronaldo"))
        .map(row => (row("squads_shirt_number").toDouble, row("squads_player_id")))
        .groupBy(identity)
        .view // Utilizar una vista para mejorar la eficiencia en el cálculo de valores
        .mapValues(_.size.toDouble)
        .toList
        .sortBy(-_._2) // Ordenar en orden descendente por frecuencia
        .map { case (dorsal, frecuencia) => (dorsal._1, frecuencia) }


      val plot8 = xyplot(
        dorslaesgoat -> bar(horizontal = false,
          width = 0.4,
          fill = Color.blue)
      )(
        par
          .xlab("Dorsales")
          .ylab("Frecuencias")
          .main("Dorsales que utilizaron Cristiano Ronaldo y Lionel Messi ")
        //.xlim(Some(7d -> 19d))
      )
      pngToFile(new File("C:\\Users\\agrab\\Documents\\ArchivoPIntegrador\\Graficas\\Grafica4.png"), plot8.build, 1000)
    }

    charting4(contentFile2)

    def charting4(data: List[Map[String, String]]): Unit = {
      val golexMundial = data
        .map(row => (
          row("matches_tournament_id").stripPrefix("WC-").toDouble,
          row("matches_match_id"),
          row("matches_home_team_score"),
          row("matches_away_team_score")
        ))
        .distinct
        .map(t4 => (t4._1, t4._3.toDouble + t4._4.toDouble))
        .groupBy(_._1)
        .map(t2 => (t2._1, t2._2.map(_._2).sum))
        .toList
        .sortBy(-_._2)


      val plot5 = xyplot(golexMundial -> line())(
        par
          .xlab("Años de los Mundial")
          .ylab("Goles"))

      pngToFile(new File("C:\\Users\\agrab\\Documents\\ArchivoPIntegrador\\Graficas\\Grafica5.png"), plot5.build, 1000)


    }

    charting5(contentFile2)

    def charting5(data: List[Map[String, String]]): Unit = {
      val penalgolArg: List[(Double, Double)] = data
        .filter(_("goals_penalty") != "NA")
        .filter(row => (
          row("home_team_name") == "Argentina" || row("away_team_name") == "Argentina"))
        .map(row => (
          row("tournaments_year").toDouble,
          row("goals_penalty").toDouble
        ))
        .groupBy(_._1)
        .view.mapValues(_.map(_._2).sum)
        .toList
        .sortBy(_._1)
        .reverse
        .take(5)


      val plot8 = xyplot(
        penalgolArg -> bar(horizontal = false,
          width = 0.5,
          fill = Color.green)
      )(
        par
          .xlab("Año")
          .ylab("Goles Penal")

          .main("Goles de Penal de Argentina en los ultimos 5 mundiales "))

      pngToFile(new File("C:\\Users\\agrab\\Documents\\ArchivoPIntegrador\\Graficas\\Grafico6.png"), plot8.build, 1000)


      charting6(contentFile2)

      def charting6(data: List[Map[String, String]]): Unit = {
        val conteoequipostorneo = data
          .map(row => (row("tournaments_year").toDouble, row("tournaments_count_teams").toDouble))
          .distinct
          .groupBy(_._2)
          .view.mapValues(_.head)
          .values
          .toList
          .sortBy(_._2)

        val plot8 = xyplot(
          conteoequipostorneo -> bar(horizontal = false,
            width = 0.5,
            fill = Color.red)
        )(
          par
            .xlab("Año")
            .ylab("Participaciones de Equipo")

            .main("Conteo de   equipos participantes en los Mundiales"))

        pngToFile(new File("C:\\Users\\agrab\\Documents\\ArchivoPIntegrador\\Graficas\\Grafico7.png"), plot8.build, 1000)


        charting7(contentFile)

        def charting7(data: List[Map[String, String]]): Unit = {
          val dorsalesjugador = data
            .filter(row => row("players_defender") == "1" && row("squads_shirt_number") != "0")
            .map(row => row("squads_shirt_number").toDouble)

          val histogramData2 = xyplot(HistogramData(dorsalesjugador, 10) -> bar())(
            par
              .xlab("Dorsal Defensa")
              .ylab("Frq")
              .main("Dorsales mas usados por los defensores")
          )


          pngToFile(new File("C:\\Users\\agrab\\Documents\\ArchivoPIntegrador\\Graficas\\Grafica8.png"), histogramData2.build, 1000)
        }
      }
    }
  }
}

