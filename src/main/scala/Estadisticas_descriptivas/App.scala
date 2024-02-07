package ec.edu.utpl.computación.pfr.pi
package Estadisticas_descriptivas

import com.github.tototoshi.csv.*
import java.io.File
import java.time.LocalDate
import java.time.format.DateTimeFormatter

implicit object CustomFormat extends DefaultCSVFormat {
  override val delimiter: Char = ';'
}

object App {
  @main
  def pintegra() = {
    val pathDataFile: String = "C:\\Users\\agrab\\Documents\\ArchivoPIntegrador/dsPartidosyGoles.csv"

    val pathDataFile2: String = "C:\\Users\\agrab\\Documents\\ArchivoPIntegrador/dsAlineacionesXTorneo.csv"

    val reader = CSVReader.open(new File(pathDataFile))

    val reader2 = CSVReader.open(new File(pathDataFile2))

    val contentFile: List[Map[String, String]] = reader.allWithHeaders()

    val contentFile2: List[Map[String, String]] = reader2.allWithHeaders()

    reader.close()

    reader2.close()

    println(s"\n(dsPartidosyGoles) Fila:${contentFile.length} y Columnas: ${contentFile(1).keys.size} ")

    println(s"\n(dsAlineacionesXTorneo) Fila:${contentFile.length} y Columnas: ${contentFile2(1).keys.size} ")

    def maximaCapacidadEstadio(data: List[Map[String, String]]): (Int, String) =
      data
        .map(t2 => (t2("stadiums_stadium_capacity").toInt, t2("stadiums_stadium_name")))
        .max

    println("")
    println(s"El estadio con mayor espectadores: ${maximaCapacidadEstadio(contentFile)}")


    def minimaCapacidadEstadio(data: List[Map[String, String]]): Int =
      data
        .map(_("stadiums_stadium_capacity").toInt)
        .min

    println("")
    println(s"Menor cantidad de espectadores en un estadio ${minimaCapacidadEstadio(contentFile)}")


    def promedioCapacidadEstadio(data: List[Map[String, String]]): Double =
      val promedio = data.map(_("stadiums_stadium_capacity").toInt).distinct
      promedio.sum / promedio.length

    println("")
    println(s"El promedio de espectadores en los mundiales es: ${promedioCapacidadEstadio(contentFile)}")
    println("")

    def maximoGolminutoQatar(data: List[Map[String, String]]): Int =
      data
        .filter(row => (row("matches_tournament_id") == "WC-2022" && row("goals_minute_regulation") != "NA"))
        .map(_("goals_minute_regulation").toInt)
        .max

    println(s"Gol mas tardío en Qatar 2022: ${maximoGolminutoQatar(contentFile)}")
    println("")

    def minimoGolminutoQatar(data: List[Map[String, String]]): Int =
      data
        .filter(row => (row("matches_tournament_id") == "WC-2022" && row("goals_minute_regulation") != "NA"))
        .map(_("goals_minute_regulation").toInt)
        .min

    println(s"Gol mas temprano en Qatar 2022: ${minimoGolminutoQatar(contentFile)}")
    println("")

    def promedioGolminutoQatar(data: List[Map[String, String]]): Int =
      val promedio = data.filter(row => row("goals_minute_regulation") != "NA" && row("matches_tournament_id") == "WC-2022").map(_("goals_minute_regulation").toInt).distinct
      promedio.sum / promedio.length

    println(s"Promedio de minuto gol: ${promedioGolminutoQatar(contentFile)}")
    println("")


    def frecuenciajugadores(data: List[Map[String, String]]) = {
      val jugadores = data
        .distinct
        .filterNot(row => row("players_given_name") == "not applicable" || row("players_female") == "1")
        .map(t4 => ((t4("players_given_name"), t4("players_family_name"), t4("squads_player_id")), t4("squads_tournament_id").stripPrefix("WC-")))

      val frecuencia = jugadores
        .groupBy(_._1)
        .view.mapValues(años => (años.size, años.map(_._2).distinct.sorted))
        .toList
        .sortBy(_._2._1)
        .reverse
        .take(10)

      frecuencia
    }

    println("Jugadores con más participaciones en los Mundiales")
    frecuenciajugadores(contentFile2).foreach { case ((nombre, apellido, _), (frecuencia, años)) =>
      println(s"$nombre $apellido, $frecuencia(${años}")
    }

    //El dorsal mas comun en los defensores en los mundiales
    def dorsalmoda(data: List[Map[String, String]]) = {
      val dorsalesjugador = data
        .distinct
        .filter(row => row("players_defender") == "1")
        .map(_("squads_shirt_number"))
        .groupBy(identity)
        .view.mapValues(_.size)
        .toList
        .maxBy(_._2)

      dorsalesjugador

    }

    println("")
    println(s"El dorsal mas común en los defensores es: ${dorsalmoda(contentFile2)}")


    def youngestPlayerPerWorldCup() = {
      val dateFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd")
      val playersWithBirthDate = contentFile2.filter(row => row("players_birth_date") != "" && row("players_birth_date") != "not available")
        .map(row => (
          row("squads_player_id"),
          row("players_family_name"),
          row("players_given_name"),
          LocalDate.parse(row("players_birth_date"), dateFormat).getYear))
      val groupedByWorldCup = playersWithBirthDate.groupBy(_._1)
      val youngestPlayers = groupedByWorldCup.view.mapValues(players => players.minBy(_._4)).toList
      val sortedYoungestPlayers = youngestPlayers.sortBy(_._2._4).reverse.take(10)
      sortedYoungestPlayers.foreach { case (worldCup, (playerId, familyName, givenName, birthYear)) =>
        println(s"List($playerId,$familyName,$givenName,$birthYear)")
      }
    }

    println("")
    println("Jugadores mas jovenes en disputar un Mundial:")
    youngestPlayerPerWorldCup()



    }

}
