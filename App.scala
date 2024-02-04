package ec.edu.utpl.computación.pfr.pi

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
