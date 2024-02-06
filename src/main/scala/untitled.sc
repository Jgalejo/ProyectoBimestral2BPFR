import org.nspl._
import org.nspl.awtrenderer._

// Define tus categorías y frecuencias
val categories = List("Categoria1", "Categoria2")
val frequencies = List(10d, 11d)

// Mapea tus categorías a números
val categoryMap = categories.zipWithIndex.map { case (cat, idx) => (idx.toDouble + 1, cat) }.toMap
val barPlotData = categoryMap.keys.toList.zip(frequencies)

val plot = xyplot(
  barPlotData -> bar(horizontal=false,
    width = 0.5,
    fill = Color.gray2)
)(
  par
    .xlab("Categorías")
    .ylab("Frecuencia")
    .xlim(Some(0d -> (categoryMap.size + 1).toDouble))
)

// Renderiza el gráfico
renderToByteArray(plot.build, width=2000)
