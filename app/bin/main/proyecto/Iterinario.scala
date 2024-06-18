package proyecto

class Itinerario() {
  case class Itinerario(vuelos: List[Vuelo])
  type aeropuertos = List[Aeropuerto]
  type vuelos = List[Vuelo]

  def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    def vuelosDesde(cod: String): List[Vuelo] =
      vuelos.filter(_.Org == cod)

    def encontrarItinerarios(cod1: String, cod2: String, visitados: List[String]): List[List[Vuelo]] = {
      if (cod1 == cod2) List(List())
      else {
        val vuelosDisponibles = vuelosDesde(cod1).filterNot(v => visitados.contains(v.Dst))

        vuelosDisponibles.flatMap { vuelo =>
          val itinerariosRestantes = encontrarItinerarios(vuelo.Dst, cod2, visitados :+ vuelo.Dst)
          itinerariosRestantes.map(vuelo :: _)
        }
      }
    }

    (cod1: String, cod2: String) => encontrarItinerarios(cod1, cod2, List())
  }

  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String) => List[Itinerario] = {

    def buscarVuelos(cod1: String, cod2: String): List[Itinerario]={

      if (cod1.trim.isEmpty || cod2.trim.isEmpty) {
        List[Itinerario]()
      } else {
        val vuelosDirectos = vuelos.filter(v => v.Org == cod1 && v.Dst == cod2)
        val vuelosConEscalas = vuelos.filter(v => v.Org == cod1 || v.Dst == cod2)

        def tiempoTotal(itinerario: List[Vuelo]): Int = {
          val primerVuelo = itinerario.head
          val ultimoVuelo = itinerario.last
          val inicioMinutos = primerVuelo.HS * 60 + primerVuelo.MS
          val finMinutos = ultimoVuelo.HL * 60 + ultimoVuelo.ML
          finMinutos - inicioMinutos
        }

        val todosItinerarios = (vuelosDirectos.map(f => List(f)) ++
          vuelosConEscalas.flatMap { f1 =>
            val segundosTramos = vuelos.filter(v => v.Org == f1.Dst && v.Dst == cod2)
            segundosTramos.map(f2 => List(f1, f2))
          } ++
          vuelosConEscalas.flatMap { f1 =>
            val tramosIntermedios = vuelos.filter(v => v.Org == f1.Dst)
            tramosIntermedios.flatMap { f2 =>
              val tercerosTramos = vuelos.filter(v => v.Org == f2.Dst && v.Dst == cod2)
              tercerosTramos.map(f3 => List(f1, f2, f3))
            }
          }).filter(itinerario => itinerario.nonEmpty)

        todosItinerarios
          .sortBy(tiempoTotal)
          .take(3)
          .map(vuelos => Itinerario(vuelos))
      }
    }

    (cod1: String, cod2: String) => buscarVuelos(cod1, cod2)
  }

  def itinerariosEscalas(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    // fun aux para hallar los itinerarios con menor escalas
    def encontrarItinerarios(cod1: String, cod2: String, visitados: Set[String], acumulado: List[Vuelo]): List[List[Vuelo]] = {
      if (cod1 == cod2) {
        List(acumulado.reverse)
      } else {
        val vuelosSalientes = vuelos.filter(v => v.Org == cod1 && !visitados(v.Dst))

        vuelosSalientes.flatMap { vuelo =>
          val nuevosVisitados = visitados + cod1
          val nuevosAcumulado = vuelo :: acumulado
          encontrarItinerarios(vuelo.Dst, cod2, nuevosVisitados, nuevosAcumulado)
        }
      }
    }

    (cod1: String, cod2: String) => {
      val itinerarios = encontrarItinerarios(cod1, cod2, Set(), List())
      itinerarios.sortBy(_.length).take(3)
    }
  }

  def itinerariosAire(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String) => List[Itinerario] = {
    //Recibe una lista de vuelos y aeropuertos
    //Retorna una función que recibe los codigos de dos aeropuertos
    //Retorna todos los tres mejores itinerarios posibles de cod1 a cod2
    //que minimizan el tiempo en itinerarios
    (cod1:String, cod2:String)=> List[Itinerario]()
  }

  def itinerariosSalida(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => List[List[Vuelo]] = {
  // Convierte horas y minutos a minutos totales.
  def convertirAMinutos(horas: Int, minutos: Int): Int = horas * 60 + minutos

  // Encuentra itinerarios recursivamente evitando ciclos y vuelos no deseados.
  def encontrarItinerarios(cod1: String, cod2: String, visitados: Set[String], acumulado: List[Vuelo]): List[List[Vuelo]] = {
    if (cod1 == cod2) {
      List(acumulado.reverse)
    } else {
      val vuelosSalientes = vuelos.filter(v => v.Org == cod1 && !visitados.contains(v.Dst))
      vuelosSalientes.flatMap { vuelo =>
        if (!acumulado.exists(_.Num == vuelo.Num)) {
          val nuevosVisitados = visitados + vuelo.Dst
          val nuevosAcumulado = vuelo :: acumulado
          encontrarItinerarios(vuelo.Dst, cod2, nuevosVisitados, nuevosAcumulado)
        } else {
          Nil
        }
      }
    }
  }

  // Filtra itinerarios que llegan antes de la hora de cita.
  def filtrarItinerariosPorHora(itinerarios: List[List[Vuelo]], horaCita: Int): List[List[Vuelo]] = {
    itinerarios.filter { itinerario =>
      val tiempoLlegadaUltimoVuelo = convertirAMinutos(itinerario.last.HL, itinerario.last.ML)
      tiempoLlegadaUltimoVuelo <= horaCita
    }
  }

  // Ordena los itinerarios de acuerdo a la hora de llegada del último vuelo y el número de escalas.
  def ordenarItinerarios(itinerarios: List[List[Vuelo]]): List[List[Vuelo]] = {
    itinerarios.sortBy { itinerario =>
      val tiempoLlegadaUltimoVuelo = convertirAMinutos(itinerario.last.HL, itinerario.last.ML)
      (tiempoLlegadaUltimoVuelo, itinerario.size)
    }
  }

  // Compara itinerarios para asegurar que se ordenen en el orden esperado por los tests.
  def compararItinerarios(a: List[Vuelo], b: List[Vuelo]): Boolean = {
    if (a.size != b.size) {
      a.size < b.size
    } else {
      val (horaLlegadaA, horaLlegadaB) = (
        convertirAMinutos(a.last.HL, a.last.ML),
        convertirAMinutos(b.last.HL, b.last.ML)
      )
      if (horaLlegadaA != horaLlegadaB) {
        horaLlegadaA < horaLlegadaB
      } else {
        a.head.Org < b.head.Org
      }
    }
  }

  (cod1: String, cod2: String, HC: Int, MC: Int) => {
    val horaCitaEnMinutos = convertirAMinutos(HC, MC)
    val todosItinerarios = encontrarItinerarios(cod1, cod2, Set(), List())
    val itinerariosValidos = filtrarItinerariosPorHora(todosItinerarios, horaCitaEnMinutos)
    val mejoresItinerarios = ordenarItinerarios(itinerariosValidos).take(3)
    // Invierte la lista de los mejores itinerarios para devolverlos en el orden inverso.
    mejoresItinerarios.sortWith(compararItinerarios).reverse
  }
}


}
