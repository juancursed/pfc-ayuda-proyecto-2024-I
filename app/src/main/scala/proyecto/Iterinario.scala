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

  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {

    def buscarVuelos(cod1: String, cod2: String): List[List[Vuelo]] = {
      (cod1, cod2) match {
        case (_, "") => List[List[Vuelo]]()
        case ("", _) => List[List[Vuelo]]()
        case (_, _) =>
          val vuelosDirectos = vuelos.filter(v => v.Org == cod1 && v.Dst == cod2)
          val vuelosConEscalasOrigen = vuelos.filter(v => v.Org == cod1)
          val vuelosConEscalasDestino = vuelos.filter(v => v.Dst == cod2)

          def tiempoTotal(itinerario: List[Vuelo]): Int = {
            val primerVuelo = itinerario.head
            val ultimoVuelo = itinerario.last
            val inicioMinutos = primerVuelo.HS * 60 + primerVuelo.MS
            val finMinutos = ultimoVuelo.HL * 60 + ultimoVuelo.ML
            finMinutos - inicioMinutos
          }

          val itinerariosDirectos = vuelosDirectos.map(v => List(v))

          val itinerariosUnaEscala = for {
            f1 <- vuelosConEscalasOrigen
            f2 <- vuelosConEscalasDestino if f1.Dst == f2.Org
          } yield List(f1, f2)

          val todosItinerarios = (itinerariosDirectos ++ itinerariosUnaEscala)
            .filter(itinerario => itinerario.nonEmpty)

          todosItinerarios.sortBy(tiempoTotal).take(3)
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

  def itinerariosAire(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {

    def distancia(a1: Aeropuerto, a2: Aeropuerto): Double = {
      math.sqrt(math.pow(a2.X - a1.X, 2) + math.pow(a2.Y - a1.Y, 2))
    }

    def buscarVuelo(cod1: String, cod2: String): List[List[Vuelo]] = {
      (cod1, cod2) match {
        case ("", _) => List[List[Vuelo]]()
        case (_, "") => List[List[Vuelo]]()
        case (_, _) =>
          val vuelosDirectos = vuelos.filter(v => v.Org == cod1 && v.Dst == cod2)
          val vuelosConEscalasOrigen = vuelos.filter(v => v.Org == cod1)
          val vuelosConEscalasDestino = vuelos.filter(v => v.Dst == cod2)

          val itinerariosDirectos = vuelosDirectos.map(v => List(v))

          val itinerariosUnaEscala = for {
            f1 <- vuelosConEscalasOrigen
            f2 <- vuelosConEscalasDestino if f1.Dst == f2.Org
          } yield List(f1, f2)

          val itinerariosDosEscalas = for {
            f1 <- vuelosConEscalasOrigen
            f2 <- vuelos.filter(v => v.Org == f1.Dst && v.Dst != f1.Org) // Asegurar que no regrese al origen
            f3 <- vuelosConEscalasDestino if f2.Dst == f3.Org
          } yield List(f1, f2, f3)

          val todosItinerarios = itinerariosDirectos ++ itinerariosUnaEscala ++ itinerariosDosEscalas

          // Filtrar itinerarios que contienen vuelos repetidos
          todosItinerarios.filter(itinerario => {
            val uniqueVuelos = itinerario.distinct
            uniqueVuelos.length == itinerario.length
          })
      }
    }

    def calcularDistanciaTotal(itinerario: List[Vuelo]): Double = {
      itinerario.map { vuelo =>
        val origen = aeropuertos.find(_.Cod == vuelo.Org).get
        val destino = aeropuertos.find(_.Cod == vuelo.Dst).get
        distancia(origen, destino)
      }.sum
    }

    (cod1: String, cod2: String) => {
      val vuelosEncontrados = buscarVuelo(cod1, cod2)
      val itinerariosOrdenados = vuelosEncontrados.sortBy(calcularDistanciaTotal).take(3)
      itinerariosOrdenados
    }
  }


  def itinerariosSalida(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => List[List[Vuelo]] = {
  // Convierte horas y minutos a minutos totales.
  def convertirAMinutos(horas: Int, minutos: Int): Int = horas * 60 + minutos

  // Encuentra itinerarios recursivamente evitando ciclos y duplicados.
  def encontrarItinerarios(cod1: String, cod2: String, visitados: Set[String], acumulado: List[Vuelo]): List[List[Vuelo]] = {
    if (cod1 == cod2) {
      List(acumulado.reverse)
    } else {
      val vuelosSalientes = vuelos.filter(v => v.Org == cod1 && !visitados.contains(v.Dst))
      vuelosSalientes.flatMap { vuelo =>
        // Evita ciclos y vuelos no deseados.
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

   def filtrarItinerariosPorHora(itinerarios: List[List[Vuelo]], horaCita: Int): List[List[Vuelo]] = {
    itinerarios.filter { itinerario =>
      val tiempoLlegadaUltimoVuelo = convertirAMinutos(itinerario.last.HL, itinerario.last.ML)
      tiempoLlegadaUltimoVuelo <= horaCita
    }
  }



  // Ordena itinerarios por tiempo de llegada, tamaño del itinerario y número de vuelo.
  def ordenarItinerarios(itinerarios: List[List[Vuelo]]): List[List[Vuelo]] = {
    itinerarios.sortBy { itinerario =>
      val tiempoLlegadaUltimoVuelo = convertirAMinutos(itinerario.last.HL, itinerario.last.ML)
      (tiempoLlegadaUltimoVuelo, itinerario.size, itinerario.map(_.Num).mkString)
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
    // Devuelve solo los mejores itinerarios, evitando incluir más de los necesarios.
    mejoresItinerarios.sortWith(compararItinerarios)
  }
}



















}
