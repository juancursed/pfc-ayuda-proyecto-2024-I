package proyecto

class Itinerario() {
  case class Itinerario(vuelos: List[Vuelo])
  type aeropuertos = List[Aeropuerto]
  type vuelos = List[Vuelo]
/*
  def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    def buscarItinerarios(cod1: String, cod2: String, visitados: Set[String]): List[List[Vuelo]] = {
      if (cod1 == cod2) {
        List(List())
      } else {
        vuelos.filter(_.Org == cod1).flatMap { vuelo =>
          if (!visitados(vuelo.Dst)) {
            val nuevosVisitados = visitados + vuelo.Dst
            val itinerariosRestantes = buscarItinerarios(vuelo.Dst, cod2, nuevosVisitados)
            itinerariosRestantes.map(vuelo :: _)
          } else {
            Nil
          }
        }
      }
    }

    def construirItinerarios(vuelos: List[Vuelo]): List[Itinerario] = {
      vuelos.map(vuelo => Itinerario(List(vuelo)))
    }

    (cod1: String, cod2: String) => {
      val posiblesItinerarios = buscarItinerarios(cod1, cod2, Set())
      posiblesItinerarios.map(construirItinerarios)
    }
  }
 
   def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {

    def buscarVuelos(cod1: String, cod2: String): List[Itinerario] = {
      (cod1, cod2) match{
        case (_, "") => List[Itinerario]()
        case ("", _) => List[Itinerario]()
        case (_ , _) =>
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

          val itinerariosDosEscalas = for {
            f1 <- vuelosConEscalasOrigen
            f2 <- vuelos.filter(v => v.Org == f1.Dst)
            f3 <- vuelosConEscalasDestino if f2.Dst == f3.Org
          } yield List(f1, f2, f3)

          val todosItinerarios = (itinerariosDirectos ++ itinerariosUnaEscala ++ itinerariosDosEscalas)
            .filter(itinerario => itinerario.nonEmpty)

          todosItinerarios
            .sortBy(tiempoTotal)
            .take(3)
            .map(Itinerario)

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
  } */

  def itinerariosSalida(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => List[Itinerario] = {
    def convertirAMinutos(horas: Int, minutos: Int): Int = horas * 60 + minutos

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

    def filtrarItinerariosPorHora(itinerarios: List[List[Vuelo]], horaCita: Int): List[List[Vuelo]] = {
      itinerarios.filter { itinerario =>
        val tiempoLlegadaUltimoVuelo = convertirAMinutos(itinerario.last.HL, itinerario.last.ML)
        tiempoLlegadaUltimoVuelo <= horaCita
      }
    }

    (cod1: String, cod2: String, HC: Int, MC: Int) => {
      val horaCitaEnMinutos = convertirAMinutos(HC, MC)
      val todosItinerarios = encontrarItinerarios(cod1, cod2, Set(), List())
      val itinerariosValidos = filtrarItinerariosPorHora(todosItinerarios, horaCitaEnMinutos)
      val mejoresItinerarios = itinerariosValidos.sortBy { itinerario =>
        convertirAMinutos(itinerario.last.HL, itinerario.last.ML)
      }.take(3)
      mejoresItinerarios.map(Itinerario)
    }
  }

}
