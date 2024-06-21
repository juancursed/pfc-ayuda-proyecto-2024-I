package proyecto

class Itinerario() {

  type aeropuertos = List[Aeropuerto]
  type vuelos = List[Vuelo]

//--------------------------------------------------------------------------------------------------------------------------------
  def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {

    val vuelosOrg = vuelos.groupBy(_.Org)

    def buscarItinerarios(codOrg: String, codDst: String, visitados: Set[String]): List[List[Vuelo]] = codDst match {
      case `codDst` => List(List())
      case _ =>
        val vuelosDisp = vuelosOrg.getOrElse(codOrg, List())
        vuelosDisp.flatMap { vuelo =>
          if (!visitados.contains(vuelo.Dst)) {
            val itinerariosRestantes = buscarItinerarios(vuelo.Dst, codDst, visitados + vuelo.Org)
            itinerariosRestantes.map(itinerario => vuelo :: itinerario)
          } else {
            List()
          }
        }
    }
    (cod1: String, cod2: String) => buscarItinerarios(cod1, cod2, Set())
  }

  //--------------------------------------------------------------------------------------------------------------------------------

  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    val ZA_Aero = aeropuertos.map(a => a.Cod -> a.GMT).toMap

    def totalTiempo(itn: List[Vuelo]): Int = {
      itn.foldLeft(0) { (acc, vuelo) =>
        val salida = cambioZonaHoraria(vuelo.Org, vuelo.HS + vuelo.MS / 100)
        val llegada = cambioZonaHoraria(vuelo.Dst, vuelo.HL + vuelo.ML / 100)
        val total = if (llegada >= salida) llegada - salida else (24 + llegada) - salida
        acc + total
      }
    }

    def auxItinerarioTiempo(codigoOrigen: String, codigoDestino: String): List[List[Vuelo]] = {
      val listaItinerarios = itinerarios(vuelos, aeropuertos)(codigoOrigen, codigoDestino)
      val tiempos = listaItinerarios.map(totalTiempo)
      val itinerariosConTiempos = listaItinerarios.zip(tiempos)
      val itinerariosOrdenados = itinerariosConTiempos.sortBy(_._2)
      itinerariosOrdenados.map(_._1).take(3)
    }

    def cambioZonaHoraria(cod: String, hora: Int): Int = {
      ZA_Aero.getOrElse(cod, 0) match {
        case -400 => hora + 4
        case -600 => hora
        case -700 => hora
        case -800 => hora
        case 900 => hora + 9
        case -900 => hora
        case 100 => hora + 1
        case 300 => hora + 3
        case 400 => hora + 4
        case -500 => hora
        // Manejo de otro caso
      }
    }

    (cod1: String, cod2: String) => auxItinerarioTiempo(cod1, cod2)
  }

  //--------------------------------------------------------------------------------------------------------------------------------

  def itinerariosEscalas(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {

    def numEscalas(itinerario: List[Vuelo]): Int = {
      itinerario.foldLeft(0) { (acc, vuelo) =>
        acc + vuelo.Esc
      }
    }
    def auxiar_Escalas(cod1: String, cod2: String): List[List[Vuelo]] = {
      val lista = itinerarios(vuelos, aeropuertos)(cod1, cod2)
      val numeroDeEscalas = lista.map(numEscalas)
      val vueloEscalas = lista.zip(numeroDeEscalas)
      val vuelosOrdenadosEscalas = vueloEscalas.sortBy(_._2)
      vuelosOrdenadosEscalas.map(_._1).take(3)
    }

    (cod1: String, cod2: String) => auxiar_Escalas(cod1, cod2)
  }

  //--------------------------------------------------------------------------------------------------------------------------------

  def itinerariosAire(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    def auxItinerarios(codOrg: String, codDst: String): List[List[Vuelo]] = {
      val itinerariosTotales = itinerarios(vuelos, aeropuertos)(codOrg, codDst)
      itinerariosTotales.take(3)
    }
    (cod1: String, cod2: String) => auxItinerarios(cod1, cod2)
  }

  //--------------------------------------------------------------------------------------------------------------------------------

  def itinerariosSalida(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => List[Vuelo] = {
    def tiempo(itn: List[Vuelo], horaLlegada: Int, minLlegada: Int): Int = {
      val ultimaLlegada = itn.last.HL * 60 + itn.last.ML
      val llegadaEsperada = horaLlegada * 60 + minLlegada
      val diferencia = llegadaEsperada - ultimaLlegada
      if (diferencia >= 0) diferencia else Int.MaxValue
    }

    def mejor_Itinerario(codOrg: String, codDst: String, horaLlegada: Int, minLlegada: Int): List[Vuelo] = {
      val list = itinerarios(vuelos, aeropuertos)(codOrg, codDst)
      val itn_Tiempos = list.map(itinerario => (itinerario, tiempo(itinerario, horaLlegada, minLlegada)))
      val itn_Validos = itn_Tiempos.filter(_._2 < Int.MaxValue).sortBy(_._2)
      itn_Validos.map(_._1).headOption.getOrElse(List.empty[Vuelo])
    }
    (cod1: String, cod2: String, horaCita: Int, minCita: Int) => mejor_Itinerario(cod1, cod2, horaCita, minCita)
  }

}





















