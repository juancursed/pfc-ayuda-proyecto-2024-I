package proyecto

class ItinerariosPar() {
  type aeropuertos = List[Aeropuerto]
  type vuelos = List[Vuelo]
  //--------------------------------------------------------------------------------------------------------------------------------
  def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {

    val vuelosOrg = vuelos.groupBy(_.Org)

    def buscarItinerarios(codOrg: String, codDst: String, visitados: Set[String]): List[List[Vuelo]] = codDst match {
      case `codDst` => List(List())
      case _ =>
        val vuelosDisp = vuelosOrg.getOrElse(codOrg, List())

  
        val futures = vuelosDisp.par.flatMap { vuelo =>
          if (!visitados.contains(vuelo.Dst)) {
            Future {
              val itinerariosRestantes = buscarItinerarios(vuelo.Dst, codDst, visitados + vuelo.Org)
              itinerariosRestantes.map(itinerario => vuelo :: itinerario)
            }
          } else {
            Future.successful(List())
          }
        }


        val resultFuture = Future.sequence(futures.toList)
        val result = Await.result(resultFuture, Duration.Inf)
        result.flatten.toList
    }

    (cod1: String, cod2: String) => buscarItinerarios(cod1, cod2, Set())
  }

  //--------------------------------------------------------------------------------------------------------------------------------

  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    val timeZoneAirport = aeropuertos.map(a => a.Cod -> a.GMT).toMap

    def totalTiempo(itinerario: List[Vuelo]): Int = {
      itinerario.foldLeft(0) { (acc, vuelo) =>
        val salida = cambioZonaHoraria(vuelo.Org, vuelo.HS + vuelo.MS / 100)
        val llegada = cambioZonaHoraria(vuelo.Dst, vuelo.HL + vuelo.ML / 100)
        val total = if (llegada >= salida) llegada - salida else (24 + llegada) - salida
        acc + total
      }
    }

    def auxItinerarioTiempo(codigoOrigen: String, codigoDestino: String): List[List[Vuelo]] = {
      val listaItinerarios = itinerarios(vuelos, aeropuertos)(codigoOrigen, codigoDestino)


      val futures = listaItinerarios.par.map { itinerario =>
        Future {
          totalTiempo(itinerario)
        }
      }


      val resultFuture = Future.sequence(futures.toList)
      val tiempos = Await.result(resultFuture, Duration.Inf)


      val itinerariosConTiempos = listaItinerarios.zip(tiempos)
      val itinerariosOrdenados = itinerariosConTiempos.sortBy(_._2)


      itinerariosOrdenados.map(_._1).take(3)
    }


    def cambioZonaHoraria(codigo: String, hora: Int): Int = {
      timeZoneAirport.getOrElse(codigo, 0) match {
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

    def auxiliarEscalas(codigoOrigen: String, codigoDestino: String): List[List[Vuelo]] = {
      val listaItinerarios = itinerarios(vuelos, aeropuertos)(codigoOrigen, codigoDestino)



      val futures = listaItinerarios.par.map { itinerario =>
        Future {
          numEscalas(itinerario)
        }
      }


      val resultFuture = Future.sequence(futures.toList)
      val numeroDeEscalas = Await.result(resultFuture, Duration.Inf)


      val itinerariosConEscalas = listaItinerarios.zip(numeroDeEscalas)
      val itinerariosOrdenados = itinerariosConEscalas.sortBy(_._2)


      itinerariosOrdenados.map(_._1).take(3)
    }

    (cod1: String, cod2: String) => auxiliarEscalas(cod1, cod2)
  }

  //--------------------------------------------------------------------------------------------------------------------------------

  def itinerariosAire(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {

    def auxItinerarios(codigoOrigen: String, codigoDestino: String): List[List[Vuelo]] = {
      val itinerariosTotales = itinerarios(vuelos, aeropuertos)(codigoOrigen, codigoDestino)
      itinerariosTotales.take(3)
    }

    (cod1: String, cod2: String) => {
      val futureResult = Future {
        auxItinerarios(cod1, cod2)
      }

      Await.result(futureResult, Duration.Inf)
    }
  }

  //--------------------------------------------------------------------------------------------------------------------------------

  def itinerariosSalida(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => List[Vuelo] = {

    def tiempo(itinerario: List[Vuelo], horaLlegada: Int, minLlegada: Int): Int = {
      val ultimaLlegada = itinerario.last.HL * 60 + itinerario.last.ML
      val llegadaEsperada = horaLlegada * 60 + minLlegada
      val diferencia = llegadaEsperada - ultimaLlegada
      if (diferencia >= 0) diferencia else Int.MaxValue
    }

    def mejorItinerario(codigoOrigen: String, codigoDestino: String, horaLlegada: Int, minLlegada: Int): List[Vuelo] = {
      val listaItinerarios = itinerarios(vuelos, aeropuertos)(codigoOrigen, codigoDestino)



      val futures = listaItinerarios.par.map { itinerario =>
        Future {
          tiempo(itinerario, horaLlegada, minLlegada)
        }
      }



      val resultFuture = Future.sequence(futures.toList)
      val tiempos = Await.result(resultFuture, Duration.Inf)



      val itinerariosConTiempos = listaItinerarios.zip(tiempos)
      val itinerariosValidos = itinerariosConTiempos.filter(_._2 < Int.MaxValue).sortBy(_._2)



      itinerariosValidos.map(_._1).headOption.getOrElse(List.empty[Vuelo])
    }

    (cod1: String, cod2: String, horaCita: Int, minCita: Int) => mejorItinerario(cod1, cod2, horaCita, minCita)
  }

}
