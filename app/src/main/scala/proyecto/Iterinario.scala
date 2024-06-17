package proyecto

class Itinerario() {
  case class Itinerario(vuelos: List[Vuelo])
  type aeropuertos = List[Aeropuerto]
  type vuelos = List[Vuelo]

  def itinerarios(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String) => List[Itinerario] = {
    (cod1:String, cod2:String)=> List[Itinerario]()
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

  def itinerariosEscalas(vuelos:List[Vuelo], aeropuertos:List[Aeropuerto]):(String, String)=>List[Itinerario]
  = {
    //Recibe una lista de vuelos y aeropuertos
    //Retorna una función que recibe los codigos de dos aeropuertos
    //Retorna todos los tres mejores itinerarios posibles de cod1 a cod2
    //que minimizan el número de escalas
    (cod1:String, cod2:String)=> List[Itinerario]()
  }

  def itinerariosAire(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String) => List[Itinerario] = {
    //Recibe una lista de vuelos y aeropuertos
    //Retorna una función que recibe los codigos de dos aeropuertos
    //Retorna todos los tres mejores itinerarios posibles de cod1 a cod2
    //que minimizan el tiempo en itinerarios
    (cod1:String, cod2:String)=> List[Itinerario]()
  }

  def itinerariosSalida(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String, Int, Int) => List[Itinerario] = {
    //Recibe una lista de vuelos y aeropuertos
    //Retorna una función que recibe los codigos de dos aeropuertos y dos enteros, que es la hora de la cita
    //Retorna todos los tres mejores itinerarios posibles de cod1 a cod2
    //que permiten llegar a una hora de la cita
    (cod1:String, cod2:String, HC:Int, MC:Int)=> List[Itinerario]()
  }

}
