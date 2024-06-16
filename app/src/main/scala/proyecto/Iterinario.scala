package proyecto

class Itinerario() {
  case class Itinerario(vuelos: List[Vuelo])
  type aeropuertos = List[Aeropuerto]
  type vuelos = List[Vuelo]

  def itinerarios(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String) => List[Itinerario] = {
    (cod1:String, cod2:String)=> List[Itinerario]()
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
