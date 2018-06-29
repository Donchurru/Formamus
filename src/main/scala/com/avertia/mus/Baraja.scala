package com.avertia.mus

import scala.util.Random

/**
 * Case class con la estructura de datos de una carta
 *
 * @param valor As, 2, 3, 4, 5, 6, 7, Sota, Caballo, Rey
 * @param palo  Oros, Copas, Espadas y Bastos
 */
case class Carta(valor: String, palo: String)

object Baraja {

  private val mazo =
    Seq(Carta("As", "Oros"), Carta("2", "Oros"), Carta("3", "Oros"), Carta("4", "Oros"),
      Carta("5", "Oros"), Carta("6", "Oros"), Carta("7", "Oros"), Carta("Sota", "Oros"),
      Carta("Caballo", "Oros"), Carta("Rey", "Oros"),
      Carta("As", "Copas"), Carta("2", "Copas"), Carta("3", "Copas"), Carta("4", "Copas"),
      Carta("5", "Copas"), Carta("6", "Copas"), Carta("7", "Copas"), Carta("Sota", "Copas"),
      Carta("Caballo", "Copas"), Carta("Rey", "Copas"),
      Carta("As", "Espadas"), Carta("2", "Espadas"), Carta("3", "Espadas"), Carta("4", "Espadas"),
      Carta("5", "Espadas"), Carta("6", "Espadas"), Carta("7", "Espadas"), Carta("Sota", "Espadas"),
      Carta("Caballo", "Espadas"), Carta("Rey", "Espadas"),
      Carta("As", "Bastos"), Carta("2", "Bastos"), Carta("3", "Bastos"), Carta("4", "Bastos"),
      Carta("5", "Bastos"), Carta("6", "Bastos"), Carta("7", "Bastos"), Carta("Sota", "Bastos"),
      Carta("Caballo", "Bastos"), Carta("Rey", "Bastos"))

  /**
   * En esta variable almacenamos los descartes cada vez que nos damos mus
   */
  var descartes = Seq[Carta]()

  /**
   * Inicialmente sera el mazo de cartas completo y habrá que ir actualizandolo a medida que
   * vamos repartiendo cartas, de tal manera, que las repartidas desaparecen del mazoActualizado
   */
  var mazoActualizado = Random.shuffle(mazo)

  /**
   * Este metodo es el encargado de repartir las cartas del mazo. En la mano inicial repartira 4
   * cartas siempre, pero en las siguientes repartira un numero "n" dependiendo de los descartes.
   * Hay que tener en cuenta que el mazo puede llegar a acabarse, en cuyo caso, y si es
   * necesario, el mazoActualizado una vez acabado, se recargará con los descartes de los muses
   *
   * Pista: El metodo "take" de las colecciones puede ser de ayuda. Mirar tambien los metodos que
   * permiten añadir elementos a una coleccion dejandolo en otra coleccion.
   *
   * @param numero Numero de cartas a repartir
   * @return Seq[[Carta]]
   */
  def repartirCartas(numero: Int): Seq[Carta] = {
    val cartas = mazoActualizado take numero
    if (cartas.length < numero) volcarDescartesEnMazo
    val repartidas = cartas ++ (mazoActualizado take (numero - cartas.length))
    actualizaMazo(repartidas)
    repartidas
  }

  /**
   * Este método nos sirve para dejar actualizado el mazoActualizado, que sera el actual menos
   * las repartidas.
   * Pista: Mirar los metodos que permiten añadir, quitar, etc de una colección
   *
   * @param repartidas Seq[[Carta]] de las cartas repartidas y que hay que quitar del
   *                   mazoActualizado
   */
  private def actualizaMazo(repartidas: Seq[Carta]): Unit = {
    mazoActualizado = mazoActualizado diff repartidas
  }

 /**
   * Este metodo deja el mazoActualizado con todas las cartas de mazo. Se utilizara solo cuando
   * acaba una partida. Recordad dejar descartes a cero. Actualmente se utiliza en test para
   * dejar la baraja limpia en cada test.
   */
  def reiniciaMazo: Unit = {
    mazoActualizado = Random.shuffle(mazo)
    descartes = Seq()
  }

  /**
   * Este metodo se utilizará cuando se acaben las cartas del mazoActualizado. En ese momento, se
   * pondran en el mazoActualizado los descartes de los muses que haya habido. Tener en cuenta
   * que los descartes despues de esta acción no existiran
   */
  def volcarDescartesEnMazo: Unit = {
    mazoActualizado = Random.shuffle(descartes)
    descartes = Seq()
  }

  /**
   * Con este metodo podremos añadir los descartes de cada mus dado a la variable de descartes.
   *
   * Pista: Consultar metodos de manejos de listas y conjuntos para unir elementos en una nueva
   * coleccion
   *
   * @param descarte Seq[[Carta]] a insertar en la variable descartes.
   */
  def descartar(descarte: Seq[Carta]): Unit = {
    descartes ++= descarte
  }

  /**
   * Este metodo nos informara del numero de cartas que quedan en el mazoActualizado
   *
   * @return [[Int]] con el numero de cartas del mazoActualizado
   */
  def cuantasQuedan: Int = mazoActualizado.length

  /**
   * Metodo que mete en un String las cartas que quedan en el mazoActualizado
   *
   * @return [[String]] con las cartas restantes del mazoActualizado
   */
  override def toString: String =
    s"""Cartas que quedan en el mazo: ${Baraja.cuantasQuedan}
       |*******************************
       |${mazoActualizado map (x => s"${x.valor} de ${x.palo}\n") mkString}
       |""".stripMargin
}
