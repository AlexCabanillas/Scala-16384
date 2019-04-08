
/*====================================================================================
|                 Luis Alejandro Cabanillas Prudencio  DNI: 04236930P                |
|                    Álvaro de las Heras Fernández  DNI: 03146833L                   |
|                                                                                    |
|         16384 Juego que simula al 2048 implementado con Scala                      |
====================================================================================*/

object game {

  //------------------------------FUNCIONES GENERICAS---------------------------------

  //Obtiene el valor de una posicion indice del tablero
  def obtener(tablero: List[Int], indice: Int): Int = {
    if (!tablero.isEmpty) {
      if (indice == 1) tablero.head
      else obtener(tablero.tail, indice - 1)
    } else -1
  }

  def crearValorRandom(dificultad: Int): Int = {
    val valores = List(2, 4, 8)
    val random = util.Random;
    if(dificultad < 4){
      val colo=obtener(valores,random.nextInt(dificultad)+1)
      colo
    }else{
      val colo=obtener(valores,random.nextInt(3)+1)
      colo
      
    }
  }

  def crearRandomPos(tam: Int): Int = {
    val random = util.Random;
    val pos = random.nextInt(tam) + 1
    pos
  }

  def poner(lista: List[Int], dificultad: Int): List[Int] = {
    val pos = crearRandomPos(lista.length)
    val col = crearValorRandom(dificultad)
    if (lista.isEmpty) Nil
    if (obtener(lista, pos) == 0)
      if (pos == 1) col :: lista.tail
      else lista.head :: poner(lista.tail, dificultad)
    else poner(lista, dificultad)
  }

  def poner2(l: List[Int], valor: Int, pos: Int): List[Int] = {
    if (l.isEmpty) Nil
    else if (pos == 1) valor :: l.tail
    else l.head :: poner2(l.tail, valor, pos - 1)
  }

  def generarTab(tam: Int): List[Int] = {
    if (tam == 0) Nil
    else 0 :: generarTab(tam - 1)
  }

  def huecosLibres(tablero: List[Int]): Int = {
    if (tablero.length > 0) {
      if (tablero.head == 0)
        huecosLibres(tablero.tail) + 1
      else
        huecosLibres(tablero.tail)
    } else 0
  }
  def rellenarTab(tablero: List[Int], numCasillas: Int,dificultad:Int): List[Int] = {
    if (tablero.isEmpty) Nil
    else if ((huecosLibres(tablero) > 0) && (numCasillas != 0)) { rellenarTab(poner(tablero,dificultad), numCasillas - 1,dificultad) }
    else tablero
  }

  def imprimir(lista: List[Int], columnas: Int) {
    if (!lista.isEmpty) {
      if (lista.length % columnas == 0) println()
      print("|" + lista.head + "|")
      imprimir(lista.tail, columnas)
    }
  }

  def eliminarSumar(tablero: List[Int], indice: Int): List[Int] = {
    if (!tablero.isEmpty) {
      if (indice == 1) 0 :: tablero.tail
      else tablero.head :: eliminarSumar(tablero.tail, indice - 1)
    } else tablero
  }

  //--------------------------FUNCIONES AUXILIARES DE MOVIMIENTO--------------------------

  def reverse(lista: List[Int]): List[Int] = {
    if (lista.length == 0) lista
    else reverse(lista.tail) ::: lista.head :: Nil
  }

  //------------------------   MOVIMIENTOS   -------------------------------

  def moverAbajo(tablero: List[Int], columnas: Int): List[Int] = {
    if (!tablero.isEmpty) {
      if (tablero.head > 0) {
        if (obtener(tablero, columnas + 1) == 0) moverAbajo(poner2(eliminarSumar(tablero, 1), tablero.head, columnas + 1), columnas)
        else tablero.head :: moverAbajo(tablero.tail, columnas)

      } else tablero.head :: moverAbajo(tablero.tail, columnas)
    } else tablero
  }
  def moverDerecha(tablero: List[Int], columnas: Int): List[Int] = {
    if (!tablero.isEmpty) {
      if ((tablero.head > 0)) {
        if ((obtener(tablero, 2) == 0) && ((tablero.length) % columnas != 1)) moverDerecha(poner2(eliminarSumar(tablero, 1), tablero.head, 2), columnas)
        else tablero.head :: moverDerecha(tablero.tail, columnas)

      } else tablero.head :: moverDerecha(tablero.tail, columnas)
    } else tablero
  }
  def moverArriba(tablero: List[Int], columnas: Int): List[Int] = {
    val tablero_aux = reverse(tablero)
    if (!tablero_aux.isEmpty) {
      if (tablero_aux.head > 0) {
        if (obtener(tablero_aux, columnas + 1) == 0) reverse(moverAbajo(poner2(eliminarSumar(tablero_aux, 1), tablero_aux.head, columnas + 1), columnas))
        else reverse(tablero_aux.head :: moverAbajo(tablero_aux.tail, columnas))

      } else reverse(tablero_aux.head :: moverAbajo(tablero_aux.tail, columnas))
    } else tablero_aux
  }
  def moverIzquierda(tablero: List[Int], columnas: Int): List[Int] = {
    val tablero_aux = reverse(tablero)
    if (!tablero_aux.isEmpty) {
      if (tablero_aux.head > 0) {
        if (obtener(tablero_aux, 2) == 0) reverse(moverDerecha(poner2(eliminarSumar(tablero_aux, 1), tablero_aux.head, 2), columnas))
        else reverse(tablero_aux.head :: moverDerecha(tablero_aux.tail, columnas))

      } else reverse(tablero_aux.head :: moverDerecha(tablero_aux.tail, columnas))
    } else tablero_aux
  }

  //------------------------ MOVIMIENTO DE TODAS LAS CASILLAS-------------------------------

  def moverTodoAbajo(tablero: List[Int], movimientos: Int, columnas: Int): List[Int] = {
    if (movimientos == 1) tablero
    else moverTodoAbajo(moverAbajo(tablero, columnas), movimientos - 1, columnas)
  }
  def moverTodoArriba(tablero: List[Int], movimientos: Int, columnas: Int): List[Int] = {
    if (movimientos == 1) tablero
    else moverTodoArriba(moverArriba(tablero, columnas), movimientos - 1, columnas)
  }
  def moverTodoDerecha(tablero: List[Int], movimientos: Int, columnas: Int): List[Int] = {
    if (movimientos == 1) tablero
    else moverTodoDerecha(moverDerecha(tablero, columnas), movimientos - 1, columnas)
  }
  def moverTodoIzquierda(tablero: List[Int], movimientos: Int, columnas: Int): List[Int] = {
    if (movimientos == 1) tablero
    else moverTodoIzquierda(moverIzquierda(tablero, columnas), movimientos - 1, columnas)
  }

  def sumarIzquierda(tablero: List[Int], columnas: Int): List[Int] = {
    if (!tablero.isEmpty) {
      if ((tablero.head > 0)) {
        if ((obtener(tablero, 2) == tablero.head) && ((tablero.length) % columnas != 1)) sumarIzquierda(poner2(eliminarSumar(tablero, 2), tablero.head * 2, 1), columnas)
        else tablero.head :: sumarIzquierda(tablero.tail, columnas)
      } else tablero.head :: sumarIzquierda(tablero.tail, columnas)
    } else tablero
  }
  def sumarArriba(tablero: List[Int], columnas: Int): List[Int] = {
    if (!tablero.isEmpty) {
      if ((tablero.head > 0)) {
        if (obtener(tablero, columnas + 1) == tablero.head) sumarArriba(poner2(eliminarSumar(tablero, columnas + 1), tablero.head * 2, 1), columnas)
        else tablero.head :: sumarArriba(tablero.tail, columnas)

      } else tablero.head :: sumarArriba(tablero.tail, columnas)
    } else tablero
  }

  def juego(tablero: List[Int], columnas: Int, dificultad: Int): Unit = {
    imprimir(tablero, columnas)
    print("\nMovimiento: ")
    val tecla = scala.io.StdIn.readChar()
    tecla match {
      case ('w' | 'W') => juego(moverTodoArriba(sumarArriba(moverTodoArriba(tablero, columnas, columnas), columnas), columnas, columnas), columnas, dificultad)
      case ('a' | 'A') => juego(moverTodoIzquierda(sumarIzquierda(moverTodoIzquierda(tablero, columnas, columnas), columnas), columnas, columnas), columnas, dificultad)
      case ('d' | 'D') => juego(moverTodoDerecha(reverse(sumarIzquierda(reverse(moverTodoDerecha(tablero, columnas, columnas)), columnas)), columnas, columnas), columnas, dificultad)
      case ('s' | 'S') => juego(moverTodoAbajo(reverse(sumarArriba(reverse(moverTodoAbajo(tablero, columnas, columnas)), columnas)), columnas, columnas), columnas, dificultad)
      case ('e' | 'E') => println("¡Hasta la próxima!")
    }

  }

  //------------------------------- PRUEBAS ----------------------------------------------

  def main(args: Array[String]) {
    println("¡Bienvenido al juego 16384!\n")
    println("¿Qué dificultad desea?\n")
    val dificultad = scala.io.StdIn.readInt()
    dificultad match {
      case 1 => {
        val tam = 4
        val tableroRelleno = rellenarTab(generarTab(tam * tam), 2,dificultad)
        juego(tableroRelleno, tam, dificultad)
      }
      case 2 => {
        val tam = 9
        val tableroRelleno = rellenarTab(generarTab(tam * tam), 4,dificultad)
        juego(tableroRelleno, tam, dificultad)
      }
      case 3 => {
        val tam = 14
        val tableroRelleno = rellenarTab(generarTab(tam * tam), 6,dificultad)
        juego(tableroRelleno, tam, dificultad)
      }
      case 4 => {
        val tam = 17
        val tableroRelleno = rellenarTab(generarTab(tam * tam), 6,dificultad)
        juego(tableroRelleno, tam, dificultad)
      }
    }

  }

}