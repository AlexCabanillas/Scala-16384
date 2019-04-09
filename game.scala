
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
    val valores = List(2,4,8)
    val random = util.Random;
    if (dificultad < 4) {
      val colo = obtener(valores, random.nextInt(dificultad) + 1)
      colo
    } else {
      val colo = obtener(valores, random.nextInt(3) + 1)
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
  def rellenarTab(tablero: List[Int], numCasillas: Int, dificultad: Int): List[Int] = {
    if (tablero.isEmpty) Nil
    else if ((huecosLibres(tablero) > 0) && (numCasillas != 0)) { rellenarTab(poner(tablero, dificultad), numCasillas - 1, dificultad) }
    else tablero
  }

    def imprimir(lista: List[Int], columnas: Int) {
    if (!lista.isEmpty) {
      if (lista.length % columnas == 0) print("\n|")
      val corregirEspacios = espacios(lista.head)
      print(corregirEspacios + lista.head+ "|")
      imprimir(lista.tail, columnas)
    }
  }
  def espacios(numero:Int):String={
    " "*(7-digitos(numero))
  }
  def digitos(numero:Int):Int={
    if (numero<10){1}
    else if (numero<100){2}
    else if (numero<1000){3}
    else if (numero<10000){4}
    else if (numero<100000){5}
    else if (numero<1000000){6}
    else {7}
  }

  def eliminar(tablero: List[Int], indice: Int): List[Int] = {
    if (!tablero.isEmpty) {
      if (indice == 1) 0 :: tablero.tail
      else tablero.head :: eliminar(tablero.tail, indice - 1)
    } else tablero
  }

  //--------------------------FUNCIONES AUXILIARES DE MOVIMIENTO--------------------------

  def reverse(lista: List[Int]): List[Int] = {
    if (lista.length == 0) lista
    else reverse(lista.tail) ::: lista.head :: Nil
  }

  def quitarHasta(tablero: List[Int], limite: Int): List[Int] = {
    if (tablero.length == limite) Nil
    else tablero.head :: quitarHasta(tablero.tail, limite)

  }
  //------------------------   MOVIMIENTOS   -------------------------------

  def moverAbajo(tablero: List[Int], columnas: Int): List[Int] = {
    if (!tablero.isEmpty) {
      if (tablero.head > 0) {
        if (obtener(tablero, columnas + 1) == 0) moverAbajo(poner2(eliminar(tablero, 1), tablero.head, columnas + 1), columnas)
        else tablero.head :: moverAbajo(tablero.tail, columnas)

      } else tablero.head :: moverAbajo(tablero.tail, columnas)
    } else tablero
  }
  def moverDerecha(tablero: List[Int], columnas: Int): List[Int] = {
    if (!tablero.isEmpty) {
      if ((tablero.head > 0)) {
        if ((obtener(tablero, 2) == 0) && ((tablero.length) % columnas != 1)) moverDerecha(poner2(eliminar(tablero, 1), tablero.head, 2), columnas)
        else tablero.head :: moverDerecha(tablero.tail, columnas)

      } else tablero.head :: moverDerecha(tablero.tail, columnas)
    } else tablero
  }
  def moverArriba(tablero: List[Int], columnas: Int): List[Int] = {
    val tablero_aux = reverse(tablero)
    if (!tablero_aux.isEmpty) {
      if (tablero_aux.head > 0) {
        if (obtener(tablero_aux, columnas + 1) == 0) reverse(moverAbajo(poner2(eliminar(tablero_aux, 1), tablero_aux.head, columnas + 1), columnas))
        else reverse(tablero_aux.head :: moverAbajo(tablero_aux.tail, columnas))

      } else reverse(tablero_aux.head :: moverAbajo(tablero_aux.tail, columnas))
    } else tablero_aux
  }
  def moverIzquierda(tablero: List[Int], columnas: Int): List[Int] = {
    val tablero_aux = reverse(tablero)
    if (!tablero_aux.isEmpty) {
      if (tablero_aux.head > 0) {
        if (obtener(tablero_aux, 2) == 0) reverse(moverDerecha(poner2(eliminar(tablero_aux, 1), tablero_aux.head, 2), columnas))
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

  def sumarIzquierda(tablero: List[Int], columnas: Int, puntuacion: Int, conteo: Int): List[Int] = {
    if (!tablero.isEmpty) {
      if ((tablero.head > 0)) {
        if ((obtener(tablero, 2) == tablero.head) && ((tablero.length) % columnas != 1)) sumarIzquierda(poner2(eliminar(tablero, 2), tablero.head * 2, 1), columnas, puntuacion + tablero.head * 2, conteo + 1)
        else tablero.head :: sumarIzquierda(tablero.tail, columnas, puntuacion, conteo)
      } else tablero.head :: sumarIzquierda(tablero.tail, columnas, puntuacion, conteo)
    } else tablero ::: puntuacion :: conteo :: Nil
  }
  def sumarArriba(tablero: List[Int], columnas: Int, puntuacion: Int, conteo: Int): List[Int] = {
    if (!tablero.isEmpty) {
      if ((tablero.head > 0)) {
        if (obtener(tablero, columnas + 1) == tablero.head) {
          sumarArriba(poner2(eliminar(tablero, columnas + 1), tablero.head * 2, 1), columnas, puntuacion + tablero.head * 2, conteo + 1)
        } else tablero.head :: sumarArriba(tablero.tail, columnas, puntuacion, conteo)

      } else tablero.head :: sumarArriba(tablero.tail, columnas, puntuacion, conteo)
    } else tablero ::: puntuacion :: conteo :: Nil
  }

  def juego(tablero: List[Int], columnas: Int, dificultad: Int, casillas: Int, puntuacion: Int, conteo: Int, bloqueoEjeX: Boolean, bloqueoEjeY: Boolean, vidas: Int, modo: Boolean): Unit = {
    if (bloqueoEjeX && bloqueoEjeY) {
      println("¡Has perdido la partida!")
      nuevaPartida(dificultad, vidas - 1, puntuacion,modo)
    } else {
      imprimir(tablero, columnas)

      println("\nPuntuacion: " + puntuacion + "\tConteo: " + conteo + "\tVidas: " + vidas)
      print("\nMovimiento: ")

      val tecla = movimiento(modo)
      tecla match {
        case ('w' | 'W') => {
          val tableroSumado = sumarArriba(moverTodoArriba(tablero, columnas, columnas), columnas, puntuacion, conteo)
          if (huecosLibres(tablero) == 0) {
            if (puntuacion == obtener(tableroSumado, tableroSumado.length - 1)) {
              println("Ya no puedes mover verticalmente")
              juego(
                rellenarTab(moverTodoArriba(quitarHasta(tableroSumado, 2), columnas, columnas), casillas, dificultad),
                columnas, dificultad, casillas, obtener(tableroSumado, tableroSumado.length - 1), obtener(tableroSumado, tableroSumado.length), bloqueoEjeX, true, vidas, modo)
            } else {
              juego(
                rellenarTab(moverTodoArriba(quitarHasta(tableroSumado, 2), columnas, columnas), casillas, dificultad),
                columnas, dificultad, casillas, obtener(tableroSumado, tableroSumado.length - 1), obtener(tableroSumado, tableroSumado.length), false, false, vidas, modo)
            }
          } else {
            juego(
              rellenarTab(moverTodoArriba(quitarHasta(tableroSumado, 2), columnas, columnas), casillas, dificultad),
              columnas, dificultad, casillas, obtener(tableroSumado, tableroSumado.length - 1), obtener(tableroSumado, tableroSumado.length), false, false, vidas, modo)

          }
        }
        case ('a' | 'A') => {
          val tableroSumado = sumarIzquierda(moverTodoIzquierda(tablero, columnas, columnas), columnas, puntuacion, conteo)
          if (huecosLibres(tablero) == 0) {
            if (puntuacion == obtener(tableroSumado, tableroSumado.length - 1)) {
              println("Ya no puedes mover horizontalmente")
              juego(
                rellenarTab(moverTodoIzquierda(quitarHasta(tableroSumado, 2), columnas, columnas), casillas, dificultad),
                columnas, dificultad, casillas, obtener(tableroSumado, tableroSumado.length - 1), obtener(tableroSumado, tableroSumado.length), true, bloqueoEjeY, vidas, modo)

            } else {
              juego(
                rellenarTab(moverTodoIzquierda(quitarHasta(tableroSumado, 2), columnas, columnas), casillas, dificultad),
                columnas, dificultad, casillas, obtener(tableroSumado, tableroSumado.length - 1), obtener(tableroSumado, tableroSumado.length), false, false, vidas, modo)
            }
          } else {
            juego(
              rellenarTab(moverTodoIzquierda(quitarHasta(tableroSumado, 2), columnas, columnas), casillas, dificultad),
              columnas, dificultad, casillas, obtener(tableroSumado, tableroSumado.length - 1), obtener(tableroSumado, tableroSumado.length), false, false, vidas, modo)
          }
        }
        case ('d' | 'D') => {
          val tableroSumado = sumarIzquierda(reverse(moverTodoDerecha(tablero, columnas, columnas)), columnas, puntuacion, conteo)
          if (huecosLibres(tablero) == 0) {
            if (puntuacion == obtener(tableroSumado, tableroSumado.length - 1)) {

              println("Ya no puedes mover horizontalmente")
              juego(
                rellenarTab(moverTodoDerecha(reverse(quitarHasta(tableroSumado, 2)), columnas, columnas), casillas, dificultad),
                columnas, dificultad, casillas, obtener(tableroSumado, tableroSumado.length - 1), obtener(tableroSumado, tableroSumado.length), true, bloqueoEjeY, vidas, modo)
            } else {
              juego(
                rellenarTab(moverTodoDerecha(reverse(quitarHasta(tableroSumado, 2)), columnas, columnas), casillas, dificultad),
                columnas, dificultad, casillas, obtener(tableroSumado, tableroSumado.length - 1), obtener(tableroSumado, tableroSumado.length), false, false, vidas, modo)
            }
          } else {
            juego(
              rellenarTab(moverTodoDerecha(reverse(quitarHasta(tableroSumado, 2)), columnas, columnas), casillas, dificultad),
              columnas, dificultad, casillas, obtener(tableroSumado, tableroSumado.length - 1), obtener(tableroSumado, tableroSumado.length), false, false, vidas, modo)
          }
        }
        case ('s' | 'S') => {
          val tableroSumado = sumarArriba(reverse(moverTodoAbajo(tablero, columnas, columnas)), columnas, puntuacion, conteo)
          if (huecosLibres(tablero) == 0) {
            if (puntuacion == obtener(tableroSumado, tableroSumado.length - 1)) {
              println("Ya no puedes mover verticalmente")
              juego(
                rellenarTab(moverTodoAbajo(reverse(quitarHasta(tableroSumado, 2)), columnas, columnas), casillas, dificultad),
                columnas, dificultad, casillas, obtener(tableroSumado, tableroSumado.length - 1), obtener(tableroSumado, tableroSumado.length), bloqueoEjeX, true, vidas, modo)

            } else {
              juego(
                rellenarTab(moverTodoAbajo(reverse(quitarHasta(tableroSumado, 2)), columnas, columnas), casillas, dificultad),
                columnas, dificultad, casillas, obtener(tableroSumado, tableroSumado.length - 1), obtener(tableroSumado, tableroSumado.length), false, false, vidas, modo)
            }
          } else {
            juego(
              rellenarTab(moverTodoAbajo(reverse(quitarHasta(tableroSumado, 2)), columnas, columnas), casillas, dificultad),
              columnas, dificultad, casillas, obtener(tableroSumado, tableroSumado.length - 1), obtener(tableroSumado, tableroSumado.length), false, false, vidas, modo)
          }
        }
        case ('e' | 'E') => println("¡Hasta la próxima!")
        case default => {
          println("Direccion imposible!")
          juego(tablero, columnas, dificultad, casillas, puntuacion, conteo, bloqueoEjeX, bloqueoEjeY, vidas, modo)
        }
      }
    }
  }

  def nuevaPartida(dificultad: Int, vidas: Int, puntuacion: Int, modo: Boolean): Unit = {
    if (vidas > 0) {

      dificultad match {
        case 1 => {
          val tam = 4
          val tableroRelleno = rellenarTab(generarTab(tam * tam), 2, dificultad)
          val casillas = 10
          juego(tableroRelleno, tam, dificultad, casillas, puntuacion, 0, false, false, vidas, modo)
        }
        case 2 => {
          val tam = 9
          val tableroRelleno = rellenarTab(generarTab(tam * tam), 4, dificultad)
          val casillas = 3
          juego(tableroRelleno, tam, dificultad, casillas, puntuacion, 0, false, false, vidas, modo)
        }
        case 3 => {
          val tam = 14
          val tableroRelleno = rellenarTab(generarTab(tam * tam), 6, dificultad)
          val casillas = 5
          juego(tableroRelleno, tam, dificultad, casillas, puntuacion, 0, false, false, vidas, modo)
        }
        case 4 => {
          val tam = 17
          val tableroRelleno = rellenarTab(generarTab(tam * tam), 6, dificultad)
          val casillas = 6
          juego(tableroRelleno, tam, dificultad, casillas, puntuacion, 0, false, false, vidas, modo)
        }
      }
    } else {
      println("Has perdido todas tus vidas :(\n")
      println("Puntuacion final: " + puntuacion)
    }

  }

  def movimiento(modo: Boolean): Char = {
    if (modo) {
      scala.io.StdIn.readChar()

    } else {
      val random = util.Random;
      (random.nextInt(4) + 1) match {
        case 1 => 'a'
        case 2 => 'w'
        case 3 => 's'
        case 4 => 'd'
      }
    }
  }

  //------------------------------- PRUEBAS ----------------------------------------------

  def main(args: Array[String]) {
    println("¡Bienvenido al juego 16384!\n")
    println("¿Que modo desea? (m:manual/a:automatico)\n")
    val modo = scala.io.StdIn.readChar()
    println("¿Qué dificultad desea?\n")
    val dificultad = scala.io.StdIn.readInt()
    dificultad match {
      case 1 => {
        val tam = 4
        val tableroRelleno = rellenarTab(generarTab(tam * tam), 2, dificultad)
        val casillas = 1
        juego(tableroRelleno, tam, dificultad, casillas, 0, 0, false, false, 3, modo == 'm')
      }
      case 2 => {
        val tam = 9
        val tableroRelleno = rellenarTab(generarTab(tam * tam), 4, dificultad)
        val casillas = 3
        juego(tableroRelleno, tam, dificultad, casillas, 0, 0, false, false, 3, modo == 'm')
      }
      case 3 => {
        val tam = 14
        val tableroRelleno = rellenarTab(generarTab(tam * tam), 6, dificultad)
        val casillas = 5
        juego(tableroRelleno, tam, dificultad, casillas, 0, 0, false, false, 3, modo == 'm')
      }
      case 4 => {
        val tam = 17
        val tableroRelleno = rellenarTab(generarTab(tam * tam), 6, dificultad)
        val casillas = 6
        juego(tableroRelleno, tam, dificultad, casillas, 0, 0, false, false, 3, modo == 'm')
      }
    }

  }

}
