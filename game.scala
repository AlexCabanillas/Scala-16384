
/*====================================================================================
|                 Luis Alejandro Cabanillas Prudencio  DNI: 04236930P                |
|                    Álvaro de las Heras Fernández  DNI: 03146833L                   |
|                                                                                    |
|         16384 Juego que simula al 2048 implementado con Scala                      |
====================================================================================*/

object game {

  //------------------------------FUNCIONES GENERICAS---------------------------------

  /**
    * Obtiene el valor de una posicion del tablero
    * @param tablero tablero en el que buscara el valor
    * @param indice posicion en el tablero de la que se obtendra el valor
    * @return Si existe el valor lo devuelve si no devuelve -1
    */
  def obtener(tablero: List[Int], indice: Int): Int = {
    //Si el tablero no esta vacio
    if (tablero.length>0) {
      //Si el indice es la cabeza se devuelve
      if (indice == 1) tablero.head
      //Si no se quita otro elemento y se reduce el indice
      else obtener(tablero.tail, indice - 1)
      //Si esta fuera de rango devuelve -1
    } else -1
  }

  /**
    * Genera un valor aleatorio de un determinado conjunto
    * @param dificultad Parametro que determinara los posibles valores a generar
    * @return Valor generado aleatoriamente para la dificultad dada
    */
  def crearValorRandom(dificultad: Int): Int = {
    //Conjunto de posibles valores a generar
    val valores = List(2,4,8)
    val random = util.Random;
    //Segun la dificultad coge unos u otros elementos del conjunto
    if (dificultad < 4) {
      val valor = obtener(valores, random.nextInt(dificultad) + 1)
      valor
    } else {
      val valor = obtener(valores, random.nextInt(3) + 1)
      valor
    }
  }

  /**
    * Genera una posicion hasta el tamano maximo
    * @param tam tamano que no debe sobrepasar
    * @return posicion aleatoria dentro del rango
    */
  def crearRandomPos(tam: Int): Int = {
    val random = util.Random;
    //Valor aleatorio que se genera
    val pos = random.nextInt(tam) + 1
    pos
  }
  /**
    * Pone un valor dado en una lista en la posicion dada
    * @param lista lista en la que se pondra el valor
    * @param valor valor que se introducira
    * @param pos posicion en la que se introducira
    * @return lista con el valor introducido
    */
  def poner(lista: List[Int], valor: Int, pos: Int): List[Int] = {
    //Comprueba si esta vacia
    if (lista.length==0) Nil
    //Si es la primera posicion se anade al principio
    else if (pos == 1) valor :: lista.tail
    //Si no se itera hasta llegar a ella
    else lista.head :: poner(lista.tail, valor, pos - 1)
  }
//POSIBLE MEJORA
  /**
    * Pone un elemento en una posicion vacia con un valor aleatorio que depende de la dificultad
    * @param lista lista o tablero donde se pondran los valores
    * @param dificultad parametro que condiciona el valor a introducir
    * @return el tablero con el valor introducido
    */
  def poner(lista: List[Int], dificultad: Int): List[Int] = {
    //Se genera una posicion y un valor aleatorios
    val pos = crearRandomPos(lista.length)
    val valor = crearValorRandom(dificultad)
    //Comprueba si esta vacia
    if (lista.length==0) Nil
    else{
      //Si la posicion esta vacia
      if (obtener(lista, pos) == 0)
        //Coloca el valor en ella
        poner(lista,valor,pos)
      else poner(lista, dificultad)
    }
  }


  /**
    * Genera un tablero relleno de ceros (vectorizado)
    * @param tam tamano que tendra el tablero
    * @return tablero creado lleno de 0
    */
  def generarTab(tam: Int): List[Int] = {
    if (tam == 0) Nil
    //Llamada recursiva hasta que el tamano sea el deseado
    else 0 :: generarTab(tam - 1)
  }

  /**
    *Cuenta el numero de huecos que hay (posiciones a 0)
    * @param tablero tablero en el que se contaran
    * @return numero de huecos del tablero
    */
  def huecosLibres(tablero: List[Int]): Int = {
    //Si el tablero no es vacio contamos
    if (tablero.length > 0) {
      //Cada vez que haya una celda vacia se suma uno y se llama recursivamente
      if (tablero.head == 0)
        huecosLibres(tablero.tail) + 1
      else
      //Si no es 0 se llama recursivamente sin contar
        huecosLibres(tablero.tail)
    } else 0
  }

  /**
    * Rellena el tablero con un numero de semillas segun una dificultad
    * @param tablero tablero que sera rellenado
    * @param numCasillas cantidad de casillas a rellenar
    * @param dificultad dificultad que condicionara los valores de relleno
    * @return el tablero ya relleno
    */
  def rellenarTab(tablero: List[Int], numCasillas: Int, dificultad: Int): List[Int] = {
    if (tablero.length==0) Nil
    //Mientras haya huecos y casillas se rellena el tablero
    else if ((huecosLibres(tablero) > 0) && (numCasillas != 0)) { rellenarTab(poner(tablero, dificultad), numCasillas - 1, dificultad)}
    //Finalmente devuelve el tablero
    else tablero
  }

  /**
    *Imprime el tablero con su formato adecuado
    * @param lista tablero que se va a mostrar por pantalla
    * @param columnas columnas que tendra cada fila
    */
  def imprimir(lista: List[Int], columnas: Int):Unit={
    if (lista.length>0) {
      //Cuando llega a la ultima columna pasa a la siguiente fila
      if (lista.length % columnas == 0) print("\n|")
      //Espacios que corrigen visualmente los valores del tablero
      val corregirEspacios = espacios(lista.head)
      //Imprime los valores del tablero con los espacios ajustados
      print(corregirEspacios + lista.head+ "|")
      //Se vuelve a llamar a imprimir
      imprimir(lista.tail, columnas)
    }
  }

  /**
    * Espacios generados para cada valor
    * @param numero valor del que dependen los espacios
    * @return espacios que ajustaran el valor
    */
  def espacios(numero:Int):String={
    " "*(7-digitos(numero))
  }

  /**
    * Calcula la cantidad de digitos del numero
    * @param numero numero que se comprobara
    * @return digitos del numero
    */
  def digitos(numero:Int):Int={
    //Si es menor que la condicion devuelve los digitos correspondientes hasta 7
    if (numero<10){1}
    else if (numero<100){2}
    else if (numero<1000){3}
    else if (numero<10000){4}
    else if (numero<100000){5}
    else if (numero<1000000){6}
    else {7}
  }

  /**
    * Elimina el valor de la posicion dada del tablero
    * @param tablero tablero en el que se eliminara la posicion
    * @param indice posicion que sera eliminada
    * @return tablero con la posicion eliminada
    */
  def eliminar(tablero: List[Int], indice: Int): List[Int] = {
   //Comprueba si no esta vacio
    if (tablero.length>0) {
      //Comprueba si es la cabeza para anadir 0
      if (indice == 1) 0 :: tablero.tail
      //Si no vuelve a buscarlo recursivamente
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
