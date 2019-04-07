/*====================================================================================
|                 Luis Alejandro Cabanillas Prudencio  DNI: 04236930P                |
|                    Álvaro de las Heras Fernández  DNI: 03146833L                   |
|                                                                                    |
|         16384 Juego que simula al 2048 implementado con Scala                      |
====================================================================================*/

//------------------------------FUNCIONES GENERICAS---------------------------------
object prueba2 {
//Obtiene el valor de una posicion indice del tablero
def obtener(tablero: List[Int], indice: Int): Int = {
  if (!tablero.isEmpty) {
    if (indice == 1) tablero.head
    else obtener(tablero.tail, indice - 1)
  } else -1
}
def crearValorRandom(): Int = {
  val list = List(2,4)
  val random = util.Random;
  val colo = list(random.nextInt(2))
  colo
}

def crearRandomPos(tam:Int): Int = {
  val random = util.Random;
  val pos = random.nextInt(tam) + 1
  pos
}

def poner(lista: List[Int]): List[Int] = {
  val pos = crearRandomPos(lista.length)
  val col = crearValorRandom()
  if (lista.isEmpty) Nil
    if (obtener(lista,pos)==0)
      if (pos == 1) col :: lista.tail
      else lista.head :: poner(lista.tail)
  else poner(lista)
}

def poner2(l: List[Int], valor: Int, pos: Int): List[Int] = {
  if (l.isEmpty) Nil
  else if (pos == 1) valor :: l.tail
  else l.head :: poner2(l.tail, valor, pos - 1)
}

def generarTab(filas: Int, columnas: Int, tam: Int): List[Int] = {
  if (tam == 0) Nil
  else 0 :: generarTab(filas, columnas, tam - 1)
}

def huecosLibres(tablero:List[Int]):Int = {
  if (tablero.length > 0){
    if (tablero.head == 0)
      huecosLibres(tablero.tail)+1
    else
      huecosLibres(tablero.tail)
  } else 0
}
def rellenarTab(tablero: List[Int], numCasillas: Int): List[Int] = {
  if (tablero.isEmpty) Nil
  else if ((huecosLibres(tablero)>0) && (numCasillas != 0)){ rellenarTab(poner(tablero), numCasillas - 1)}
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
        if (obtener(tablero_aux,2) == 0) reverse(moverDerecha(poner2(eliminarSumar(tablero_aux, 1), tablero_aux.head,2), columnas))
        else reverse(tablero_aux.head :: moverDerecha(tablero_aux.tail, columnas))

      } else reverse(tablero_aux.head :: moverDerecha(tablero_aux.tail, columnas))
    } else tablero_aux
  }

//------------------------ MOVIMIENTO DE TODAS LAS CASILLAS-------------------------------

def moverTodoAbajo(tablero: List[Int], filas: Int, columnas: Int): List[Int] = {
  if (filas == 1) tablero
  else moverTodoAbajo(moverAbajo(tablero, columnas), filas - 1, columnas)
}
def moverTodoArriba(tablero: List[Int], filas: Int, columnas: Int): List[Int] = {
  if (filas == 1) tablero
  else moverTodoArriba(moverArriba(tablero, columnas), filas - 1, columnas)
}
def moverTodoDerecha(tablero: List[Int], filas: Int, columnas: Int): List[Int] = {
  if (filas == 1) tablero
  else moverTodoDerecha(moverDerecha(tablero, columnas), filas - 1, columnas)
}
def moverTodoIzquierda(tablero: List[Int], filas: Int, columnas: Int): List[Int] = {
  if (filas == 1) tablero
  else moverTodoIzquierda(moverIzquierda(tablero, columnas), filas - 1, columnas)
}

def sumarIzquierda(tablero: List[Int],columnas:Int):List[Int]={
  if (!tablero.isEmpty) {
    if ((tablero.head > 0)) {
      if ((obtener(tablero, 2) == tablero.head) && ((tablero.length) % columnas != 1)) sumarIzquierda(poner2(eliminarSumar(tablero, 2), tablero.head*2, 1), columnas)
      else tablero.head :: sumarIzquierda(tablero.tail, columnas)
    } else tablero.head :: sumarIzquierda(tablero.tail, columnas)
  } else tablero
}
def sumarArriba(tablero: List[Int],columnas:Int):List[Int]={
  if (!tablero.isEmpty) {
    if ((tablero.head > 0)) {
      if (obtener(tablero, columnas + 1) == tablero.head) sumarArriba(poner2(eliminarSumar(tablero, columnas + 1), tablero.head*2, 1), columnas)
      else tablero.head :: sumarArriba(tablero.tail, columnas)

    } else tablero.head :: sumarArriba(tablero.tail, columnas)
  } else tablero
}

//------------------------------- PRUEBAS ----------------------------------------------

val filas = 6
val columnas = 6
val tam = filas * columnas

val tablero = generarTab(6, 6, tam)
val tableroRelleno = rellenarTab(tablero, 16)

imprimir(tableroRelleno, 6)

imprimir(moverTodoDerecha(reverse(sumarIzquierda(reverse(moverTodoDerecha(tableroRelleno, filas,columnas)),columnas)),filas ,columnas), columnas)

imprimir(moverTodoIzquierda(sumarIzquierda(moverTodoIzquierda(tableroRelleno,filas ,columnas),columnas),filas ,columnas), columnas)

imprimir(moverTodoArriba(sumarArriba(moverTodoArriba(tableroRelleno,filas ,columnas),columnas),filas ,columnas), columnas)

imprimir(moverTodoAbajo(reverse(sumarArriba(reverse(moverTodoAbajo(tableroRelleno,filas ,columnas)),columnas)),filas ,columnas), columnas)
}
