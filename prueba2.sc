/*====================================================================================
|                 Luis Alejandro Cabanillas Prudencio  DNI: 04236930P                |
|                    Álvaro de las Heras Fernández  DNI: 03146833L                   |
|                                                                                    |
|         16384 Juego que simula al 2048 implementado con matrices y CUDA            |
====================================================================================*/
object prueba2{
//------------------------------FUNCIONES GENERICAS---------------------------------

def crearRandomCol(): Int = {
  val random = util.Random;
  val colo = random.nextInt(8) + 1
  colo
}                                                 //> crearRandomCol: ()Int
def crearRandomPos(): Int = {
  val random = util.Random;
  val pos = random.nextInt(8) + 1
  pos
}                                                 //> crearRandomPos: ()Int
def poner(l: List[Int]): List[Int] = {
  val pos = crearRandomPos()
  val col = crearRandomCol()
  if (l.isEmpty) Nil
  else if (pos == 1) col :: l.tail
  else l.head :: poner(l.tail)
}                                                 //> poner: (l: List[Int])List[Int]

def poner2(l: List[Int], valor: Int, pos: Int): List[Int] = {
  if (l.isEmpty) Nil
  else if (pos == 1) valor :: l.tail
  else l.head :: poner2(l.tail, valor, pos - 1)
}                                                 //> poner2: (l: List[Int], valor: Int, pos: Int)List[Int]

def generarTab(filas: Int, columnas: Int, tam: Int): List[Int] = {
  val tablero = List[Int]()
  if (tam == 0) Nil
  else 0 :: generarTab(filas, columnas, tam - 1)

}                                                 //> generarTab: (filas: Int, columnas: Int, tam: Int)List[Int]

def rellenarTab(lista: List[Int], numCasillas: Int): List[Int] = {
  if (lista.isEmpty) Nil
  else if (numCasillas != 0) rellenarTab(poner(lista), numCasillas - 1)
  else lista
}                                                 //> rellenarTab: (lista: List[Int], numCasillas: Int)List[Int]

def imprimir(lista: List[Int], columnas: Int) {
  if (!lista.isEmpty) {
    if (lista.length % columnas == 0) println()
    print("|" + lista.head + "|")
    imprimir(lista.tail, columnas)
  }
}                                                 //> imprimir: (lista: List[Int], columnas: Int)Unit

//Obtiene el valor de una posicion indice del tablero
def obtener(tablero: List[Int], indice: Int): Int = {
  if (!tablero.isEmpty) {
    if (indice == 1) tablero.head
    else obtener(tablero.tail, indice - 1)
  } else -1

}                                                 //> obtener: (tablero: List[Int], indice: Int)Int
def eliminarSumar(tablero: List[Int], indice: Int): List[Int] = {
  if (!tablero.isEmpty) {
    if (indice == 1) 0 :: tablero.tail
    else tablero.head :: eliminarSumar(tablero.tail, indice - 1)
  } else tablero
}                                                 //> eliminarSumar: (tablero: List[Int], indice: Int)List[Int]


//--------------------------FUNCIONES AUXILIARES DE MOVIMIENTO--------------------------

def reverse(lista: List[Int]): List[Int] = {
  if (lista.length == 0) lista
  else reverse(lista.tail) ::: lista.head :: Nil
}                                                 //> reverse: (lista: List[Int])List[Int]

//------------------------   MOVIMIENTOS   -------------------------------

def moverAbajo(tablero: List[Int], columnas: Int): List[Int] = {
  if (!tablero.isEmpty) {
    if (tablero.head > 0) {
      if (obtener(tablero, columnas + 1) == 0) moverAbajo(poner2(eliminarSumar(tablero, 1), tablero.head, columnas + 1), columnas)
      else tablero.head :: moverAbajo(tablero.tail, columnas)

    } else tablero.head :: moverAbajo(tablero.tail, columnas)
  } else tablero
}                                                 //> moverAbajo: (tablero: List[Int], columnas: Int)List[Int]

def moverArriba(tablero: List[Int], columnas: Int): List[Int] = {
  val tablero_aux = reverse(tablero)
  if (!tablero_aux.isEmpty) {
    if (tablero_aux.head > 0) {
      if (obtener(tablero_aux, columnas + 1) == 0) reverse(moverAbajo(poner2(eliminarSumar(tablero_aux, 1), tablero_aux.head, columnas + 1), columnas))
      else reverse(tablero_aux.head :: moverAbajo(tablero_aux.tail, columnas))

    } else reverse(tablero_aux.head :: moverAbajo(tablero_aux.tail, columnas))
  } else tablero_aux
}                                                 //> moverArriba: (tablero: List[Int], columnas: Int)List[Int]

//------------------------ MOVIMIENTO DE TODAS LAS CASILLAS-------------------------------

def moverTodo(tablero: List[Int], filas: Int, columnas: Int, direccion: Int): List[Int] = {
  if (filas == 1) tablero
  else moverTodo(moverAbajo(tablero, columnas), filas - 1, columnas, direccion)
}                                                 //> moverTodo: (tablero: List[Int], filas: Int, columnas: Int, direccion: Int)L
                                                  //| ist[Int]
def moverTodoArriba(tablero: List[Int], filas: Int, columnas: Int, direccion: Int): List[Int] = {
  if (filas == 1) tablero
  else moverTodoArriba(moverArriba(tablero, columnas), filas - 1, columnas, direccion)
}                                                 //> moverTodoArriba: (tablero: List[Int], filas: Int, columnas: Int, direccion:
                                                  //|  Int)List[Int]

//------------------------------- PRUEBAS ----------------------------------------------

val filas = 6                                     //> filas  : Int = 6
val columnas = 6                                  //> columnas  : Int = 6
val tam = filas*columnas                          //> tam  : Int = 36

val tablero = generarTab(6, 6, tam)               //> tablero  : List[Int] = List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
val tableroRelleno = rellenarTab(tablero, 9)      //> tableroRelleno  : List[Int] = List(6, 0, 3, 1, 2, 1, 1, 0, 0, 0, 0, 6, 0, 2
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

imprimir(tableroRelleno, 6)                       //> 
                                                  //| |6||0||3||1||2||1|
                                                  //| |1||0||0||0||0||6|
                                                  //| |0||2||0||0||0||0|
                                                  //| |0||0||0||0||0||0|
                                                  //| |0||0||0||0||0||0|
                                                  //| |0||0||0||0||0||0|

imprimir(moverArriba(tableroRelleno, 6), 6)       //> 
                                                  //| |6||2||3||1||2||1|
                                                  //| |1||0||0||0||0||6|
                                                  //| |0||0||0||0||0||0|
                                                  //| |0||0||0||0||0||0|
                                                  //| |0||0||0||0||0||0|
                                                  //| |0||0||0||0||0||0|

imprimir(moverTodoArriba(tableroRelleno, 6, 6, 1), 6)
                                                  //> 
                                                  //| |6||2||3||1||2||1|
                                                  //| |1||0||0||0||0||6|
                                                  //| |0||0||0||0||0||0|
                                                  //| |0||0||0||0||0||0|
                                                  //| |0||0||0||0||0||0|
                                                  //| |0||0||0||0||0||0|

imprimir(moverTodo(tableroRelleno, 6, 6, 1), 6)   //> 
                                                  //| |0||0||0||0||0||0|
                                                  //| |0||0||0||0||0||0|
                                                  //| |0||0||0||0||0||0|
                                                  //| |0||0||0||0||0||0|
                                                  //| |6||0||0||0||0||1|
}