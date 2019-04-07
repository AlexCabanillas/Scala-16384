object prueba2 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
    def crearRandomCol ():Int= {
  	val random = util.Random;
  	val colo = random.nextInt(8)+1
  	colo
  
  
  }                                               //> crearRandomCol: ()Int
  def crearRandomPos():Int={
  	val random = util.Random;
  	val pos = random.nextInt(8)+1
  	pos
  }                                               //> crearRandomPos: ()Int
def poner(l:List[Int]):	List[Int]=	{
	val pos = crearRandomPos()
	val col = crearRandomCol()
	if	(l.isEmpty)	Nil
	else	if	(pos==1)	col::l.tail
	else	l.head::poner(l.tail)	}         //> poner: (l: List[Int])List[Int]
	
	def poner2(l:List[Int],valor:Int,pos:Int):	List[Int]=	{
	if	(l.isEmpty)	Nil
	else	if	(pos==1)	valor::l.tail
	else	l.head::poner2(l.tail,valor,pos-1)	}
                                                  //> poner2: (l: List[Int], valor: Int, pos: Int)List[Int]
	
def generarTab(filas: Int, columnas: Int,tam: Int): List[Int]= {
	val tablero= List[Int]()
	if(tam==0) Nil
	else 0::generarTab(filas,columnas,tam-1)

}                                                 //> generarTab: (filas: Int, columnas: Int, tam: Int)List[Int]
  generarTab(3,3,9)                               //> res0: List[Int] = List(0, 0, 0, 0, 0, 0, 0, 0, 0)
  
 	def rellenarTab(lista: List[Int], numCasillas: Int): List[Int] = {
  	if (lista.isEmpty) Nil
  	else if (numCasillas !=0 ) rellenarTab(poner(lista),numCasillas-1)
  
  	else lista
  }                                               //> rellenarTab: (lista: List[Int], numCasillas: Int)List[Int]
  
def imprimir(lista:List[Int],columnas: Int, filas: Int){
	
	if (!lista.isEmpty){
	if (lista.length%columnas==0) println()
	print("|"+lista.head+"|")
	imprimir(lista.tail,columnas,filas)
	
	}
	
}                                                 //> imprimir: (lista: List[Int], columnas: Int, filas: Int)Unit
  val tablero =generarTab(3,3,9)                  //> tablero  : List[Int] = List(0, 0, 0, 0, 0, 0, 0, 0, 0)
  val tableroRelleno=rellenarTab(tablero,5)       //> tableroRelleno  : List[Int] = List(0, 5, 6, 1, 0, 0, 0, 8, 0)
  imprimir(tableroRelleno,3,3)                    //> 
                                                  //| |0||5||6|
                                                  //| |1||0||0|
                                                  //| |0||8||0|
  
  /*def pruebaImp(){
  print("g")
  }*/
  
  //Obtiene el valor de una posicion indice del tablero
  def obtener(tablero:List[Int], indice:Int): Int = {
  if (!tablero.isEmpty){
  	if (indice==1) tablero.head
  	else obtener(tablero.tail,indice-1)
  	}else -1
  
  }                                               //> obtener: (tablero: List[Int], indice: Int)Int
  
  obtener(tableroRelleno,1)                       //> res1: Int = 0
  obtener(tableroRelleno,2)                       //> res2: Int = 5
  obtener(tableroRelleno,3)                       //> res3: Int = 6
  obtener(tableroRelleno,4)                       //> res4: Int = 1
  obtener(tableroRelleno,5)                       //> res5: Int = 0
  obtener(tableroRelleno,15)                      //> res6: Int = -1
  
  def eliminarSumar(tablero:List[Int],indice:Int): List[Int] = {
   if (!tablero.isEmpty){
  	if (indice==1) 0::tablero.tail
  	else tablero.head::eliminarSumar(tablero.tail,indice-1)
  	}else tablero
  }                                               //> eliminarSumar: (tablero: List[Int], indice: Int)List[Int]
  eliminarSumar(tableroRelleno,1)                 //> res7: List[Int] = List(0, 5, 6, 1, 0, 0, 0, 8, 0)
  eliminarSumar(tableroRelleno,2)                 //> res8: List[Int] = List(0, 0, 6, 1, 0, 0, 0, 8, 0)
  eliminarSumar(tableroRelleno,3)                 //> res9: List[Int] = List(0, 5, 0, 1, 0, 0, 0, 8, 0)
  eliminarSumar(tableroRelleno,4)                 //> res10: List[Int] = List(0, 5, 6, 0, 0, 0, 0, 8, 0)
  eliminarSumar(tableroRelleno,5)                 //> res11: List[Int] = List(0, 5, 6, 1, 0, 0, 0, 8, 0)
  
  /*def movimiento(tablero:List[Int],columnas:Int,direccion:Int): List[Int]={
  //Movimiento hacia abajo
  if (direccion==1) {
   if(!tablero.isEmpty){
  	if (tablero.head>0){
  		if(obtener(tablero,columnas+1)==0)  movimiento(poner2(eliminarSumar(tablero,1),tablero.head,columnas+1),columnas,direccion)
  		else tablero.head:: movimiento(tablero.tail,columnas,direccion)
  	
  	}else tablero.head::movimiento(tablero.tail,columnas,direccion)
  	} else tablero
  //Movimiento hacia arriba
  }
  
  } else tablero.head::movimiento(tablero.tail,columnas,direccion)
  }
  
  imprimir(tableroRelleno,3,3)
 
   
  imprimir(movimiento(tableroRelleno,3,1),3,3)
 */
 /*def moverTodo(tablero: List[Int],filas: Int,columnas:Int,direccion:Int): List[Int]={
 if(filas==1) tablero
 else moverTodo(movimiento(tablero,columnas,direccion),filas-1,columnas,direccion)
 }
 imprimir(moverTodo(tableroRelleno,3,3,2),3,3)
 */
 
 def reverse(lista: List[Int]): List[Int] ={
 if (lista.length==0) lista
 else reverse(lista.tail):::lista.head::Nil
 
 }                                                //> reverse: (lista: List[Int])List[Int]
 
 def moverArriba(tablero:List[Int],columnas:Int): List[Int]={
 val tablero_aux=reverse(tablero)
 if(!tablero_aux.isEmpty){
  	if (tablero_aux.head>0){
  		if(obtener(tablero_aux,columnas+1)==0)  moverArriba(poner2(eliminarSumar(tablero_aux,1),tablero_aux.head,columnas+1),columnas)
  		else tablero_aux.head:: moverArriba(tablero_aux.tail,columnas)
  	
  	}else tablero.head::moverArriba(tablero.tail,columnas)
 }else tablero_aux
 }                                                //> moverArriba: (tablero: List[Int], columnas: Int)List[Int]
 
 def moverAbajo(tablero:List[Int],columnas:Int): List[Int]={
   if(!tablero.isEmpty){
  	if (tablero.head>0){
  		if(obtener(tablero,columnas+1)==0)  moverAbajo(poner2(eliminarSumar(tablero,1),tablero.head,columnas+1),columnas)
  		else tablero.head:: moverAbajo(tablero.tail,columnas)
  	
  	}else tablero.head::moverAbajo(tablero.tail,columnas)
  	} else tablero
 }                                                //> moverAbajo: (tablero: List[Int], columnas: Int)List[Int]
 
 imprimir(tableroRelleno,3,3)                     //> 
                                                  //| |0||5||6|
                                                  //| |1||0||0|
                                                  //| |0||8||0|
 
 
 
 
   
 imprimir(moverArriba(tableroRelleno,3),3,3)      //> 
                                                  //| |0||5||6|
                                                  //| |1||0||0|
                                                  //| |0||8||0|
  
  
  
  
  
 imprimir(tableroRelleno,3,3)                     //> 
                                                  //| |0||5||6|
                                                  //| |1||0||0|
                                                  //| |0||8||0|
 
 
 
   
 imprimir(moverAbajo(tableroRelleno,3),3,3)       //> 
                                                  //| |0||0||0|
                                                  //| |0||5||0|
                                                  //| |1||8||6|
 
 
 
 def pruebaImp(){
  print("g")
  }                                               //> pruebaImp: ()Unit
  
}