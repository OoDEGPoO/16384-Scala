object Pruebas16384 {
  println("Welcome to the Scala workshit")        //> Welcome to the Scala workshit
  // Tableros cuadrados
  var size:Int = 4                                //> size  : Int = 4
  
  var lista:List[Int] = List.fill(size * size)(0) //> lista  : List[Int] = List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  
  def imprimir(l: List[Int], s:Int): Unit = {
  	if (!l.isEmpty)
  		(l.length % s) match {
  			case 1 => {println(l.head); imprimir(l.tail, s);}
  			case _ => {print(l.head + "\t"); imprimir(l.tail, s);}
  		}
  }                                               //> imprimir: (l: List[Int], s: Int)Unit
  
  imprimir(lista, size)                           //> 0	0	0	0
                                                  //| 0	0	0	0
                                                  //| 0	0	0	0
                                                  //| 0	0	0	0
  
  def poner(pos:Int, n:Int, l:List[Int]):List[Int] ={
  	if (l.isEmpty) Nil
  	else
  		if (pos == 1)
  			if (l.head == 0) n::l.tail
  			else l
  		else
  			l.head::poner(pos-1, n, l.tail)
  }                                               //> poner: (pos: Int, n: Int, l: List[Int])List[Int]
  
  def introFicha(x:Int, y:Int, n:Int, l:List[Int], s:Int):List[Int] = {
  	if ( x < 0 && x > s && y < 0 && y > s ) l
  	else poner(x+((y-1)*s), n, l)
  }                                               //> introFicha: (x: Int, y: Int, n: Int, l: List[Int], s: Int)List[Int]
  
  lista = introFicha(3, 2, 4, lista, size)
  
  imprimir(lista, size)                           //> 0	0	0	0
                                                  //| 0	0	4	0
                                                  //| 0	0	0	0
                                                  //| 0	0	0	0
  println(lista)                                  //> List(0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  
  def obtener(pos:Int, lista:List[Int]): Int={
  	if (lista.isEmpty) 0
  	else if (pos==1) lista.head
  	else obtener(pos-1, lista.tail)
  }                                               //> obtener: (pos: Int, lista: List[Int])Int
  
  def getFicha(x:Int, y:Int, l:List[Int], s:Int):Int = {
  	if ( x < 0 && x > s && y < 0 && y > s ) 0
  	else obtener(x+((y-1)*s), l)
  }                                               //> getFicha: (x: Int, y: Int, l: List[Int], s: Int)Int
  
  println(getFicha(2, 3, lista, size) + " - " + getFicha(3, 2, lista, size))
                                                  //> 0 - 4
  
  def movIzq(l:List[Int], s:Int):List[Int] = {
  	if (l.isEmpty) Nil
  	else
  		if ((l.head == 0) || ((l.length % s) == 1)) l.head::movIzq(l.tail, s)
  		else
  			if (obtener(2, l) == 0) 0::movIzq(l.head::l.tail.tail, s)
  			else l.head::movIzq(l.tail, s)
  }                                               //> movIzq: (l: List[Int], s: Int)List[Int]
  
  lista = introFicha(2, 3, 2, lista, size)
  lista = introFicha(1, 2, 8, lista, size)
  
  imprimir(lista, size)                           //> 0	0	0	0
                                                  //| 8	0	4	0
                                                  //| 0	2	0	0
                                                  //| 0	0	0	0
  lista = movIzq(lista, size)
  imprimir(lista, size)                           //> 0	0	0	0
                                                  //| 0	8	0	4
                                                  //| 0	0	0	2
                                                  //| 0	0	0	0
}