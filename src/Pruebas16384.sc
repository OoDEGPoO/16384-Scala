object Pruebas16384 {
  println("Welcome to the Scala workshit")        //> Welcome to the Scala workshit
  // Tableros cuadrados
  var size:Int = 4                                //> size  : Int = 4
  
  def rellenaLista(long:Int)(valor:Int):List[Int] = {
  	if (long == 0) Nil
  	else valor::rellenaLista(long-1)(valor)
  }                                               //> rellenaLista: (long: Int)(valor: Int)List[Int]
  
  def rellenaTablero(s:Int):List[Int] = {
  	rellenaLista(s * s)(0)
  }                                               //> rellenaTablero: (s: Int)List[Int]
  
  def equivalentes(m:List[Int])(n:List[Int]):Boolean ={
  	if (m.length != n.length) false
  	else
  		if (m.isEmpty) true
  		else
  			if (m.head == n.head) true && equivalentes(m.tail)(n.tail)
  			else false
  }                                               //> equivalentes: (m: List[Int])(n: List[Int])Boolean
  
  var lista:List[Int] = rellenaTablero(size)      //> lista  : List[Int] = List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  
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
  
  def setLista(pos:Int, n:Int, l:List[Int]):List[Int] ={
  	if (l.isEmpty) Nil
  	else
  		if (pos == 1)
  			n::l.tail
  		else
  			l.head::poner(pos-1, n, l.tail)
  }                                               //> setLista: (pos: Int, n: Int, l: List[Int])List[Int]
  
  //Como setLista pero comprobando que se pueda introducir
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
  
  /*def voltHorario(l:List[Int], s:Int):List[Int] = {
  	
  }*/
  
  println(getFicha(2, 3, lista, size) + " - " + getFicha(3, 2, lista, size))
                                                  //> 0 - 4

	//Cuantas posiciones a la derecha de X está el siguiente equivalente
	def buscaDch(l:List[Int], s:Int, x:Int, pos:Int):Int = {
		if (l.isEmpty || (l.length % s == 0)) 0 //Si está vacío o si se busca ya en la siguiente fila
		else
			if (l.head == x) pos + 1
			else
				if (l.head == 0) buscaDch(l.tail, s, x, pos + 1)
				else 0
	}                                         //> buscaDch: (l: List[Int], s: Int, x: Int, pos: Int)Int

	//Recorre la lista realizando posibles sumas
  def sumDch(l:List[Int], s:Int):List[Int] = {
  	var pos:Int = 0
  	
  	if (l.isEmpty) Nil
  	else
  		if (l.head == 0) l.head::sumDch(l.tail, s)
  		else {
  			pos = buscaDch(l.tail, s, l.head, 0);
  			if (pos == 0) l.head::sumDch(l.tail, s)
  			else (l.head*2)::sumDch(setLista(pos, 0, l.tail), s)
  		}
  }                                               //> sumDch: (l: List[Int], s: Int)List[Int]
  
  def movDch(l:List[Int], s:Int):List[Int] = {
  	if (l.isEmpty) Nil
  	else
  		if ((l.head == 0) || ((l.length % s) == 1)) l.head::movDch(l.tail, s)
  		else
  			if (l.tail.head == 0) 0::movDch(l.head::l.tail.tail, s)
  			else l.head::movDch(l.tail, s)
  }                                               //> movDch: (l: List[Int], s: Int)List[Int]
  
  def derecha(l:List[Int], s:Int):List[Int] = {
  	var l0:List[Int] = l
  	var l1:List[Int] = l
  	
  	l1 = sumDch(l0, s);
  	
  	do {
  		l0 = l1;
  		l1 = movDch(l0, s);
  	} while(!(equivalentes(l0)(l1)));
  	
  	l1
  }                                               //> derecha: (l: List[Int], s: Int)List[Int]
  
  def movIzq(l:List[Int], s:Int):List[Int] = {
  	if (l.isEmpty) Nil
  	else
  		if ((l.last == 0) || ((l.length % s) == 1)) movIzq(l.init, s):::l.last::Nil
  		else
  			if (l.init.last == 0) movIzq(l.init.init:::l.last::Nil, s):::0::Nil
  			else movIzq(l.init, s):::l.last::Nil
  }                                               //> movIzq: (l: List[Int], s: Int)List[Int]
  
  def izquierda(l:List[Int], s:Int):List[Int] = {
  	var l0:List[Int] = l
  	var l1:List[Int] = l
  	
  	do {
  		l0 = l1;
  		l1 = movIzq(l0, s);
  	} while(!(equivalentes(l0)(l1)));
  	
  	l1
  }                                               //> izquierda: (l: List[Int], s: Int)List[Int]
  
  lista = introFicha(2, 3, 2, lista, size)
  lista = introFicha(1, 2, 8, lista, size)
  lista = introFicha(1, 1, 2, lista, size)
	lista = introFicha(3, 1, 2, lista, size)
    

  imprimir(lista, size)                           //> 2	0	2	0
                                                  //| 8	0	4	0
                                                  //| 0	2	0	0
                                                  //| 0	0	0	0
  lista = derecha(lista, size)
  imprimir(lista, size)                           //> 0	0	4	2
                                                  //| 0	0	8	4
                                                  //| 0	0	0	2
                                                  //| 0	0	0	0
  lista = izquierda(lista, size)
  imprimir(lista, size)                           //> 4	2	0	0
                                                  //| 8	4	0	0
                                                  //| 2	0	0	0
                                                  //| 0	0	0	0
}