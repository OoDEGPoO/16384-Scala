object Pruebas16384 {
  println("Welcome to the Scala workshit")
  // Tableros cuadrados
  var size:Int = 4
  
  def rellenaLista(long:Int)(valor:Int):List[Int] = {
  	if (long == 0) Nil
  	else valor::rellenaLista(long-1)(valor)
  }
  
  def rellenaTablero(s:Int):List[Int] = {
  	rellenaLista(s * s)(0)
  }
  
  def equivalentes(m:List[Int])(n:List[Int]):Boolean ={
  	if (m.length != n.length) false
  	else
  		if (m.isEmpty) true
  		else
  			if (m.head == n.head) true && equivalentes(m.tail)(n.tail)
  			else false
  }
  
  def setLista(pos:Int, n:Int, l:List[Int]):List[Int] ={
  	if (l.length == 0) Nil
  	else
  		if (pos == 1)
  			n::l.tail
  		else
  			l.head::poner(pos-1, n, l.tail)
  }
  
  //Como setLista pero comprobando que se pueda introducir
  def poner(pos:Int, n:Int, l:List[Int]):List[Int] ={
  	if (l.length == 0) Nil
  	else
  		if (pos == 1)
  			if (l.head == 0) n::l.tail
  			else l
  		else
  			l.head::poner(pos-1, n, l.tail)
  }
  
  def introFicha(x:Int, y:Int, n:Int, l:List[Int], s:Int):List[Int] = {
  	if ( x < 0 && x > s && y < 0 && y > s ) l
  	else poner(x+((y-1)*s), n, l)
  }
  
  def obtener(pos:Int, lista:List[Int]): Int={
  	if (lista.length == 0) 0
  	else if (pos==1) lista.head
  	else obtener(pos-1, lista.tail)
  }
  
  def getFicha(l:List[Int], s:Int)(x:Int, y:Int):Int = {
  	if ( x < 0 && x > s && y < 0 && y > s ) 0
  	else obtener(x+((y-1)*s), l)
  }
  
  def invertir(l:List[Int]):List[Int] = {
  	if (l.length == 0) Nil
  	else invertir(l.tail):::l.head::Nil
  }
  
  def transpuesta_aux(l:List[Int], s:Int)(x:Int, y:Int):List[Int] = {
  	if ((l.length == 0) || (s < y)) Nil
  	else
  		if (x == s) getFicha(l, s)(y, x)::transpuesta_aux(l, s)(1, y+1)
  		else getFicha(l, s)(y, x)::transpuesta_aux(l, s)(x+1, y)
  }
  
  def transpuesta(l:List[Int], s:Int):List[Int] = transpuesta_aux(l, s)(1, 1)
  
  var lista:List[Int] = rellenaTablero(size)
  
  def imprimir(l: List[Int], s:Int): Unit = {
  	if (!(l.length == 0))
  		(l.length % s) match {
  			case 1 => {println(l.head); imprimir(l.tail, s);}
  			case _ => {print(l.head + "\t"); imprimir(l.tail, s);}
  		}
  }
  
  imprimir(lista, size)
  
  lista = introFicha(3, 2, 4, lista, size)

/* Basurilla
	//Cuantas posiciones a la derecha de X está el siguiente equivalente
	def buscaDch(l:List[Int], s:Int, x:Int, pos:Int):Int = {
		if (l.length == 0 || (l.length % s == 0)) 0 //Si está vacío o si se busca ya en la siguiente fila
		else
			if (l.head == x) pos + 1
			else
				if (l.head == 0) buscaDch(l.tail, s, x, pos + 1)
				else 0
	}*/
  
  //Suma con el siguiente valor inmediato siempre y cuando sean equivalentes
  def suma(l:List[Int], s:Int):List[Int] = {
  	if (l.length == 0) Nil
  	else
  		if ((l.head == 0) || (l.length % s == 1)) l.head::suma(l.tail, s)
  		else
  			if (l.head == l.tail.head) l.head*2::suma(0::l.tail.tail, s)
  			else l.head::suma(l.tail, s)
  }
  
  def mover(l:List[Int], s:Int):List[Int] = {
  	if (l.length == 0) Nil
  	else
  		if ((l.head == 0) || ((l.length % s) == 1)) l.head::mover(l.tail, s)
  		else
  			if (l.tail.head == 0) 0::mover(l.head::l.tail.tail, s)
  			else l.head::mover(l.tail, s)
  }
  
  def movimiento(l:List[Int], s:Int):List[Int] = {
  	var l0:List[Int] = l
  	var l1:List[Int] = l
  	
  	do {
  		l0 = l1;
  		l1 = mover(l0, s);
  	} while(!(equivalentes(l0)(l1)));
  	
  	l1 = invertir(l1)
  	l1 = suma(l1, s);
  	l1 = invertir(l1)
  	
  	do {
  		l0 = l1;
  		l1 = mover(l0, s);
  	} while(!(equivalentes(l0)(l1)));
  	
  	l1
  }
  
  def derecha(l:List[Int], s:Int):List[Int] = movimiento(l, s)
  
  def izquierda(l:List[Int], s:Int):List[Int] = invertir(movimiento(invertir(l), s))
  
  def arriba(l:List[Int], s:Int):List[Int] = transpuesta(izquierda(transpuesta(l, s), s), s)
  
  def abajo(l:List[Int], s:Int):List[Int] = transpuesta(derecha(transpuesta(l, s), s), s)
  
  lista = introFicha(2, 3, 2, lista, size)
  lista = introFicha(1, 2, 8, lista, size)
  lista = introFicha(1, 1, 2, lista, size)
  lista = introFicha(2, 1, 2, lista, size)
	lista = introFicha(3, 1, 2, lista, size)
  lista = introFicha(4, 1, 2, lista, size)
    

	println("normal")
  imprimir(lista, size)
  println("derecha")
  lista = derecha(lista, size)
  imprimir(lista, size)
  println("izquierda")
  lista = izquierda(lista, size)
  imprimir(lista, size)
  println("arriba")
  lista = arriba(lista, size)
  imprimir(lista, size)
  println("intro Ficha")
  lista = introFicha(2, 4, 2, lista, size)
  imprimir(lista, size)
  println("abajo")
  lista = abajo(lista, size)
  imprimir(lista, size)
}