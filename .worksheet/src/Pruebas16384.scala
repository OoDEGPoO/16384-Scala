object Pruebas16384 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(64); 
  println("Welcome to the Scala workshit");$skip(43); 
  // Tableros cuadrados
  var size:Int = 4;System.out.println("""size  : Int = """ + $show(size ));$skip(126); 
  
  def rellenaLista(long:Int)(valor:Int):List[Int] = {
  	if (long == 0) Nil
  	else valor::rellenaLista(long-1)(valor)
  };System.out.println("""rellenaLista: (long: Int)(valor: Int)List[Int]""");$skip(75); 
  
  def rellenaTablero(s:Int):List[Int] = {
  	rellenaLista(s * s)(0)
  };System.out.println("""rellenaTablero: (s: Int)List[Int]""");$skip(219); 
  
  def equivalentes(m:List[Int])(n:List[Int]):Boolean ={
  	if (m.length != n.length) false
  	else
  		if (m.isEmpty) true
  		else
  			if (m.head == n.head) true && equivalentes(m.tail)(n.tail)
  			else false
  };System.out.println("""equivalentes: (m: List[Int])(n: List[Int])Boolean""");$skip(177); 
  
  def setLista(pos:Int, n:Int, l:List[Int]):List[Int] ={
  	if (l.length == 0) Nil
  	else
  		if (pos == 1)
  			n::l.tail
  		else
  			l.head::poner(pos-1, n, l.tail)
  };System.out.println("""setLista: (pos: Int, n: Int, l: List[Int])List[Int]""");$skip(262); 
  
  //Como setLista pero comprobando que se pueda introducir
  def poner(pos:Int, n:Int, l:List[Int]):List[Int] ={
  	if (l.length == 0) Nil
  	else
  		if (pos == 1)
  			if (l.head == 0) n::l.tail
  			else l
  		else
  			l.head::poner(pos-1, n, l.tail)
  };System.out.println("""poner: (pos: Int, n: Int, l: List[Int])List[Int]""");$skip(157); 
  
  def introFicha(x:Int, y:Int, n:Int, l:List[Int], s:Int):List[Int] = {
  	if ( x < 0 && x > s && y < 0 && y > s ) l
  	else poner(x+((y-1)*s), n, l)
  };System.out.println("""introFicha: (x: Int, y: Int, n: Int, l: List[Int], s: Int)List[Int]""");$skip(148); 
  
  def obtener(pos:Int, lista:List[Int]): Int={
  	if (lista.length == 0) 0
  	else if (pos==1) lista.head
  	else obtener(pos-1, lista.tail)
  };System.out.println("""obtener: (pos: Int, lista: List[Int])Int""");$skip(141); 
  
  def getFicha(l:List[Int], s:Int)(x:Int, y:Int):Int = {
  	if ( x < 0 && x > s && y < 0 && y > s ) 0
  	else obtener(x+((y-1)*s), l)
  };System.out.println("""getFicha: (l: List[Int], s: Int)(x: Int, y: Int)Int""");$skip(114); 
  
  def invertir(l:List[Int]):List[Int] = {
  	if (l.length == 0) Nil
  	else invertir(l.tail):::l.head::Nil
  };System.out.println("""invertir: (l: List[Int])List[Int]""");$skip(253); 
  
  def transpuesta_aux(l:List[Int], s:Int)(x:Int, y:Int):List[Int] = {
  	if ((l.length == 0) || (s < y)) Nil
  	else
  		if (x == s) getFicha(l, s)(y, x)::transpuesta_aux(l, s)(1, y+1)
  		else getFicha(l, s)(y, x)::transpuesta_aux(l, s)(x+1, y)
  };System.out.println("""transpuesta_aux: (l: List[Int], s: Int)(x: Int, y: Int)List[Int]""");$skip(81); 
  
  def transpuesta(l:List[Int], s:Int):List[Int] = transpuesta_aux(l, s)(1, 1);System.out.println("""transpuesta: (l: List[Int], s: Int)List[Int]""");$skip(48); 
  
  var lista:List[Int] = rellenaTablero(size);System.out.println("""lista  : List[Int] = """ + $show(lista ));$skip(226); 
  
  def imprimir(l: List[Int], s:Int): Unit = {
  	if (!(l.length == 0))
  		(l.length % s) match {
  			case 1 => {println(l.head); imprimir(l.tail, s);}
  			case _ => {print(l.head + "\t"); imprimir(l.tail, s);}
  		}
  };System.out.println("""imprimir: (l: List[Int], s: Int)Unit""");$skip(27); 
  
  imprimir(lista, size);$skip(46); 
  
  lista = introFicha(3, 2, 4, lista, size);$skip(697); 

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
  };System.out.println("""suma: (l: List[Int], s: Int)List[Int]""");$skip(264); 
  
  def mover(l:List[Int], s:Int):List[Int] = {
  	if (l.length == 0) Nil
  	else
  		if ((l.head == 0) || ((l.length % s) == 1)) l.head::mover(l.tail, s)
  		else
  			if (l.tail.head == 0) 0::mover(l.head::l.tail.tail, s)
  			else l.head::mover(l.tail, s)
  };System.out.println("""mover: (l: List[Int], s: Int)List[Int]""");$skip(353); 
  
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
  };System.out.println("""movimiento: (l: List[Int], s: Int)List[Int]""");$skip(66); 
  
  def derecha(l:List[Int], s:Int):List[Int] = movimiento(l, s);System.out.println("""derecha: (l: List[Int], s: Int)List[Int]""");$skip(88); 
  
  def izquierda(l:List[Int], s:Int):List[Int] = invertir(movimiento(invertir(l), s));System.out.println("""izquierda: (l: List[Int], s: Int)List[Int]""");$skip(96); 
  
  def arriba(l:List[Int], s:Int):List[Int] = transpuesta(izquierda(transpuesta(l, s), s), s);System.out.println("""arriba: (l: List[Int], s: Int)List[Int]""");$skip(93); 
  
  def abajo(l:List[Int], s:Int):List[Int] = transpuesta(derecha(transpuesta(l, s), s), s);System.out.println("""abajo: (l: List[Int], s: Int)List[Int]""");$skip(46); 
  
  lista = introFicha(2, 3, 2, lista, size);$skip(43); 
  lista = introFicha(1, 2, 8, lista, size);$skip(43); 
  lista = introFicha(1, 1, 2, lista, size);$skip(43); 
  lista = introFicha(2, 1, 2, lista, size);$skip(42); 
	lista = introFicha(3, 1, 2, lista, size);$skip(43); 
  lista = introFicha(4, 1, 2, lista, size);$skip(25); 
    

	println("normal");$skip(24); 
  imprimir(lista, size);$skip(21); 
  println("derecha");$skip(31); 
  lista = derecha(lista, size);$skip(24); 
  imprimir(lista, size);$skip(23); 
  println("izquierda");$skip(33); 
  lista = izquierda(lista, size);$skip(24); 
  imprimir(lista, size);$skip(20); 
  println("arriba");$skip(30); 
  lista = arriba(lista, size);$skip(24); 
  imprimir(lista, size);$skip(25); 
  println("intro Ficha");$skip(43); 
  lista = introFicha(2, 4, 2, lista, size);$skip(24); 
  imprimir(lista, size);$skip(19); 
  println("abajo");$skip(29); 
  lista = abajo(lista, size);$skip(24); 
  imprimir(lista, size)}
}
