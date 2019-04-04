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
  };System.out.println("""equivalentes: (m: List[Int])(n: List[Int])Boolean""");$skip(48); 
  
  var lista:List[Int] = rellenaTablero(size);System.out.println("""lista  : List[Int] = """ + $show(lista ));$skip(220); 
  
  def imprimir(l: List[Int], s:Int): Unit = {
  	if (!l.isEmpty)
  		(l.length % s) match {
  			case 1 => {println(l.head); imprimir(l.tail, s);}
  			case _ => {print(l.head + "\t"); imprimir(l.tail, s);}
  		}
  };System.out.println("""imprimir: (l: List[Int], s: Int)Unit""");$skip(27); 
  
  imprimir(lista, size);$skip(173); 
  
  def setLista(pos:Int, n:Int, l:List[Int]):List[Int] ={
  	if (l.isEmpty) Nil
  	else
  		if (pos == 1)
  			n::l.tail
  		else
  			l.head::poner(pos-1, n, l.tail)
  };System.out.println("""setLista: (pos: Int, n: Int, l: List[Int])List[Int]""");$skip(258); 
  
  //Como setLista pero comprobando que se pueda introducir
  def poner(pos:Int, n:Int, l:List[Int]):List[Int] ={
  	if (l.isEmpty) Nil
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
  };System.out.println("""introFicha: (x: Int, y: Int, n: Int, l: List[Int], s: Int)List[Int]""");$skip(46); 
  
  lista = introFicha(3, 2, 4, lista, size);$skip(27); 
  
  imprimir(lista, size);$skip(17); 
  println(lista);$skip(144); 
  
  def obtener(pos:Int, lista:List[Int]): Int={
  	if (lista.isEmpty) 0
  	else if (pos==1) lista.head
  	else obtener(pos-1, lista.tail)
  };System.out.println("""obtener: (pos: Int, lista: List[Int])Int""");$skip(141); 
  
  def getFicha(x:Int, y:Int, l:List[Int], s:Int):Int = {
  	if ( x < 0 && x > s && y < 0 && y > s ) 0
  	else obtener(x+((y-1)*s), l)
  };System.out.println("""getFicha: (x: Int, y: Int, l: List[Int], s: Int)Int""");$skip(147); 
  
  /*def voltHorario(l:List[Int], s:Int):List[Int] = {
  	
  }*/
  
  println(getFicha(2, 3, lista, size) + " - " + getFicha(3, 2, lista, size));$skip(335); 

	//Cuantas posiciones a la derecha de X está el siguiente equivalente
	def buscaDch(l:List[Int], s:Int, x:Int, pos:Int):Int = {
		if (l.isEmpty || (l.length % s == 0)) 0 //Si está vacío o si se busca ya en la siguiente fila
		else
			if (l.head == x) pos + 1
			else
				if (l.head == 0) buscaDch(l.tail, s, x, pos + 1)
				else 0
	};System.out.println("""buscaDch: (l: List[Int], s: Int, x: Int, pos: Int)Int""");$skip(361); 

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
  };System.out.println("""sumDch: (l: List[Int], s: Int)List[Int]""");$skip(264); 
  
  def movDch(l:List[Int], s:Int):List[Int] = {
  	if (l.isEmpty) Nil
  	else
  		if ((l.head == 0) || ((l.length % s) == 1)) l.head::movDch(l.tail, s)
  		else
  			if (l.tail.head == 0) 0::movDch(l.head::l.tail.tail, s)
  			else l.head::movDch(l.tail, s)
  };System.out.println("""movDch: (l: List[Int], s: Int)List[Int]""");$skip(226); 
  
  def derecha(l:List[Int], s:Int):List[Int] = {
  	var l0:List[Int] = l
  	var l1:List[Int] = l
  	
  	l1 = sumDch(l0, s);
  	
  	do {
  		l0 = l1;
  		l1 = movDch(l0, s);
  	} while(!(equivalentes(l0)(l1)));
  	
  	l1
  };System.out.println("""derecha: (l: List[Int], s: Int)List[Int]""");$skip(288); 
  
  def movIzq(l:List[Int], s:Int):List[Int] = {
  	if (l.isEmpty) Nil
  	else
  		if ((l.last == 0) || ((l.length % s) == 1)) movIzq(l.init, s):::l.last::Nil
  		else
  			if (l.init.last == 0) movIzq(l.init.init:::l.last::Nil, s):::0::Nil
  			else movIzq(l.init, s):::l.last::Nil
  };System.out.println("""movIzq: (l: List[Int], s: Int)List[Int]""");$skip(201); 
  
  def izquierda(l:List[Int], s:Int):List[Int] = {
  	var l0:List[Int] = l
  	var l1:List[Int] = l
  	
  	do {
  		l0 = l1;
  		l1 = movIzq(l0, s);
  	} while(!(equivalentes(l0)(l1)));
  	
  	l1
  };System.out.println("""izquierda: (l: List[Int], s: Int)List[Int]""");$skip(46); 
  
  lista = introFicha(2, 3, 2, lista, size);$skip(43); 
  lista = introFicha(1, 2, 8, lista, size);$skip(43); 
  lista = introFicha(1, 1, 2, lista, size);$skip(42); 
	lista = introFicha(3, 1, 2, lista, size);$skip(30); 
    

  imprimir(lista, size);$skip(31); 
  lista = derecha(lista, size);$skip(24); 
  imprimir(lista, size);$skip(33); 
  lista = izquierda(lista, size);$skip(24); 
  imprimir(lista, size)}
}
