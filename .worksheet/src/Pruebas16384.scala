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
  
  def transpuesta(l:List[Int], s:Int):List[Int] = transpuesta_aux(l, s)(1, 1);System.out.println("""transpuesta: (l: List[Int], s: Int)List[Int]""");$skip(727); 

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
  def suma(l:List[Int], s:Int)(p:Int):List[Int] = {
  	if (l.length == 0) p::Nil
  	else
  		if ((l.head == 0) || (l.length % s == 1)) l.head::suma(l.tail, s)(p)
  		else
  			if (l.head == l.tail.head) l.head*2::suma(0::l.tail.tail, s)(p+(l.head*2))
  			else l.head::suma(l.tail, s)(p)
  };System.out.println("""suma: (l: List[Int], s: Int)(p: Int)List[Int]""");$skip(269); 
  
  //
  def mover(l:List[Int], s:Int):List[Int] = {
  	if (l.length == 0) Nil
  	else
  		if ((l.head == 0) || ((l.length % s) == 1)) l.head::mover(l.tail, s)
  		else
  			if (l.tail.head == 0) 0::mover(l.head::l.tail.tail, s)
  			else l.head::mover(l.tail, s)
  };System.out.println("""mover: (l: List[Int], s: Int)List[Int]""");$skip(192); 
  
  //Mientras se pueda seguir moviendo, mueve
  def mueve(l:List[Int], s:Int):List[Int] = {
  	val laux:List[Int] = mover(l, s)
  	if (equivalentes(l)(laux)) laux
  	else mueve(laux, s)
  };System.out.println("""mueve: (l: List[Int], s: Int)List[Int]""");$skip(437); 
  
  //Pasos del movimiento de las fichas a la dch
  def movimiento(l:List[Int], s:Int):List[Int] = {
  	val t:List[Int] = (
  		invertir(//4º						- Volvemos a colocar
  			suma(//3º							- Suma los inmediatos
  				invertir(//2º				- Para que la suma se primero por el lateral
  					mueve(l, s))//1º	- Para colocar todo pegado a la dch
  			, s)(0)))
  	t.head::mueve(//5º								- Movemos a su posición final
  		t.tail
  	, s)
  };System.out.println("""movimiento: (l: List[Int], s: Int)List[Int]""");$skip(66); 
  
  def derecha(l:List[Int], s:Int):List[Int] = movimiento(l, s);System.out.println("""derecha: (l: List[Int], s: Int)List[Int]""");$skip(139); 
  
  def izquierda(l:List[Int], s:Int):List[Int] = {
  	val aux:List[Int] = movimiento(invertir(l), s)
  	aux.head::invertir(aux.tail)
  };System.out.println("""izquierda: (l: List[Int], s: Int)List[Int]""");$skip(147); 
  
  def arriba(l:List[Int], s:Int):List[Int] = {
  	val aux:List[Int] = izquierda(transpuesta(l, s), s)
  	aux.head::transpuesta(aux.tail, s)
  };System.out.println("""arriba: (l: List[Int], s: Int)List[Int]""");$skip(145); 
  
  def abajo(l:List[Int], s:Int):List[Int] =  {
  	val aux:List[Int] = derecha(transpuesta(l, s), s)
  	aux.head::transpuesta(aux.tail, s)
  };System.out.println("""abajo: (l: List[Int], s: Int)List[Int]""");$skip(226); 
  
  def imprimir(l: List[Int], s:Int): Unit = {
  	if (!(l.length == 0))
  		(l.length % s) match {
  			case 1 => {println(l.head); imprimir(l.tail, s);}
  			case _ => {print(l.head + "\t"); imprimir(l.tail, s);}
  		}
  };System.out.println("""imprimir: (l: List[Int], s: Int)Unit""");$skip(96); 
  
  def linea(s:Int):Unit ={
  	if (s == 0) print("\n")
  	else {print("-\t"); linea(s-1)}
  };System.out.println("""linea: (s: Int)Unit""");$skip(97); 
  
  def vidas(v:Int):Unit ={
  	if (v == 0) print("\n")
  	else {print(" <3 "); vidas(v-1)}
  };System.out.println("""vidas: (v: Int)Unit""");$skip(1421); 
  
  def bienvenidaIU():Unit ={
		print(".----------------.  .----------------.  .----------------.  .----------------.  .----------------.\n");
		print("| .--------------. || .--------------. || .--------------. || .--------------. || .--------------. |\n");
		print("| |     __       | || |    ______    | || |    ______    | || |     ____     | || |   _    _     | |\n");
		print("| |    /  |      | || |  .' ____ \\   | || |   / ____ `.  | || |   .' __ '.   | || |  | |  | |    | |\n");
		print("| |    `| |      | || |  | |____\\_|  | || |   `'  __) |  | || |   | (__) |   | || |  | |__| |_   | |\n");
		print("| |     | |      | || |  | '____`'.  | || |   _ | __ '.  | || |   .`____'.   | || |  |____   _|  | |\n");
		print("| |    _| |_     | || |  | (____) |  | || |  | \\____) |  | || |  | (____) |  | || |      _| |_   | |\n");
		print("| |   |_____|    | || |  '.______.'  | || |   \\______.'  | || |  `.______.'  | || |     |_____|  | |\n");
		print("| |              | || |              | || |              | || |              | || |              | |\n");
		print("| '--------------' || '--------------' || '--------------' || '--------------' || '--------------' |\n");
		print("'----------------'  '----------------'  '----------------'  '----------------'  '----------------' \n\n");
		print("                       Created by: Diego-Edgar Gracia & Daniel Lopez                                \n\n");
  };System.out.println("""bienvenidaIU: ()Unit""");$skip(116); 
  
  def encabezadoIU(s:Int, p:Int, best:Int):Unit ={
  	println("Punt: " + p + "\tMejor: " + best)
  	linea(s)
  };System.out.println("""encabezadoIU: (s: Int, p: Int, best: Int)Unit""");$skip(96); 
  
  def pieIU(s:Int, v:Int):Unit ={
  	linea(s)
  	print("VIDAS:\t"); vidas(v)
  	linea(s)
  };System.out.println("""pieIU: (s: Int, v: Int)Unit""");$skip(123); 
  
  def IU(l:List[Int], s:Int, v:Int, p:Int, b:Int):Unit ={
  	encabezadoIU(s, p, b)
  	imprimir(l, s)
  	pieIU(s, v)
  };System.out.println("""IU: (l: List[Int], s: Int, v: Int, p: Int, b: Int)Unit""");$skip(48); 
  
  var lista:List[Int] = rellenaTablero(size);System.out.println("""lista  : List[Int] = """ + $show(lista ));$skip(25); 
  var puntuacion:Int = 0;System.out.println("""puntuacion  : Int = """ + $show(puntuacion ));$skip(21); 
  var nvidas:Int = 3;System.out.println("""nvidas  : Int = """ + $show(nvidas ));$skip(20); 
  
  bienvenidaIU();$skip(44); 
  
  IU(lista, size, nvidas, puntuacion, 0);$skip(43); 
  lista = introFicha(3, 2, 4, lista, size);$skip(46); 
  
  lista = introFicha(2, 3, 2, lista, size);$skip(43); 
  lista = introFicha(1, 2, 8, lista, size);$skip(43); 
  lista = introFicha(1, 1, 2, lista, size);$skip(43); 
  lista = introFicha(2, 1, 2, lista, size);$skip(42); 
	lista = introFicha(3, 1, 2, lista, size);$skip(43); 
  lista = introFicha(4, 1, 2, lista, size);$skip(25); 
    

	println("normal");$skip(41); 
  IU(lista, size, nvidas, puntuacion, 0);$skip(21); 
  println("derecha");$skip(31); 
  lista = derecha(lista, size);$skip(37); 
  puntuacion = lista.head+puntuacion;$skip(21); 
  lista = lista.tail;$skip(41); 
  IU(lista, size, nvidas, puntuacion, 0);$skip(23); 
  println("izquierda");$skip(33); 
  lista = izquierda(lista, size);$skip(37); 
  puntuacion = lista.head+puntuacion;$skip(21); 
  lista = lista.tail;$skip(41); 
  IU(lista, size, nvidas, puntuacion, 0);$skip(20); 
  println("arriba");$skip(30); 
  lista = arriba(lista, size);$skip(37); 
  puntuacion = lista.head+puntuacion;$skip(21); 
  lista = lista.tail;$skip(41); 
  IU(lista, size, nvidas, puntuacion, 0);$skip(25); 
  println("intro Ficha");$skip(43); 
  lista = introFicha(2, 4, 2, lista, size);$skip(41); 
  IU(lista, size, nvidas, puntuacion, 0);$skip(19); 
  println("abajo");$skip(29); 
  lista = abajo(lista, size);$skip(37); 
  puntuacion = lista.head+puntuacion;$skip(21); 
  lista = lista.tail;$skip(41); 
  IU(lista, size, nvidas, puntuacion, 0);$skip(21); 
  println("derecha");$skip(31); 
  lista = derecha(lista, size);$skip(37); 
  puntuacion = lista.head+puntuacion;$skip(21); 
  lista = lista.tail;$skip(41); 
  IU(lista, size, nvidas, puntuacion, 0)}
}
