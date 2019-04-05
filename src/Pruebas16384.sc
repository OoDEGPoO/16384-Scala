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
  }
  
  //
  def mover(l:List[Int], s:Int):List[Int] = {
  	if (l.length == 0) Nil
  	else
  		if ((l.head == 0) || ((l.length % s) == 1)) l.head::mover(l.tail, s)
  		else
  			if (l.tail.head == 0) 0::mover(l.head::l.tail.tail, s)
  			else l.head::mover(l.tail, s)
  }
  
  //Mientras se pueda seguir moviendo, mueve
  def mueve(l:List[Int], s:Int):List[Int] = {
  	val laux:List[Int] = mover(l, s)
  	if (equivalentes(l)(laux)) laux
  	else mueve(laux, s)
  }
  
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
  }
  
  def derecha(l:List[Int], s:Int):List[Int] = movimiento(l, s)
  
  def izquierda(l:List[Int], s:Int):List[Int] = {
  	val aux:List[Int] = movimiento(invertir(l), s)
  	aux.head::invertir(aux.tail)
  }
  
  def arriba(l:List[Int], s:Int):List[Int] = {
  	val aux:List[Int] = izquierda(transpuesta(l, s), s)
  	aux.head::transpuesta(aux.tail, s)
  }
  
  def abajo(l:List[Int], s:Int):List[Int] =  {
  	val aux:List[Int] = derecha(transpuesta(l, s), s)
  	aux.head::transpuesta(aux.tail, s)
  }
  
  def imprimir(l: List[Int], s:Int): Unit = {
  	if (!(l.length == 0))
  		(l.length % s) match {
  			case 1 => {println(l.head); imprimir(l.tail, s);}
  			case _ => {print(l.head + "\t"); imprimir(l.tail, s);}
  		}
  }
  
  def linea(s:Int):Unit ={
  	if (s == 0) print("\n")
  	else {print("-\t"); linea(s-1)}
  }
  
  def vidas(v:Int):Unit ={
  	if (v == 0) print("\n")
  	else {print(" <3 "); vidas(v-1)}
  }
  
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
  }
  
  def encabezadoIU(s:Int, p:Int, best:Int):Unit ={
  	println("Punt: " + p + "\tMejor: " + best)
  	linea(s)
  }
  
  def pieIU(s:Int, v:Int):Unit ={
  	linea(s)
  	print("VIDAS:\t"); vidas(v)
  	linea(s)
  }
  
  def IU(l:List[Int], s:Int, v:Int, p:Int, b:Int):Unit ={
  	encabezadoIU(s, p, b)
  	imprimir(l, s)
  	pieIU(s, v)
  }
  
  var lista:List[Int] = rellenaTablero(size)
  var puntuacion:Int = 0
  var nvidas:Int = 3
  
  bienvenidaIU()
  
  IU(lista, size, nvidas, puntuacion, 0)
  lista = introFicha(3, 2, 4, lista, size)
  
  lista = introFicha(2, 3, 2, lista, size)
  lista = introFicha(1, 2, 8, lista, size)
  lista = introFicha(1, 1, 2, lista, size)
  lista = introFicha(2, 1, 2, lista, size)
	lista = introFicha(3, 1, 2, lista, size)
  lista = introFicha(4, 1, 2, lista, size)
    

	println("normal")
  IU(lista, size, nvidas, puntuacion, 0)
  println("derecha")
  lista = derecha(lista, size)
  puntuacion = lista.head+puntuacion
  lista = lista.tail
  IU(lista, size, nvidas, puntuacion, 0)
  println("izquierda")
  lista = izquierda(lista, size)
  puntuacion = lista.head+puntuacion
  lista = lista.tail
  IU(lista, size, nvidas, puntuacion, 0)
  println("arriba")
  lista = arriba(lista, size)
  puntuacion = lista.head+puntuacion
  lista = lista.tail
  IU(lista, size, nvidas, puntuacion, 0)
  println("intro Ficha")
  lista = introFicha(2, 4, 2, lista, size)
  IU(lista, size, nvidas, puntuacion, 0)
  println("abajo")
  lista = abajo(lista, size)
  puntuacion = lista.head+puntuacion
  lista = lista.tail
  IU(lista, size, nvidas, puntuacion, 0)
  println("derecha")
  lista = derecha(lista, size)
  puntuacion = lista.head+puntuacion
  lista = lista.tail
  IU(lista, size, nvidas, puntuacion, 0)
}