

object principal {
  
  def rellenaLista(long:Int)(valor:Int):List[Int] = {
  	if (long == 0) Nil
  	else valor::rellenaLista(long-1)(valor)
  }
  
  def rellenaTablero(s:Int):List[Int] = {
  	rellenaLista(s * s)(0)
  }
  
  //Si son la misma lista
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
  
  //devuelve la sublista desde el principio de l hasta pos, incluido
  def subLista(l:List[Int], pos:Int):List[Int] = {
    if (pos == 0 || l.length == 0) Nil
    else l.head::subLista(l.tail, pos -1)
  }
  
  //devuelve la sublista de l desde ini hasta fin, incluidos
  def subLista(l:List[Int], ini:Int, fin:Int):List[Int] = {
    if (ini == 1 || l.length == 0) subLista(l, fin)
    else subLista(l.tail, ini-1, fin-1)
  }
  
  //lista invertida en orden
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
  
  //Transpuesta de la matriz cuadrada l (se introduce linealizada)
  def transpuesta(l:List[Int], s:Int):List[Int] = transpuesta_aux(l, s)(1, 1)
  
  //Suma de todos los elementos de una lista 
  def sumatorio(l:List[Int]):Int = {
    if (l.length == 0) 0
    else l.head+sumatorio(l.tail)
  }
  
  //Suma con el siguiente valor inmediato siempre y cuando sean equivalentes
  def suma(l:List[Int], s:Int)(p:List[Int]):List[Int] = {
  	if (l.length == 0) p
  	else
  		if ((l.head == 0) || (l.length % s == 1)) l.head::suma(l.tail, s)(p)
  		else
  			if (l.head == l.tail.head) l.head*2::suma(0::l.tail.tail, s)(l.head*2::p)
  			else l.head::suma(l.tail, s)(p)
  }
  
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
    val sum:List[Int] = suma(invertir(mueve(l, s)), s)(Nil)
    val punt:List[Int] = subLista(sum, (s*s)+1, sum.length)
    
  	val t:List[Int] = (invertir(subLista(sum, s*s)))
  	mueve(t, s):::punt
  }
  
  def derecha(l:List[Int], s:Int):List[Int] = movimiento(l, s)
  
  def izquierda(l:List[Int], s:Int):List[Int] = {
  	val aux:List[Int] = movimiento(invertir(l), s)
  	invertir(subLista(aux, s*s)):::subLista(aux, (s*s)+1, aux.length)
  }
  
  def arriba(l:List[Int], s:Int):List[Int] = {
  	val aux:List[Int] = izquierda(transpuesta(l, s), s)
  	transpuesta(subLista(aux, s*s), s):::subLista(aux, (s*s)+1, aux.length)
  }
  
  def abajo(l:List[Int], s:Int):List[Int] =  {
  	val aux:List[Int] = derecha(transpuesta(l, s), s)
  	transpuesta(subLista(aux, s*s), s):::subLista(aux, (s*s)+1, aux.length)
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
		print("                       --------------- AHORA EN SCALA --------------                                \n\n");
  }
  
  def encabezadoIU(s:Int, p:Int, best:Int):Unit ={
  	println("Punt: " + p + "\tMejor: " + best)
  	linea(s)
  }
  
  def pieIU(s:Int, v:Int):Unit ={
  	linea(s)
  	print("VIDAS:\t"); vidas(v)
  	linea(s)
  	println()
  }
  
  def IU(l:List[Int], s:Int, v:Int, p:Int, b:Int):Unit ={
  	encabezadoIU(s, p, b)
  	imprimir(l, s)
  	pieIU(s, v)
  }
  
  /*def jugada(t:List[Int], s:Int, v:Int, p:Int, b:Int):Int ={
    
  }*/
  
  def main(args: Array[String]):Unit = {
    var size:Int = 4
    var tablero:List[Int] = rellenaTablero(size)
    var puntuacion:Int = 0
    var nvidas:Int = 3
    
    bienvenidaIU()
    
    IU(tablero, size, nvidas, puntuacion, 0)
    tablero = introFicha(3, 2, 4, tablero, size)
    
    tablero = introFicha(2, 3, 2, tablero, size)
    tablero = introFicha(1, 2, 8, tablero, size)
    tablero = introFicha(1, 1, 2, tablero, size)
    tablero = introFicha(2, 1, 2, tablero, size)
  	tablero = introFicha(3, 1, 2, tablero, size)
    tablero = introFicha(4, 1, 2, tablero, size)
      
  
  	println("normal")
    IU(tablero, size, nvidas, puntuacion, 0)
    println("derecha")
    tablero = derecha(tablero, size)
    puntuacion = sumatorio(subLista(tablero, (size*size)+1, tablero.length)) + puntuacion
    tablero = subLista(tablero, size*size)
    IU(tablero, size, nvidas, puntuacion, 0)
    println("izquierda")
    tablero = izquierda(tablero, size)
    puntuacion = sumatorio(subLista(tablero, (size*size)+1, tablero.length)) + puntuacion
    tablero = subLista(tablero, size*size)
    IU(tablero, size, nvidas, puntuacion, 0)
    println("arriba")
    tablero = arriba(tablero, size)
    puntuacion = sumatorio(subLista(tablero, (size*size)+1, tablero.length)) + puntuacion
    tablero = subLista(tablero, size*size)
    IU(tablero, size, nvidas, puntuacion, 0)
    println("intro Ficha")
    tablero = introFicha(2, 4, 2, tablero, size)
    IU(tablero, size, nvidas, puntuacion, 0)
    println("abajo")
    tablero = abajo(tablero, size)
    puntuacion = sumatorio(subLista(tablero, (size*size)+1, tablero.length)) + puntuacion
    tablero = subLista(tablero, size*size)
    IU(tablero, size, nvidas, puntuacion, 0)
    println("derecha")
    tablero = derecha(tablero, size)
    puntuacion = sumatorio(subLista(tablero, (size*size)+1, tablero.length)) + puntuacion
    tablero = subLista(tablero, size*size)
    IU(tablero, size, nvidas, puntuacion, 0)
  }
}