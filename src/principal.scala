

object principal {
  
  def mayor(x:Int)(y:Int):Int = {
    if (x > y) x
    else y
  }
  
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
  
  //Introduce la semilla de forma recursiva
  def introSemilla_aux(tablero:List[Int], cont:Int, semillas:List[Int], s:Int): List[Int]={ 
    
    val r = scala.util.Random
    
    if (cont==0) tablero
    else {
      val fila= r.nextInt(s)+1
      val columna= r.nextInt(s)+1
      val ficha= obtener(r.nextInt(semillas.length)+1,semillas)      
      introSemilla_aux(introFicha(columna,fila,ficha,tablero,s), cont-1,semillas,s)
    }   
  }
  
  //Metodo para introducir las semillas segun la dificultad
  def introSemillas(tablero: List[Int], dif:Int, s:Int): List[Int]={
    val dif1: List[Int]= List(2)
    val dif2: List[Int]= List(2,4)
    val dif3_4: List[Int]= List(2,4,8)
      
    val r = scala.util.Random
    
    dif match{
      case 1=>{
        introSemilla_aux(tablero,1,dif1,s)
      }
      case 2=>{
        introSemilla_aux(tablero,3,dif2,s)
      }
      case 3=>{
        introSemilla_aux(tablero,5,dif3_4,s)
      }
      case 4=>{
        introSemilla_aux(tablero,6,dif3_4,s)
      }
    }
  }
  
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
  
  //Comprobamos si se pueden realizar mas movimientos
  def comprobarMovimientos(tablero: List[Int], s:Int): Boolean={
    equivalentes(tablero)(subLista(derecha(tablero,s), s*s)) &&  //Dch
    equivalentes(tablero)(subLista(izquierda(tablero,s), s*s)) &&  //Izq
    equivalentes(tablero)(subLista(abajo(tablero,s), s*s)) &&  //Abajo
    equivalentes(tablero)(subLista(arriba(tablero,s), s*s))  //Arriba
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
  	println("Punt: " + p + "\tMejor: " + best + "\t(WASD para mover el tablero\tO para Salir)")
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
  
  def reconocerTeclado():Int={
    val tecla=scala.io.StdIn.readChar()
    
      //Arriba-->1
      if(tecla=='w' || tecla=='W'){
        1
      }
      //Izquierda-->2
      else if(tecla=='a'|| tecla=='A'){
        2
      }
      //Abajo-->3
      else if(tecla=='s' || tecla=='S'){
        3
      }
      //Derecha-->4
      else if(tecla=='d' || tecla=='D'){
        4
      }
      //Salir-->5
      else if(tecla=='o' || tecla=='O'){
        5
      }
      else{
        reconocerTeclado()
      }
  }
  
  def dificultad():Int={
	  print("                       --------- SELECCIONA LA DIFICULTAD ----------                                \n\n");
    val tecla=scala.io.StdIn.readChar()
    if(tecla=='1'){
      1
    }
    else if(tecla=='2'){
      2
    }
    else if(tecla=='3'){
      3
    }
    else if(tecla=='4'){
      4
    }
    else{
      dificultad()
    }
  }
  
  def iniciaTablero(dif:Int):List[Int] = dif match {
    case 1 => introSemilla_aux(rellenaTablero(4), 2, List(2), 4)
    case 2 => introSemilla_aux(rellenaTablero(9), 4, List(2, 4), 9)
    case 3 => introSemilla_aux(rellenaTablero(14), 6, List(2, 4, 8), 14)
    case 4 => introSemilla_aux(rellenaTablero(17), 6, List(2, 4, 8), 17)
  }
  
  //se llama a si mismo realizando cada ejecución de juego, y devuelve finalmente la puntuación obtenida
  //t-tablero  /  s-size  /  v-vidas  /  p-puntos  /  b-mejores puntos  /  d-dificultad
  def partida(t:List[Int], s:Int, v:Int, p:Int, b:Int, d:Int):Int ={
    reconocerTeclado() match {
      case 1 => {
        val l1:List[Int] = arriba(t, s)
        val puntos:Int = p + sumatorio(subLista(l1, (s*s)+1, l1.length))
        val l2:List[Int] = introSemillas(subLista(l1, s*s), d, s)
        val best:Int = mayor(puntos)(b)
        
        IU(l2, s, v, puntos, best)
        if (comprobarMovimientos(l2, s)) puntos
        else partida(l2, s, v, puntos, best, d)
        }
      case 2 => {
        val l1:List[Int] = izquierda(t, s)
        val puntos:Int = p + sumatorio(subLista(l1, (s*s)+1, l1.length))
        val l2:List[Int] = introSemillas(subLista(l1, s*s), d, s)
        val best:Int = mayor(puntos)(b)
        
        IU(l2, s, v, puntos, best)
        if (comprobarMovimientos(l2, s)) puntos
        else partida(l2, s, v, puntos, best, d)
        }
      case 3 => {
        val l1:List[Int] = abajo(t, s)
        val puntos:Int = p + sumatorio(subLista(l1, (s*s)+1, l1.length))
        val l2:List[Int] = introSemillas(subLista(l1, s*s), d, s)
        val best:Int = mayor(puntos)(b)
        
        IU(l2, s, v, puntos, best)
        if (comprobarMovimientos(l2, s)) puntos
        else partida(l2, s, v, puntos, best, d)
        }
      case 4 => {
        val l1:List[Int] = derecha(t, s)
        val puntos:Int = p + sumatorio(subLista(l1, (s*s)+1, l1.length))
        val l2:List[Int] = introSemillas(subLista(l1, s*s), d, s)
        val best:Int = mayor(puntos)(b)
        
        IU(l2, s, v, puntos, best)
        if (comprobarMovimientos(l2, s)) puntos
        else partida(l2, s, v, puntos, best, d)
        }
      case 5 => -p
      case _ => partida(t, s, v, p, b, d)
    }
  }
  
  def coin(vidas:Int, best:Int, dif:Int):Int = {
    if (vidas > 0) {
      dif match {
        case 1 => {
          val t:List[Int] = iniciaTablero(1)
          IU(t, 4, 2, 0, best)
          val b:Int = mayor(partida(t , 4, vidas, 0, best, 1))(best)
          if (b >= 0) coin (vidas-1, b, dif)
          else -b
        }
        case 2 => {
          val t:List[Int] = iniciaTablero(2)
          IU(t, 9, 2, 0, best)
          val b:Int = mayor(partida(t , 9, vidas, 0, best, 2))(best)
          if (b >= 0) coin (vidas-1, b, dif)
          else -b
        }
        case 3 => {
          val t:List[Int] = iniciaTablero(3)
          IU(t, 14, 2, 0, best)
          val b:Int = mayor(partida(t , 14, vidas, 0, best, 3))(best)
          if (b >= 0) coin (vidas-1, b, dif)
          else -b
        }
        case 4 => {
          val t:List[Int] = iniciaTablero(4)
          IU(t, 17, 2, 0, best)
          val b:Int = mayor(partida(t , 17, vidas, 0, best, 4))(best)
          if (b >= 0) coin (vidas-1, b, dif)
          else -b
        }
      }
    }
    else best
  }
  
  def main(args: Array[String]):Unit = {
    bienvenidaIU
    dificultad match {
      case 1 => {
        val b:Int = coin(3, 0, 1)
        print("                       ---------         GAME OVER        ----------                                \n\n");
        println("Mejor Puntuacion: " + b)
      }
      case 2 => {
        val b:Int = coin(3, 0, 2)
        print("                       ---------         GAME OVER        ----------                                \n\n");
        println("Mejor Puntuacion: " + b)
      }
      case 3 => {
        val b:Int = coin(3, 0, 3)
        print("                       ---------         GAME OVER        ----------                                \n\n");
        println("Mejor Puntuacion: " + b)
      }
      case 4 => {
        val b:Int = coin(3, 0, 4)
        print("                       ---------         GAME OVER        ----------                                \n\n");
        println("Mejor Puntuacion: " + b)
      }
      case _ => {
        println("ERROR")
      }
    }
    /*
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
    IU(tablero, size, nvidas, puntuacion, 0)*/
  }
}