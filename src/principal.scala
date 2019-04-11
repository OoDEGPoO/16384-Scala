//import java.awt.BorderLayout
//import java.awt.Dimension
//import java.awt.Color
//import javax.swing.JFrame
//import javax.swing.JScrollPane
//import javax.swing.JTextArea
//import javax.swing.JTable


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
  			l.head::setLista(pos-1, n, l.tail)
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
      
      val nuevoTablero=introSemilla_aux(introFicha(columna,fila,ficha,tablero,s), cont-1,semillas,s)
      if(nuevoTablero==tablero){
        introSemilla_aux(introFicha(columna,fila,ficha,tablero,s), cont,semillas,s)
      }
      else{
        nuevoTablero
      }
      
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
  
  def listaIntAString(l:List[Int]): List[String] = {
    if (l.length == 0) Nil
    else l.head.toString()::listaIntAString(l.tail)
  }
  
  def getArrayFromList(l:List[Object]): Array[Object] = {
    if (l.length == 0) Array()
    else Array(l.head) ++ getArrayFromList(l.tail)
  }
  
  def ListIntToArrayObject(l:List[Int]): Array[Object] = {
    if (l.length == 0) Array()
    else Array(l.head.toString()) ++ ListIntToArrayObject(l.tail)
  }
  
  def ListADobleArray(l:List[Int], s:Int): Array[Array[Object]] = {
    if (l.length == 0) Array()
    else Array( ListIntToArrayObject(subLista(l, s)) ) ++ ListADobleArray(subLista(l, s+1, l.length), s)
  }
  
  def tituloVacioColumnas(s:Int): Array[Object] = {
    if (s == 0) Array()
    else Array("") ++ tituloVacioColumnas(s-1)
  }
  
  def imprimir(l: List[Int], s:Int): Unit = {
  	if (!(l.length == 0))
  		(l.length % s) match {
  			case 1 => {println(l.head); imprimir(l.tail, s);}
  			case _ => {print(l.head + "\t"); imprimir(l.tail, s);}
  		}
  }
  
  def actualizaTablaAux(tabla:JTable, l:List[Int], s:Int)(x:Int, y:Int):Unit ={
    if (l.length != 0) {
      if (x == s) {
        tabla.setValueAt(l.head.toString(), x-1, y-1)
        actualizaTablaAux(tabla, l.tail, s)(1, y+1)
      }
      else {
        tabla.setValueAt(l.head.toString(), x-1, y-1)
        actualizaTablaAux(tabla, l.tail, s)(x+1, y)
      }
    }
  }
  
  def actualizaTabla(tabla:JTable, l:List[Int], s:Int):Unit ={
    actualizaTablaAux(tabla, l, s)(1, 1)
  }
  
  def linea(s:Int):String ={
  	if (s == 0) "\n"
  	else {"-\t" + linea(s-1)}
  }
  
  def vidas(v:Int):String ={
  	if (v == 0) ""
  	else {" <3 " + vidas(v-1)}
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
  
  def encabezadoIU(s:Int, p:Int, best:Int):String ={
  	"Punt: " + p.toString + "\tMejor: " + best.toString + "\t(WASD para mover el tablero\tO para Salir)"
  }
  
  def pieIU(v:Int):String ={
  	"VIDAS:\t" + vidas(v)
  }
  
  def iuGrafica(l:List[Int], s:Int, v:Int, p:Int, b:Int, cabecera:JTextArea, tabla:JTable, pie:JTextArea):Unit = {
    cabecera.setText(encabezadoIU(s, p, b))
    actualizaTabla(tabla, transpuesta(l, s), s)
    pie.setText(pieIU(v))
  }
  
  def IU(l:List[Int], s:Int, v:Int, p:Int, b:Int, cabecera:JTextArea, tabla:JTable, pie:JTextArea):Unit ={
  	print(encabezadoIU(s, p, b) + "\n" + linea(s))
  	imprimir(l, s)
  	print(linea(s)+pieIU(v) + "\n" +linea(s))
  	iuGrafica(l, s, v, p, b, cabecera, tabla, pie)
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
  
  def modo(): Int={
    print("                       --------- SELECCIONA EL MODO ----------                                \n\n");
    val tecla=scala.io.StdIn.readChar()
    if(tecla=='m' || tecla=='M'){
      1
    }
    else if(tecla=='a' || tecla=='A'){
      2
    }
    else{
      modo()
    }
  }
  
  def dificultad():Int={
	  print("                       --------- SELECCIONA LA DIFICULTAD ----------                                \n\n");
    val tecla=scala.io.StdIn.readInt()
    if(tecla>0 && tecla <5){
      tecla
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
  
  def iniTableroIU(dif:Int):Array[Array[Object]] = dif match {
    case 1 => ListADobleArray(rellenaTablero(4), 4)
    case 2 => ListADobleArray(rellenaTablero(9), 9)
    case 3 => ListADobleArray(rellenaTablero(14), 14)
    case 4 => ListADobleArray(rellenaTablero(17), 17)
  }
  
  def iniColumnasIU(dif:Int):Array[Object] = dif match {
    case 1 => tituloVacioColumnas(4)
    case 2 => tituloVacioColumnas(9)
    case 3 => tituloVacioColumnas(14)
    case 4 => tituloVacioColumnas(17)
  }
  
  //se llama a si mismo realizando cada ejecución de juego, y devuelve finalmente la puntuación obtenida
  //t-tablero  /  s-size  /  v-vidas  /  p-puntos  /  b-mejores puntos  /  d-dificultad
  def partida(t:List[Int], s:Int, v:Int, p:Int, b:Int, d:Int, cabecera:JTextArea, tabla:JTable, pie:JTextArea):Int ={
    reconocerTeclado() match {
      case 1 => {
        val l1:List[Int] = arriba(t, s)
        val puntos:Int = p + sumatorio(subLista(l1, (s*s)+1, l1.length))
        val l2:List[Int] = introSemillas(subLista(l1, s*s), d, s)
        val best:Int = mayor(puntos)(b)
        
        IU(l2, s, v, puntos, best, cabecera, tabla, pie)
        if (comprobarMovimientos(l2, s)) puntos
        else partida(l2, s, v, puntos, best, d, cabecera, tabla, pie)
        }
      case 2 => {
        val l1:List[Int] = izquierda(t, s)
        val puntos:Int = p + sumatorio(subLista(l1, (s*s)+1, l1.length))
        val l2:List[Int] = introSemillas(subLista(l1, s*s), d, s)
        val best:Int = mayor(puntos)(b)
        
        IU(l2, s, v, puntos, best, cabecera, tabla, pie)
        if (comprobarMovimientos(l2, s)) puntos
        else partida(l2, s, v, puntos, best, d, cabecera, tabla, pie)
        }
      case 3 => {
        val l1:List[Int] = abajo(t, s)
        val puntos:Int = p + sumatorio(subLista(l1, (s*s)+1, l1.length))
        val l2:List[Int] = introSemillas(subLista(l1, s*s), d, s)
        val best:Int = mayor(puntos)(b)
        
        IU(l2, s, v, puntos, best, cabecera, tabla, pie)
        if (comprobarMovimientos(l2, s)) puntos
        else partida(l2, s, v, puntos, best, d, cabecera, tabla, pie)
        }
      case 4 => {
        val l1:List[Int] = derecha(t, s)
        val puntos:Int = p + sumatorio(subLista(l1, (s*s)+1, l1.length))
        val l2:List[Int] = introSemillas(subLista(l1, s*s), d, s)
        val best:Int = mayor(puntos)(b)
        
        IU(l2, s, v, puntos, best, cabecera, tabla, pie)
        if (comprobarMovimientos(l2, s)) puntos
        else partida(l2, s, v, puntos, best, d, cabecera, tabla, pie)
        }
      case 5 => -p
      case _ => partida(t, s, v, p, b, d, cabecera, tabla, pie)
    }
  }
  
  def partidaAuto(t:List[Int], s:Int, v:Int, p:Int, b:Int, d:Int, cabecera:JTextArea, tabla:JTable, pie:JTextArea):Int ={
        Thread.sleep(1000)
        val larriba:List[Int] = arriba(t, s)
        val puntosArriba:Int = p + sumatorio(subLista(larriba, (s*s)+1, larriba.length))
        
        val labajo:List[Int] = abajo(t, s)
        val puntosAbajo:Int = p + sumatorio(subLista(labajo, (s*s)+1, labajo.length))
        
        val lizquierda:List[Int] = izquierda(t, s)
        val puntosIzquierda:Int = p + sumatorio(subLista(lizquierda, (s*s)+1, lizquierda.length))
        
        val lderecha:List[Int] = derecha(t, s)
        val puntosDerecha:Int = p + sumatorio(subLista(lderecha, (s*s)+1, lderecha.length))
        
        val listaTableros= List("arriba","abajo","izquierda","derecha")
        val listaPuntos= List(puntosArriba,puntosAbajo,puntosIzquierda,puntosDerecha)       
        val mejorLista=mejorMovimiento(listaTableros,listaPuntos)
        
        mejorLista.head match{
          case "arriba"=>{
            val tableroFinal= introSemillas(subLista(larriba, s*s), d, s)
            val puntosFinal= puntosArriba
            val best:Int = mayor(puntosFinal)(b)
            
            IU(tableroFinal, s, v,puntosFinal, b, cabecera, tabla, pie)
            if (comprobarMovimientos(tableroFinal, s)) puntosFinal
            else partidaAuto(tableroFinal, s, v, puntosFinal, best, d, cabecera, tabla, pie)
          }
          case "abajo"=>{
            val tableroFinal= introSemillas(subLista(labajo, s*s), d, s)
            val puntosFinal= puntosAbajo
            val best:Int = mayor(puntosFinal)(b)
            
            IU(tableroFinal, s, v,puntosFinal, b, cabecera, tabla, pie)
            if (comprobarMovimientos(tableroFinal, s)) puntosFinal
            else partidaAuto(tableroFinal, s, v, puntosFinal, best, d, cabecera, tabla, pie)
          }
          case "izquierda"=>{
            val tableroFinal= introSemillas(subLista(lizquierda, s*s), d, s)
            val puntosFinal= puntosIzquierda
            val best:Int = mayor(puntosFinal)(b)
            
            IU(tableroFinal, s, v,puntosFinal, b, cabecera, tabla, pie)
            if (comprobarMovimientos(tableroFinal, s)) puntosFinal
            else partidaAuto(tableroFinal, s, v, puntosFinal, best, d, cabecera, tabla, pie)
          }
          case "derecha"=>{
            val tableroFinal= introSemillas(subLista(lderecha, s*s), d, s)
            val puntosFinal= puntosDerecha
            val best:Int = mayor(puntosFinal)(b)
            
            IU(tableroFinal, s, v,puntosFinal, b, cabecera, tabla, pie)
            if (comprobarMovimientos(tableroFinal, s)) puntosFinal
            else partidaAuto(tableroFinal, s, v, puntosFinal, best, d, cabecera, tabla, pie)
          }
        }       
  }
  
  def mejorMovimiento(movimientos:List[String], puntos:List[Int]):List[String]={
    if(puntos.length>1){
      if(puntos.head>(puntos.tail).head){
        mejorMovimiento(movimientos.head::movimientos.tail.tail,puntos.head::puntos.tail.tail)
      }
      else{
        mejorMovimiento(movimientos.tail,puntos.tail)
      }
    }
    else{
      movimientos
    }
  }
  
  def coin(vidas:Int, best:Int, dif:Int, cabecera:JTextArea, tabla:JTable, pie:JTextArea):Int = {
    if (vidas > 0) {
      dif match {
        case 1 => {
          val t:List[Int] = iniciaTablero(1)
          IU(t, 4, vidas, 0, best, cabecera, tabla, pie)
          val b:Int = mayor(partida(t , 4, vidas, 0, best, 1, cabecera, tabla, pie))(best)
          if (b >= 0) coin (vidas-1, b, dif, cabecera, tabla, pie)
          else -b
        }
        case 2 => {
          val t:List[Int] = iniciaTablero(2)
          IU(t, 9, vidas, 0, best, cabecera, tabla, pie)
          val b:Int = mayor(partida(t , 9, vidas, 0, best, 2, cabecera, tabla, pie))(best)
          if (b >= 0) coin (vidas-1, b, dif, cabecera, tabla, pie)
          else -b
        }
        case 3 => {
          val t:List[Int] = iniciaTablero(3)
          IU(t, 14, vidas, 0, best, cabecera, tabla, pie)
          val b:Int = mayor(partida(t , 14, vidas, 0, best, 3, cabecera, tabla, pie))(best)
          if (b >= 0) coin (vidas-1, b, dif, cabecera, tabla, pie)
          else -b
        }
        case 4 => {
          val t:List[Int] = iniciaTablero(4)
          IU(t, 17, vidas, 0, best, cabecera, tabla, pie)
          val b:Int = mayor(partida(t , 17, vidas, 0, best, 4, cabecera, tabla, pie))(best)
          if (b >= 0) coin (vidas-1, b, dif, cabecera, tabla, pie)
          else -b
        }
      }
    }
    else best
  }
  
  def modoAutomatico(vidas:Int, best:Int, dif:Int, cabecera:JTextArea, tabla:JTable, pie:JTextArea):Int= {
     if (vidas > 0) {
      dif match {
        case 1 => {
          val t:List[Int] = iniciaTablero(1)
          IU(t, 4, vidas, 0, best, cabecera, tabla, pie)
          val b:Int = mayor(partidaAuto(t , 4, vidas, 0, best, 1, cabecera, tabla, pie))(best)
          if (b >= 0) modoAutomatico(vidas-1, b, dif, cabecera, tabla, pie)
          else -b
        }
        case 2 => {
          val t:List[Int] = iniciaTablero(2)
          IU(t, 9, vidas, 0, best, cabecera, tabla, pie)
          val b:Int = mayor(partidaAuto(t , 9, vidas, 0, best, 2, cabecera, tabla, pie))(best)
          if (b >= 0) modoAutomatico(vidas-1, b, dif, cabecera, tabla, pie)
          else -b
        }
        case 3 => {
          val t:List[Int] = iniciaTablero(3)
          IU(t, 14, vidas, 0, best, cabecera, tabla, pie)
          val b:Int = mayor(partidaAuto(t , 14, vidas, 0, best, 3, cabecera, tabla, pie))(best)
          if (b >= 0) modoAutomatico(vidas-1, b, dif, cabecera, tabla, pie)
          else -b
        }
        case 4 => {
          val t:List[Int] = iniciaTablero(4)
          IU(t, 17, vidas, 0, best, cabecera, tabla, pie)
          val b:Int = mayor(partidaAuto(t , 17, vidas, 0, best, 4, cabecera, tabla, pie))(best)
          if (b >= 0) modoAutomatico(vidas-1, b, dif, cabecera, tabla, pie)
          else -b
        }
      }
    }
    else best
  }
  
  def main(args: Array[String]):Unit = {
    
    
    bienvenidaIU
    
    modo match{      
      //Modo Manual
      case 1 => {
        val dif= dificultad()
        val ventana = new JFrame("16384 en Scala")
        val cabecera = new JTextArea()
        cabecera.setText("")
        val pie = new JTextArea()
        pie.setText("")
        val tabla = new JTable(iniTableroIU(dif), iniColumnasIU(dif))
        
        ventana.getContentPane.add(new JScrollPane(cabecera), BorderLayout.NORTH)
        ventana.getContentPane.add(new JScrollPane(pie), BorderLayout.SOUTH)
        ventana.getContentPane.add(new JScrollPane(tabla), BorderLayout.CENTER)
        ventana.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
        ventana.setSize(new Dimension(600, 400))
        ventana.setLocationRelativeTo(null)
        ventana.setVisible(true)
    
        val b:Int= coin(3,0,dif, cabecera, tabla, pie)
        print("                       ---------         GAME OVER        ----------                                \n\n");
        println("Mejor Puntuacion: " + b)
      }
      
      //Modo Automatico
      case 2 =>{
        val dif= dificultad()
        val ventana = new JFrame("16384 en Scala (Modo Automático)")
        val cabecera = new JTextArea()
        cabecera.setText("")
        val pie = new JTextArea()
        pie.setText("")
        val tabla = new JTable(iniTableroIU(dif), iniColumnasIU(dif))
        
        ventana.getContentPane.add(new JScrollPane(cabecera), BorderLayout.NORTH)
        ventana.getContentPane.add(new JScrollPane(pie), BorderLayout.SOUTH)
        ventana.getContentPane.add(new JScrollPane(tabla), BorderLayout.CENTER)
        ventana.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
        ventana.setSize(new Dimension(600, 400))
        ventana.setLocationRelativeTo(null)
        ventana.setVisible(true)
    
        val b:Int= modoAutomatico(3,0,dif, cabecera, tabla, pie)
        print("                       ---------         GAME OVER        ----------                                \n\n");
        println("Mejor Puntuacion: " + b)
      }   
    }
  }
}