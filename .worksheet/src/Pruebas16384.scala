object Pruebas16384 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(64); 
  println("Welcome to the Scala workshit");$skip(43); 
  // Tableros cuadrados
  var size:Int = 4;System.out.println("""size  : Int = """ + $show(size ));$skip(53); 
  
  var lista:List[Int] = List.fill(size * size)(0);System.out.println("""lista  : List[Int] = """ + $show(lista ));$skip(220); 
  
  def imprimir(l: List[Int], s:Int): Unit = {
  	if (!l.isEmpty)
  		(l.length % s) match {
  			case 1 => {println(l.head); imprimir(l.tail, s);}
  			case _ => {print(l.head + "\t"); imprimir(l.tail, s);}
  		}
  };System.out.println("""imprimir: (l: List[Int], s: Int)Unit""");$skip(27); 
  
  imprimir(lista, size);$skip(199); 
  
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
  };System.out.println("""getFicha: (x: Int, y: Int, l: List[Int], s: Int)Int""");$skip(80); 
  
  println(getFicha(2, 3, lista, size) + " - " + getFicha(3, 2, lista, size));$skip(266); 
  
  def movIzq(l:List[Int], s:Int):List[Int] = {
  	if (l.isEmpty) Nil
  	else
  		if ((l.head == 0) || ((l.length % s) == 1)) l.head::movIzq(l.tail, s)
  		else
  			if (obtener(2, l) == 0) 0::movIzq(l.head::l.tail.tail, s)
  			else l.head::movIzq(l.tail, s)
  };System.out.println("""movIzq: (l: List[Int], s: Int)List[Int]""");$skip(46); 
  
  lista = introFicha(2, 3, 2, lista, size);$skip(43); 
  lista = introFicha(1, 2, 8, lista, size);$skip(27); 
  
  imprimir(lista, size);$skip(30); 
  lista = movIzq(lista, size);$skip(24); 
  imprimir(lista, size)}
}
