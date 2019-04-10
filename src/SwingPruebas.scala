import java.awt.BorderLayout
import java.awt.Dimension
import java.awt.Color
import javax.swing.JFrame
import javax.swing.JScrollPane
import javax.swing.JTextArea
import javax.swing.JTable

object SwingPruebas extends App {
  
  def setLista(pos:Int, n:Int, l:List[Int]):List[Int] ={
  	if (l.length == 0) Nil
  	else
  		if (pos == 1)
  			n::l.tail
  		else
  			l.head::setLista(pos-1, n, l.tail)
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

    val textArea1 = new JTextArea
    textArea1.setText("Hello, Swing world")
    val scrollPane1 = new JScrollPane(textArea1)
    
    val textArea2 = new JTextArea
    textArea2.setText("Hello, Swing world")
    val scrollPane2 = new JScrollPane(textArea2)
    textArea2.setBackground(Color.BLUE)
    textArea2.setForeground(Color.GREEN)
    textArea2.append(" Ueeeejeeeee ")
    textArea2.append(BorderLayout.NORTH)
    
    val s:Int = 4
    val columnas:Array[Object] = tituloVacioColumnas(s)
    val tab:List[Int] = List.fill(s*s)(0)
    val datos:Array[Array[Object]] = ListADobleArray(tab, s)

    val tabla = new JTable(datos, columnas)
    val scrollPane3 = new JScrollPane(tabla)
    
    val frame = new JFrame("Hello, Swing")
    frame.getContentPane.add(scrollPane1, BorderLayout.NORTH)
    frame.getContentPane.add(scrollPane2, BorderLayout.SOUTH)
    frame.getContentPane.add(scrollPane3, BorderLayout.CENTER)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setSize(new Dimension(600, 400))
    frame.setLocationRelativeTo(null)
    frame.setVisible(true)
    
    Thread.sleep(2000)
    
    val tab2:List[Int] = setLista(3, 4, setLista(7, 2, setLista(1, 2, setLista(12, 5, setLista(11, 6, tab)))))
    
    actualizaTablaAux(tabla, tab2, s)(1,1)
    //tabla.setValueAt("4", 1, 3)
    //tabla.setValueAt("2", 2, 3)
    //frame.getContentPane.add(new JScrollPane(new JTable(ListADobleArray(tab2, s), columnas)), BorderLayout.CENTER)

}