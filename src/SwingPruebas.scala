/*import java.awt.BorderLayout
import java.awt.Dimension
import java.awt.Color
import java.awt.Event._
import javax.swing.JFrame
import javax.swing.JTextArea
import javax.swing.JTable
import javax.swing.JScrollPane
import javax.swing.JButton
import javax.swing.event._
import javax.swing._*/


import java.awt.Color
import scala.swing._
import scala.swing.event._

//import java.awt.Component
//import java.awt.event._

/*abstract class KeyListener {
  def keyPressed(event: KeyEvent)
}

trait KeyClickable {
  def component: Component

  def addKeyListener(kl: KeyListener) = component.addKeyListener(
    new KeyAdapter {
        override def keyPressed(e: KeyEvent) = kl.keyPressed(e)
    })
}

class KeyIn extends KeyClickable{
  
}*/
/*
class tecladillo extends JFrame("Tecladillo"){
  import javax.swing.JFrame._
  
  setDefaultCloseOperation(EXIT_ON_CLOSE);
  setSize(200,200)
  val button = new JButton("Hello world"); 
   button addActionListener Print("Hello world"); 
   getContentPane() add button; 

   pack; 
   setSize(400, 300); 
   setVisible(true); 
    
   import java.awt.event._; 
    
   case class Print(msg : String) 
    with ActionListener 
    with Application { 
      def actionPerformed(e : ActionEvent) = 
         Console println msg 
   }
}
*/
object SwingPruebas extends App {
  
  //-------------- Entrada de datos ------------------------
  
  
  
  //--------------------------------------------------------
  
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
  /*
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

    val tecladillo:JFrame = new JFrame("Tecladillo")
    tecladillo.setSize(200, 200)
    tecladillo.setLocationRelativeTo(null)
    tecladillo.setLocation(0, 200)
    tecladillo.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    tecladillo.setBackground(Color.DARK_GRAY)
    
    val botIzq:JButton = new JButton("<")
    botIzq.setBackground(Color.YELLOW)
    botIzq.setForeground(Color.BLUE)
    //botIzq addActionListener textArea2.setText("boton izq");
    tecladillo.getContentPane.add(botIzq, BorderLayout.WEST)
    
    val botDch:JButton = new JButton(">")
    botDch.setBackground(Color.BLUE)
    botDch.setForeground(Color.YELLOW)
    tecladillo.getContentPane.add(botDch, BorderLayout.EAST)
    
    val botArb:JButton = new JButton("^")
    botArb.setBackground(Color.RED)
    botArb.setForeground(Color.GREEN)
    tecladillo.getContentPane.add(botArb, BorderLayout.NORTH)
    
    val botAbj:JButton = new JButton("v")
    botAbj.setBackground(Color.GREEN)
    botAbj.setForeground(Color.RED)
    tecladillo.getContentPane.add(botAbj, BorderLayout.SOUTH)
    
    tecladillo.setVisible(true)
    
    //escuchaTeclas(textArea2, botIzq, botDch, botArb, botAbj)
    Thread.sleep(3000)*/
  
  val frame = new Frame{
    title = "Hello world"
    val status1 = new Button("1"){
      maximumSize = new Dimension(80,80)
      minimumSize = new Dimension(80,80)
      background = Color.RED
    }
    val status2 = new Button("2"){
      maximumSize = new Dimension(80,80)
      minimumSize = new Dimension(80,80)
      background = Color.YELLOW
    }
    val status3 = new Button("16384"){
      maximumSize = new Dimension(80,80)
      minimumSize = new Dimension(80,80)
      background = Color.BLUE
      foreground = Color.YELLOW
    }
    val box = new BoxPanel(Orientation.Horizontal) {
      
      contents += status1
      contents += status2
      contents += status3
    }
    
    contents = new BoxPanel(Orientation.Vertical) {
      contents += new Label("Launch rainbows:")
      contents += new Button("Click me") {
        reactions += {
          case ButtonClicked(_) =>
            println("All the colours!")
        }
      }
      contents += box
    }
    
    preferredSize = new Dimension(600, 400)
    pack()
    centerOnScreen()
    open()
  }

}