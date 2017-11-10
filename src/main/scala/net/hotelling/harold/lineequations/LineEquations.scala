package net.hotelling.harold.lineequations

import java.awt.{Color, Dimension, Graphics, Graphics2D}
import javax.swing.{BoxLayout, JFrame, JPanel, SwingUtilities}

/**
  * Interactive visual tool for playing with the equations for lines.
  */
object LineEquations extends Runnable {

  def main(args: Array[String]): Unit = {
    SwingUtilities.invokeLater(LineEquations)
  }

  override def run(): Unit = {
    val frame = new JFrame("Lines")
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

    val container = new JPanel()
    container.setLayout(new BoxLayout(container, BoxLayout.Y_AXIS))

    val lineGraph = new LineGraph()
    container.add(lineGraph)

    frame.getContentPane.add(container)
    frame.pack()
    frame.setLocationByPlatform(true)
    frame.setVisible(true)
  }
}

/**
  * Draw the x and y axes and a line that can be redrawn as the equation changes.
  */
class LineGraph(val sideLength: Int = 1024) extends JPanel {
  val axesColor = new Color(128, 128, 255)
  val backgroundColor = new Color(255, 255, 255)
  val c1 = new Color(255, 0, 0)
  val c2 = new Color(0, 255, 0)
  val c3 = new Color(0, 0, 255)
  val maxValue = 20
  val minValue = -maxValue
  val delta = sideLength / (2 * maxValue)

  override def getPreferredSize = new Dimension(sideLength, sideLength)

  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)
    implicit val g2d = g.asInstanceOf[Graphics2D]

    g2d.setColor(backgroundColor)
    g2d.fillRect(0, 0, getWidth, getHeight)

    drawAxes
    val l1 = VerticalLine(4.3)
    val l2 = SlopeInterceptLine(-2.3, 1.5)
    val l3 = SlopeInterceptLine(0.1, 0)
    drawLine(l1, c1)
    drawLine(l2, c2)
    drawLine(l3, c3)
    drawLabels(List((l1.toString, c1), (l2.toString, c2), (l3.toString, c3)))
  }

  def drawAxes(implicit g2d: Graphics2D): Unit = {
    g2d.setColor(axesColor)
    val tickSize = 20
    val x0 = getWidth / 2
    val y0 = getHeight / 2

    // X Axis
    g2d.drawLine(0, y0, getWidth, y0)
    (minValue to maxValue).foreach { value =>
      val x = x0 + (value * delta)
      g2d.drawLine(x, y0 - tickSize, x, y0 + tickSize)
    }

    // Y Axis
    g2d.drawLine(x0, 0, x0, getHeight)
    (minValue to maxValue).foreach { value =>
      val y = y0 + (value * delta)
      g2d.drawLine(x0 - tickSize, y, x0 + tickSize, y)
    }
  }

  def drawLine(line: Line, color: Color)(implicit g2d: Graphics2D): Unit = line match {
    case VerticalLine(x) => {
      val xPixels = xValueToPixels(x)
      g2d.setColor(color)
      g2d.drawLine(xPixels, 0, xPixels, getHeight)
    }
    case SlopeInterceptLine(m, b) => {
      val leftX = xValueToPixels(minValue)
      val leftY = yValueToPixels(m * minValue + b)
      val rightX = xValueToPixels(maxValue)
      val rightY = yValueToPixels(m * maxValue + b)
      g2d.setColor(color)
      g2d.drawLine(leftX, leftY, rightX, rightY)
    }
  }

  def drawLabels(labels: List[(String, Color)])(implicit g2d: Graphics2D): Unit = {
    val deltaX = 20
    val deltaY = 20
    labels.zipWithIndex.foreach {
      case ((label, color), index) =>
        val y = (index + 1) * deltaY
        g2d.setColor(color)
        g2d.drawString(label, deltaX, y)
    }
  }

  def xValueToPixels(x: Double): Int = ((getWidth / 2) + (x * delta)).toInt
  def yValueToPixels(y: Double): Int = ((getHeight / 2) - (y * delta)).toInt
}

sealed abstract class Line

case class VerticalLine(val x: Double) extends Line {
  override def toString: String = s"x = $x"
}

case class SlopeInterceptLine(val m: Double, val b: Double) extends Line {
  override def toString: String = s"y = $m * x + $b"
}
