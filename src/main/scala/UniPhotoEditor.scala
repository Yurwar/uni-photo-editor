package com.yurwar.uni.photo.editor

import java.awt._
import java.awt.event._
import javax.swing._

object UniPhotoEditor {

  class UniPhotoEditorFrame extends JFrame("UniPhotoEditor\u2122") {
    setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    setSize(1024, 600)
    setLayout(new BorderLayout)

    val rightpanel = new JPanel
    rightpanel.setBorder(BorderFactory.createEtchedBorder(border.EtchedBorder.LOWERED))
    rightpanel.setLayout(new BorderLayout)
    add(rightpanel, BorderLayout.EAST)

    val controls = new JPanel
    controls.setLayout(new GridLayout(0, 2))
    rightpanel.add(controls, BorderLayout.NORTH)

    val filterLabel = new JLabel("Filter")
    controls.add(filterLabel)

    val filterCombo = new JComboBox(Array(
      "horizontal-box-blur",
      "vertical-box-blur"
    ))
    controls.add(filterCombo)

    val radiusLabel = new JLabel("Radius")
    controls.add(radiusLabel)

    val radiusSpinner = new JSpinner(new SpinnerNumberModel(3, 1, 16, 1))
    controls.add(radiusSpinner)

    val stepbutton = new JButton("Apply filter")
    stepbutton.addActionListener((_: ActionEvent) => {
      val numTasks = 32
      canvas.applyFilter(getFilterName, numTasks, getRadius)
    })
    controls.add(stepbutton)

    val clearButton = new JButton("Reload")
    clearButton.addActionListener((_: ActionEvent) => {
      canvas.reload()
    })
    controls.add(clearButton)

    val info = new JTextArea("   ")
    info.setBorder(BorderFactory.createLoweredBevelBorder)
    rightpanel.add(info, BorderLayout.SOUTH)

    val leftpannel = new JPanel
    leftpannel.setBorder(BorderFactory.createEtchedBorder(border.EtchedBorder.RAISED))
    leftpannel.setLayout(new BorderLayout)
    add(leftpannel, BorderLayout.WEST)

    val histograms = new JPanel
    histograms.setLayout(new GridLayout(4, 1))
    leftpannel.add(histograms, BorderLayout.NORTH)

    val testLabel = new JLabel("Test")
    histograms.add(testLabel)

    val mainMenuBar = new JMenuBar()

    val fileMenu = new JMenu("File")
    val openMenuItem = new JMenuItem("Open...")
    openMenuItem.addActionListener((_: ActionEvent) => {
      val fc = new JFileChooser()
      if (fc.showOpenDialog(UniPhotoEditorFrame.this) == JFileChooser.APPROVE_OPTION) {
        canvas.loadFile(fc.getSelectedFile.getPath)
      }
    })
    fileMenu.add(openMenuItem)
    val exitMenuItem = new JMenuItem("Exit")
    exitMenuItem.addActionListener((e: ActionEvent) => {
      sys.exit(0)
    })
    fileMenu.add(exitMenuItem)

    mainMenuBar.add(fileMenu)

    val helpMenu = new JMenu("Help")
    val aboutMenuItem = new JMenuItem("About")
    aboutMenuItem.addActionListener((_: ActionEvent) => {
      JOptionPane.showMessageDialog(null, "UniPhotoEditor, sample university project for editing photos")
    })
    helpMenu.add(aboutMenuItem)

    mainMenuBar.add(helpMenu)

    setJMenuBar(mainMenuBar)

    val canvas = new PhotoCanvas

    val scrollPane = new JScrollPane(canvas)

    add(scrollPane, BorderLayout.CENTER)
    setVisible(true)

    def updateInformationBox(time: Double): Unit = {
      info.setText(s"Time: $time")
    }

    def getRadius: Int = radiusSpinner.getValue.asInstanceOf[Int]

    def getFilterName: String = {
      filterCombo.getSelectedItem.asInstanceOf[String]
    }

  }

  try {
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
  } catch {
    case _: Exception => println("Cannot set look and feel, using the default one.")
  }

  val frame = new UniPhotoEditorFrame

  def main(args: Array[String]): Unit = {
    frame.repaint()
  }

}
