package com.yurwar.uni.photo.editor

import org.jfree.chart.plot.{PlotOrientation, XYPlot}
import org.jfree.chart.renderer.xy.SamplingXYLineRenderer
import org.jfree.chart.{ChartFactory, ChartPanel, JFreeChart}
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}

import java.awt._
import java.awt.event._
import javax.swing._

object UniPhotoEditor {

  /**
   * Клас головного вікна застосунку
   */
  class UniPhotoEditorFrame extends JFrame("UniPhotoEditor\uD83C\uDDFA\uD83C\uDDE6") {
    setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    setSize(1600, 896)
    setLayout(new BorderLayout)

    // Ініціалізація панелі з фільтрами та контролем над ними
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
      "binarize",
      "binarize-by-channels",
      "grayscale",
      "negate",
      "gaussian",
      "modified-cut"
    ))
    controls.add(filterCombo)

    val radiusLabel = new JLabel("Radius")
    controls.add(radiusLabel)

    val radiusSpinner = new JSpinner(new SpinnerNumberModel(3, 1, 16, 1))
    controls.add(radiusSpinner)

    val thresholdLabel = new JLabel("Threshold")
    controls.add(thresholdLabel)

    val thresholdSlider = new JSlider(new DefaultBoundedRangeModel(50, 1, 0, 101))
    controls.add(thresholdSlider)

    val stepbutton = new JButton("Apply filter")

    stepbutton.addActionListener((_: ActionEvent) => {
      canvas.applyFilter(getFilterName, getRadius, getThreshold)
      // Оновлює гістраграми після накладання фільтру
      updateHistograms()
    })
    controls.add(stepbutton)

    val clearButton = new JButton("Reload")
    clearButton.addActionListener((_: ActionEvent) => {
      canvas.reload()
      // Оновлює гістраграми після перезавантаження зображення
      updateHistograms()
    })
    controls.add(clearButton)

    val info = new JTextArea("   ")
    info.setBorder(BorderFactory.createLoweredBevelBorder)
    info.setEditable(false)
    rightpanel.add(info, BorderLayout.SOUTH)

    // Побудова панелі з гістограмами
    val leftpannel = new JPanel
    leftpannel.setBorder(BorderFactory.createEtchedBorder(border.EtchedBorder.RAISED))
    leftpannel.setLayout(new BorderLayout)
    add(leftpannel, BorderLayout.WEST)

    val histograms = new JPanel
    histograms.setLayout(new GridLayout(2, 1))
    leftpannel.add(histograms, BorderLayout.NORTH)

    // Побудова панелі верхнього меню
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

    // Побудова основного вікна з зображенням
    val canvas = new PhotoCanvas
    // Додавання слухача подій який відображає значення яскравості для пікселя
    // на який наведена мишка
    canvas.addMouseMotionListener(new MouseMotionAdapter {
      override def mouseMoved(e: MouseEvent): Unit = {
        val x = e.getX
        val y = e.getY
        if (x < canvas.image.width && y < canvas.image.height) {
          val rgba = canvas.image.apply(x, y)
          val brightness = 0.3 * red(rgba) + 0.59 * green(rgba) + 0.11 * blue(rgba)
          updatePointBrightnessBox(brightness, x, y)
        }
      }
    })

    val scrollPane = new JScrollPane(canvas)

    add(scrollPane, BorderLayout.CENTER)

    // Побудова кольорової гістограми
    val colorHistogram: ChartPanel = getColorHistogram
    histograms.add(colorHistogram)

    // Побудова гістограми градацій сірого
    val grayscaleHistogram: ChartPanel = getGrayscaleHistogram
    histograms.add(grayscaleHistogram)

    setVisible(true)

    // Оновлення текстового поля яскравості
    def updatePointBrightnessBox(brightness: Double, x: Int, y: Int): Unit = {
      info.setText(f"Brightness of current point: $brightness%1.2f, x = $x; y = $y")
    }

    def getRadius: RGBA = radiusSpinner.getValue.asInstanceOf[RGBA]

    def getThreshold: RGBA = thresholdSlider.getValue

    def getFilterName: String = {
      filterCombo.getSelectedItem.asInstanceOf[String]
    }

    private def updateHistograms(): Unit = {
      grayscaleHistogram.getChart.getPlot.asInstanceOf[XYPlot].setDataset(collectGrayscaleDataset)
      colorHistogram.getChart.getPlot.asInstanceOf[XYPlot].setDataset(collectColorDataset)
    }

    // Метод що будує гістаграму градацій сірого
    private def getGrayscaleHistogram: ChartPanel = {
      val grayscaleDataset: XYSeriesCollection = collectGrayscaleDataset

      val grayscaleChart: JFreeChart = ChartFactory.createHistogram(
        "Grayscale histogram",
        "Gray value",
        "Pixel count",
        grayscaleDataset,
        PlotOrientation.VERTICAL,
        false,
        false,
        false
      )
      val grayscalePlot: XYPlot = grayscaleChart.getXYPlot
      val grayscaleRenderer: SamplingXYLineRenderer = new SamplingXYLineRenderer()
      grayscaleRenderer.setSeriesPaint(0, Color.GRAY)
      grayscaleRenderer.setSeriesStroke(0, new BasicStroke(1.2f))
      grayscalePlot.setRenderer(grayscaleRenderer)
      val grayscaleChartPanel = new ChartPanel(grayscaleChart)
      grayscaleChartPanel.setPreferredSize(new Dimension(300, 300))
      grayscaleChartPanel
    }

    private def collectGrayscaleDataset = {
      val graySeries = new XYSeries("Gray")
      canvas.getGrayscaleStat
        .foreachEntry((value, count) => graySeries.add(value, count))

      val grayscaleDataset = new XYSeriesCollection
      grayscaleDataset.addSeries(graySeries)
      grayscaleDataset
    }

    // Метод що будує гістаграму по трьом складовим RGB
    private def getColorHistogram: ChartPanel = {
      val dataset: XYSeriesCollection = collectColorDataset

      val xyChart = ChartFactory.createHistogram(
        "Color histograms",
        "Color value",
        "Pixel count",
        dataset,
        PlotOrientation.VERTICAL,
        false,
        false,
        false
      )

      val plot: XYPlot = xyChart.getXYPlot
      val renderer: SamplingXYLineRenderer = new SamplingXYLineRenderer()
      renderer.setSeriesPaint(0, Color.RED)
      renderer.setSeriesPaint(1, Color.GREEN)
      renderer.setSeriesPaint(2, Color.BLUE)
      renderer.setSeriesStroke(0, new BasicStroke(1.2f))
      renderer.setSeriesStroke(1, new BasicStroke(1.2f))
      renderer.setSeriesStroke(2, new BasicStroke(1.2f))
      plot.setRenderer(renderer)

      val chartPanel = new ChartPanel(xyChart)
      chartPanel.setPreferredSize(new Dimension(300, 300))
      chartPanel
    }

    private def collectColorDataset = {
      val redSeries = new XYSeries("Red")
      val greenSeries = new XYSeries("Green")
      val blueSeries = new XYSeries("Blue")
      canvas.getColorStat("red")
        .foreachEntry((value, count) => redSeries.add(value, count))
      canvas.getColorStat("green")
        .foreachEntry((value, count) => greenSeries.add(value, count))
      canvas.getColorStat("blue")
        .foreachEntry((value, count) => blueSeries.add(value, count))

      val dataset = new XYSeriesCollection
      dataset.addSeries(redSeries)
      dataset.addSeries(greenSeries)
      dataset.addSeries(blueSeries)
      dataset
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
