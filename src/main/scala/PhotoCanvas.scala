package com.yurwar.uni.photo.editor

import java.awt._
import java.awt.image._
import java.io._
import javax.imageio._
import javax.swing._
import scala.collection.mutable

/**
 * Клас що містить в собі зображення для відображення його у інтерфейсі користувача
 */
class PhotoCanvas extends JComponent {

  var imagePath: Option[String] = None

  var image: Img = loadDefaultImage()

  override def getPreferredSize: Dimension = {
    new Dimension(image.width, image.height)
  }

  /**
   * @return Завантажує зображення за замовчуванням
   */
  private def loadDefaultImage(): Img = {
    val stream = this.getClass.getResourceAsStream("/images/img.png")
    try {
      loadImage(stream)
    } finally {
      stream.close()
    }
  }

  /**
   * @param path Шлях до зображення
   * @return Зображення по заданому шляху
   */
  private def loadFileImage(path: String): Img = {
    val stream = new FileInputStream(path)
    try {
      loadImage(stream)
    } finally {
      stream.close()
    }
  }

  private def loadImage(inputStream: InputStream): Img = {
    val bufferedImage = ImageIO.read(inputStream)
    val width = bufferedImage.getWidth
    val height = bufferedImage.getHeight
    val img = new Img(width, height)
    for (x <- 0 until width; y <- 0 until height) img(x, y) = bufferedImage.getRGB(x, y)
    img
  }

  /**
   * Функція для оновлення зображення до початкового
   */
  def reload(): Unit = {
    image = imagePath match {
      case Some(path) => loadFileImage(path)
      case None => loadDefaultImage()
    }
    repaint()
  }

  def loadFile(path: String): Unit = {
    imagePath = Some(path)
    reload()
  }

  /** Функція для накладання обраного фільтру на поточне зображення
   *
   * @param filterName Назва фільтру
   * @param radius     Радіус для фільтру
   * @param threshold  Порогове значення для фільтру
   */
  def applyFilter(filterName: String, radius: Int, threshold: Int): Unit = {
    val dst = new Img(image.width, image.height)
    filterName match {
      case "negate" =>
        HorizontalTraversalHandler.traverseSequential(image, dst, negateKernel)
      case "binarize" =>
        HorizontalTraversalHandler.traverseSequential(image, dst, binarizeKernel(threshold))
      case "binarize-by-channels" =>
        HorizontalTraversalHandler.traverseSequential(image, dst, binarizeByChannelsKernel(threshold))
      case "grayscale" =>
        HorizontalTraversalHandler.traverseSequential(image, dst, grayscaleKernel)
      case "gaussian" =>
        GaussianBlur.blur(image, dst, radius, 1.5)
      case "modified-cut" =>
        ModifiedCutFilter.modifiedCut(image, dst)
    }
    image = dst
    repaint()
  }

  /**
   * @return Статистика частот кольорів для побудови багатоканальної гістограми
   */
  def getColorStat: Map[String, mutable.Map[Int, Int]] = {
    val reds = mutable.Map.empty[Int, Int].withDefaultValue(0)
    for (i <- 0 to 255) {
      reds(i) = 0
    }
    val greens = mutable.Map.empty[Int, Int].withDefaultValue(0)
    for (i <- 0 to 255) {
      greens(i) = 0
    }
    val blues = mutable.Map.empty[Int, Int].withDefaultValue(0)
    for (i <- 0 to 255) {
      blues(i) = 0
    }
    for (row <- 0 until image.width;
         col <- 0 until image.height if col >= 0 && col <= image.height)
    yield {
      val rgba = image(row, col)
      val r = red(rgba)
      val g = green(rgba)
      val b = blue(rgba)
      reds(r) += 1
      greens(g) += 1
      blues(b) += 1
    }
    Map("red" -> reds, "green" -> greens, "blue" -> blues)
  }

  /**
   * @return Статистика частот кольорів для побудови гістограми відтінків сірого
   */
  def getGrayscaleStat: mutable.Map[Int, Int] = {
    val grays = mutable.Map.empty[Int, Int].withDefaultValue(0)
    for (i <- 0 to 255) {
      grays(i) = 0
    }
    val grayscaleImg = new Img(image.width, image.height)
    HorizontalTraversalHandler.traverse(image, grayscaleImg, grayscaleKernel)
    for (row <- 0 until grayscaleImg.width;
         col <- 0 until grayscaleImg.height if col >= 0 && col <= grayscaleImg.height)
    yield {
      val rgba = image(row, col)
      val gray = brightness(rgba)
      grays(gray) += 1
    }
    grays
  }


  override def paintComponent(gcan: Graphics): Unit = {
    super.paintComponent(gcan)

    val width = image.width
    val height = image.height
    val bufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    for (x <- 0 until width; y <- 0 until height) bufferedImage.setRGB(x, y, image(x, y))

    gcan.drawImage(bufferedImage, 0, 0, null)
  }

}
