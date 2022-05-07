package com.yurwar.uni.photo

import java.util.concurrent._
import scala.util.DynamicVariable

package object editor extends BoxBlurKernelInterface
  with NegateKernelInterface
  with BinarizeKernelInterface
  with GrayscaleKernelInterface
  with GaussianBlurKernelInterface {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def alpha(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def red(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def green(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def blue(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (a << 24) | (r << 16) | (g << 8) | (b << 0)
  }

  def brightness(rgba: RGBA): Int = {
    (0.3 * red(rgba) + 0.59 * green(rgba) + 0.11 * blue(rgba)).round.toInt
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  override def boxBlurKernel(radius: Int)(src: Img, x: Int, y: Int): RGBA = {
    val minX = clamp(x - radius, 0, src.width - 1)
    val minY = clamp(y - radius, 0, src.height - 1)
    val maxX = clamp(x + radius, 0, src.width - 1)
    val maxY = clamp(y + radius, 0, src.height - 1)

    var r = 0
    var g = 0
    var b = 0
    var a = 0

    var xi = minX
    var yi = minY

    var pixelCounter = 0

    while (xi <= maxX) {
      while (yi <= maxY) {
        val currPixel = src(xi, yi)
        r += red(currPixel)
        g += green(currPixel)
        b += blue(currPixel)
        a += alpha(currPixel)
        pixelCounter += 1
        yi += 1
      }
      xi += 1
      yi = minY
    }
    rgba(r / pixelCounter, g / pixelCounter, b / pixelCounter, a / pixelCounter)
  }


  override def gaussianBlurKernel(radius: Int, weightedMatrix: Array[Array[Double]])(src: Img, x: Int, y: Int): RGBA = {
    var r = 0
    var g = 0
    var b = 0
    for (weightX <- weightedMatrix.indices;
         weightY <- weightedMatrix(weightX).indices)
    yield {
      var sampleX = x + weightX - radius
      var sampleY = y + weightY - radius
      if (sampleX > src.width - 1) {
        sampleX = 2 * src.width - 1 - sampleX
      }
      if (sampleY > src.height - 1) {
        sampleY = 2 * src.height - 1 - sampleY
      }
      if (sampleX < 0) {
        sampleX = math.abs(sampleX)
      }
      if (sampleY < 0) {
        sampleY = math.abs(sampleY)
      }
      val currWeight = weightedMatrix(weightX)(weightY)
      val currPixel = src(sampleX, sampleY)
      r += (currWeight * red(currPixel)).round.toInt
      g += (currWeight * green(currPixel)).round.toInt
      b += (currWeight * blue(currPixel)).round.toInt
    }

    rgba(r, g, b, alpha(src(x, y)))
  }

  override def negateKernel(src: Img, x: Int, y: Int): RGBA = {
    val currPixel = src(x, y)
    val r = 255 - red(currPixel)
    val g = 255 - green(currPixel)
    val b = 255 - blue(currPixel)
    val a = alpha(currPixel)

    rgba(r, g, b, a)
  }


  override def binarizeKernel(threshold: Int)(src: Img, x: Int, y: Int): RGBA = {
    val currPixel = src(x, y)
    val currPixelSum = red(currPixel) + green(currPixel) + blue(currPixel)
    val rgbThreshold = (255 * 3) * (threshold / 100.0)
    if (currPixelSum <= rgbThreshold) {
      rgba(0, 0, 0, alpha(currPixel))
    } else {
      rgba(255, 255, 255, alpha(currPixel))
    }
  }


  override def binarizeByChannelsKernel(threshold: Int)(src: Img, x: Int, y: Int): RGBA = {
    val currPixel = src(x, y)
    val colourThreshold = 255 * (threshold / 100.0)
    val r = if (red(currPixel) <= colourThreshold) 0 else 255
    val g = if (green(currPixel) <= colourThreshold) 0 else 255
    val b = if (blue(currPixel) <= colourThreshold) 0 else 255

    rgba(r, g, b, alpha(currPixel))
  }


  override def grayscaleKernel(src: Img, x: Int, y: Int): RGBA = {
    val currPixel = src(x, y)
    val b = brightness(currPixel)

    rgba(b, b, b, alpha(currPixel))
  }

  val forkJoinPool = new ForkJoinPool

  abstract class TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T]
  }

  class DefaultTaskScheduler extends TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T] = {
      val t = new RecursiveTask[T] {
        def compute: T = body
      }
      Thread.currentThread match {
        case _: ForkJoinWorkerThread =>
          t.fork()
        case _ =>
          forkJoinPool.execute(t)
      }
      t
    }
  }

  val scheduler =
    new DynamicVariable[TaskScheduler](new DefaultTaskScheduler)

  def task[T](body: => T): ForkJoinTask[T] = {
    scheduler.value.schedule(body)
  }
}
