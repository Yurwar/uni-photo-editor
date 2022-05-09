package com.yurwar.uni.photo

import java.util.concurrent._
import scala.util.DynamicVariable

package object editor extends NegateKernelInterface
  with BinarizeKernelInterface
  with GrayscaleKernelInterface
  with GaussianBlurKernelInterface {

  /** Значення кожного пікселю у вигляді цілочисельного 32 бітного значення. */
  type RGBA = Int

  /** Повертає канал альфа */
  def alpha(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Повертає червоний канал. */
  def red(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Повертає зелений канал. */
  def green(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Повертає блакитний канал. */
  def blue(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Конструктор для створення значення RGBA за допомогою окремих значеннь */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (a << 24) | (r << 16) | (g << 8) | (b << 0)
  }

  /** Повертає значення яскравості для конкретного пікселю */
  def brightness(rgba: RGBA): Int = {
    (0.3 * red(rgba) + 0.59 * green(rgba) + 0.11 * blue(rgba)).round.toInt
  }

  /** Конструктор зобреження, створює двовимірний масив пікселів для заданих параметрів
   *
   * @param width  Ширина зображення в пікселях
   * @param height Висота зображення в пікселях
   * */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))


    /** Повертає піксель по заданим координатам
     *
     * @param x координата по горизонталі
     * @param y координата по вертикалі
     * @return значення RGBA пікселю
     */
    def apply(x: Int, y: Int): RGBA = data(y * width + x)

    /** Оновлює значення пікселю по заданим координатам
     *
     * @param x координата по горизонталі
     * @param y координата по вертикалі
     * @param c нове значення пікселю
     */
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Функція розмиття по Гаусу для окремого пікселя за координатами x та y
   *
   * @param radius         Радіус розмиття
   * @param weightedMatrix Зважена матриця за розподілом Гауса для заданого радіусу
   * @param src            Вхідне зображення
   * @param x              Координата по горизонталі для пікселя
   * @param y              Координата по вертикалі для пікселя */
  override def gaussianBlurKernel(radius: Int, weightedMatrix: Array[Array[Double]])
                                 (src: Img, x: Int, y: Int): RGBA = {
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

  /** Функція для обчислення негативного значення пікселю
   *
   * @param src Вхідне зображення
   * @param x   Координата по горизонталі для пікселя
   * @param y   Координата по вертикалі для пікселя
   * @return Значення RGBA для пікселя в негативі
   */
  override def negateKernel(src: Img, x: Int, y: Int): RGBA = {
    val currPixel = src(x, y)
    val r = 255 - red(currPixel)
    val g = 255 - green(currPixel)
    val b = 255 - blue(currPixel)
    val a = alpha(currPixel)

    rgba(r, g, b, a)
  }


  /** Виконує бінаризацію до чорно-білого для заданого пікселя
   *
   * @param threshold Порогове значення бінаризації у відсотках
   * @param src       Вхідне зображення
   * @param x         Координата по горизонталі для пікселя
   * @param y         Координата по вертикалі для пікселя
   * @return Значення пікселя бінаризованого до чорно-білого
   */
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

  /** Виконує бінаризацію окремо за каналами для заданого пікселя
   *
   * @param threshold Порогове значення бінаризації у відсотках
   * @param src       Вхідне зображення
   * @param x         Координата по горизонталі для пікселя
   * @param y         Координата по вертикалі для пікселя
   * @return Значення пікселя бінаризованого по каналам
   */
  override def binarizeByChannelsKernel(threshold: Int)(src: Img, x: Int, y: Int): RGBA = {
    val currPixel = src(x, y)
    val colourThreshold = 255 * (threshold / 100.0)
    val r = if (red(currPixel) <= colourThreshold) 0 else 255
    val g = if (green(currPixel) <= colourThreshold) 0 else 255
    val b = if (blue(currPixel) <= colourThreshold) 0 else 255

    rgba(r, g, b, alpha(currPixel))
  }


  /** Обчислює значення відтінку сірого для обраного пікселя
   *
   * @param src Вхідне зображення
   * @param x   Координата по горизонталі для пікселя
   * @param y   Координата по вертикалі для пікселя
   * @return Значення пікселя у відтінку сірого
   */
  override def grayscaleKernel(src: Img, x: Int, y: Int): RGBA = {
    val currPixel = src(x, y)
    val b = brightness(currPixel)

    rgba(b, b, b, alpha(currPixel))
  }

  val forkJoinPool = new ForkJoinPool

  abstract class TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T]
  }

  /**
   * Планувальник для запуску задач у багатопоточному режимі
   */
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

  /** Передає задачу до планувальника для початку її обробки у паралельному режимі
   *
   * @param body Тип задачі
   * @tparam T Клас результату виконання задачі
   * @return Задача яка виконується паралельно
   */
  def task[T](body: => T): ForkJoinTask[T] = {
    scheduler.value.schedule(body)
  }
}
