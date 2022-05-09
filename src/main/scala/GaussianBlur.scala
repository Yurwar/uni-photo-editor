package com.yurwar.uni.photo.editor

import scala.math._

object GaussianBlur extends GaussianBlurInterface {

  /** Оброблює вхідне зображення за допомогою Гаусівського фільтру
   *
   * @param src      Вхідне зображення
   * @param dst      Вихідне зображення
   * @param radius   Радіус ядра
   * @param variance Значення дисперсії
   */
  override def blur(src: Img, dst: Img, radius: Int, variance: Double): Unit = {
    // Генеруємо зважену матрицю
    val weightedMatrix = generateWeightedMatrix(radius, variance)

    // Оброблюємо покроково кожен піксель вираховуючи його нове значення за допомогою функції Гауса
    // та отриманої зваженої матриці
    HorizontalTraversalHandler.traverse(src, dst, gaussianBlurKernel(radius, weightedMatrix))
  }

  /** Функція для генерації зваженої матриці заданого радіусу
   * використовується для економії обчислювальної потужності так як
   * дозволяє не обчислювати матрицю для кожного пікселя
   *
   * @param radius   Радіус ядра
   * @param variance Значення дисперсії
   * @return Зважена матриця за формулою Гауса
   */
  private def generateWeightedMatrix(radius: Int, variance: Double) = {
    val diameter = (radius * 2) + 1
    val matrix = Array.ofDim[Double](diameter, diameter)
    var gaussianSum = 0.0

    for (x <- 0 until diameter;
         y <- 0 until diameter)
    yield {
      val gaussianValue = calculateGaussianValue(x - radius, y - radius, variance)
      matrix(x)(y) = gaussianValue
      gaussianSum += gaussianValue
    }

    matrix.map(dim => dim.map(_ / gaussianSum))
  }

  /** Обчислює двовимірну функцію Гауса
   *
   * @param x        Відносне значення х
   * @param y        Відносне значення y
   * @param variance Дисперсія
   * @return Значення по Гаусу для заданої точки
   */
  private def calculateGaussianValue(x: Int, y: Int, variance: Double): Double = {
    (1 / (2 * Pi * pow(variance, 2))) * exp(-(pow(x, 2) + pow(y, 2)) / (2 * pow(variance, 2)))
  }
}
