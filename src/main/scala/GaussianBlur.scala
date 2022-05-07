package com.yurwar.uni.photo.editor

import scala.math._

object GaussianBlur extends GaussianBlurInterface {

  override def blur(src: Img, dst: Img, radius: Int, variance: Double): Unit = {
    val weightedMatrix = generateWeightedMatrix(radius, variance)

    HorizontalTraversalHandler.traverse(src, dst, gaussianBlurKernel(radius, weightedMatrix))
  }

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

  private def calculateGaussianValue(x: Int, y: Int, variance: Double): Double = {
    (1 / (2 * Pi * pow(variance, 2))) * exp(-(pow(x, 2) + pow(y, 2)) / (2 * pow(variance, 2)))
  }
}
