package com.yurwar.uni.photo.editor

/**
 * Інтерфейс для методів горизонтального обходу зображення
 */
trait HorizontalTraversalInterface {
  def traverse(src: Img, dst: Img,
               handler: (Img, Int, Int) => RGBA): Unit
  def traverseSequential(src: Img, dst: Img,
                         handler: (Img, Int, Int) => RGBA): Unit
}

/**
 * Інтерфейс для методів Гаусівського фільтру
 */
trait GaussianBlurInterface {
  def blur(src: Img, dst: Img, radius: Int, variance: Double): Unit
}

/**
 * Інтерфейс для методів негативу
 */
trait NegateKernelInterface {
  def negateKernel(src: Img, x: Int, y: Int): RGBA
}

/**
 * Інтерфейс для методів бінаризації
 */
trait BinarizeKernelInterface {
  def binarizeKernel(threshold: Int)(src: Img, x: Int, y: Int): RGBA
  def binarizeByChannelsKernel(threshold: Int)(src: Img, x: Int, y: Int): RGBA
}

/**
 * Інтерфейс для методів перетворення у відтінки сірого
 */
trait GrayscaleKernelInterface {
  def grayscaleKernel(src: Img, x: Int, y: Int): RGBA
}

/**
 * Інтерфейс для методів розмиття по Гаусу ядра зображення
 */
trait GaussianBlurKernelInterface {
  def gaussianBlurKernel(radius: Int, weightedMatrix: Array[Array[Double]])(src: Img, x: Int, y: Int): RGBA
}

/**
 * Інтерфейс для методів модифікованого обрізного фільтру
 */
trait ModifiedCutFilterInterface {
  def modifiedCut(src: Img, dst: Img): Unit
}