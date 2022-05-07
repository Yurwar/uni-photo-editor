package com.yurwar.uni.photo.editor

trait HorizontalTraversalInterface {
  def traverse(src: Img, dst: Img,
               handler: (Img, Int, Int) => RGBA): Unit
  def traverseSequential(src: Img, dst: Img,
                         handler: (Img, Int, Int) => RGBA): Unit
}

trait HorizontalBoxBlurInterface {
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit
}

trait VerticalBoxBlurInterface {
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit
}

trait GaussianBlurInterface {
  def blur(src: Img, dst: Img, radius: Int, variance: Double): Unit
}

trait BoxBlurKernelInterface {
  def boxBlurKernel(radius: Int)(src: Img, x: Int, y: Int): RGBA
}

trait NegateKernelInterface {
  def negateKernel(src: Img, x: Int, y: Int): RGBA
}

trait BinarizeKernelInterface {
  def binarizeKernel(threshold: Int)(src: Img, x: Int, y: Int): RGBA
  def binarizeByChannelsKernel(threshold: Int)(src: Img, x: Int, y: Int): RGBA
}

trait GrayscaleKernelInterface {
  def grayscaleKernel(src: Img, x: Int, y: Int): RGBA
}

trait GaussianBlurKernelInterface {
  def gaussianBlurKernel(radius: Int, weightedMatrix: Array[Array[Double]])(src: Img, x: Int, y: Int): RGBA
}