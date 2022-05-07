package com.yurwar.uni.photo.editor

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur extends VerticalBoxBlurInterface {

  /** Blurs the columns of the source image `src` into the destination image
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    for (row <- from until end if row >= 0 && row <= src.width;
         col <- 0 until src.height)
    yield
      dst.update(row, col, boxBlurKernel(radius)(src, row, col))
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val strips = 0 to src.width by (src.width / numTasks max 1)

    strips
      .zip(strips.tail)
      .map {
        case (from, to) => task {blur(src, dst, from, to, radius)}
      }
      .foreach(_.join())
  }

}
