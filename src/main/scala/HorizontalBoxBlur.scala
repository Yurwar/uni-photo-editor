package com.yurwar.uni.photo.editor

/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur extends HorizontalBoxBlurInterface {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
   * starting with `from` and ending with `end` (non-inclusive).
   *
   * Within each row, `blur` traverses the pixels by going from left to right.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    for (row <- 0 until src.width;
         col <- from until end if col >= 0 && col <= src.height)
    yield
      dst.update(row, col, boxBlurKernel(radius)(src, row, col))
  }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
   *
   * Parallelization is done by stripping the source image `src` into
   * `numTasks` separate strips, where each strip is composed of some number of
   * rows.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val strips = 0 to src.height by (src.height / numTasks max 1)

    strips.zip(strips.tail).map({ case (from, to) => task {
      blur(src, dst, from, to, radius)
    }
    }).foreach(_.join())
  }

}
