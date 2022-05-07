package com.yurwar.uni.photo.editor

object HorizontalTraversalHandler extends HorizontalTraversalInterface {
  private def numTasks = 16


  override def traverseSequential(src: Img, dst: Img, handler: (Img, Int, Int) => RGBA): Unit = {
    traverseInternal(src, dst, 0, src.height, handler)
  }

  override def traverse(src: Img, dst: Img, handler: (Img, Int, Int) => RGBA): Unit = {
    val strips = 0 to src.height by (src.height / numTasks max 1)

    strips.zip((if (strips.last != src.height) strips.appended(src.height) else strips).tail)
      .map({
        case (from, to) => task {
          traverseInternal(src, dst, from, to, handler)
        }
      }).foreach(_.join())
  }

  private def traverseInternal(src: Img, dst: Img, from: Int, end: Int, handler: (Img, Int, Int) => RGBA) = {
    for (row <- from until end;
         col <- 0 until src.width)
    yield
      dst.update(col, row, handler(src, col, row))
  }
}
