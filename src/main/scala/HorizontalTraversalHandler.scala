package com.yurwar.uni.photo.editor

/**
 * Узагальнений об'єкт що містить функції для обходу зображення попіксельно по горизонталі
 */
object HorizontalTraversalHandler extends HorizontalTraversalInterface {

  /** Кількість паралельних задач одночасно */
  private val numTasks = 16


  /** Послідовно оброблює зображення за допомогою переданої функції горизонтально та попіксельно
   *
   * @param src     Вхідне зображення
   * @param dst     Вихідне зображення
   * @param handler Функція обробки пікселя
   */
  override def traverseSequential(src: Img, dst: Img, handler: (Img, Int, Int) => RGBA): Unit = {
    traverseInternal(src, dst, 0, src.height, handler)
  }

  /** Паралельно оброблює зображення розбиваючи його на горизонтальні смужки
   * та передає їх у відповідні задачі для обробки за допомогою переданої функції обробки
   *
   * @param src     Вхідне зображення
   * @param dst     Вихідне зображення
   * @param handler Функція обробки пікселя
   */
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
