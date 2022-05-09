package com.yurwar.uni.photo.editor

object ModifiedCutFilter extends ModifiedCutFilterInterface {

  /** Функція для визначення середнього значення в межах
   * c * 𝞂 від центрального пікселя
   *
   * @param M     Масив значень каналу певного кольору навколо центрального пікселя
   * @param index Індекс центрального пікселя
   * @param value Значення c * 𝞂
   * @return Обчислене значення каналу
   */
  def searchNearSko(M: Array[Int], index: Int, value: Double): Int = {
    var sum = 0
    var count = 0
    for (i <- M.indices)
      yield {
        if (i != index) {
          if (math.abs(M(i) - M(index)) <= value) {
            // Якщо значення відрізнається від центрального на більш ніж c * 𝞂 - виконуємо додавання
            sum += M(i)
            count += 1
          }
        }
      }
    // Якщо не знайдено ні одного елемента який би задовільняв умову - повертаємо значення медіани
    if (count == 0) {
      M(index)
    } else {
      // Знаходимо середнє
      sum /= count
      sum
    }
  }

  /** Функція обчислення модифікованого обрізного фільтру
   *
   * @param src Вхідне зображення
   * @param dst Вихідне зображення
   */
  override def modifiedCut(src: Img, dst: Img): Unit = {
    var sigmaR, sigmaG, sigmaB = 0.0
    var brightnessR, brightnessG, brightnessB = 0.0
    //Для данного фільтру використовуємо вікно розміром 3 на 3 з константою с = 0.5
    //Збільшення константи приведе до збільшення числа пікселів, які будуть брати участь в знаходженні середнього
    //Уменьшение приведет к тому, что под действие фильтра будет попадать крайне малое число пикселей
    //Зменшення приведе до того, що під дію фільтра буде попадати крайньо мала кількість пікселів
    val c = 0.5
    var rc, gc, bc = 0

    //Знаходимо середнє значення насиченості каналів зображення
    for (x <- 0 until src.width;
         y <- 0 until src.height)
    yield {
      brightnessR += red(src(x, y))
      brightnessG += green(src(x, y))
      brightnessB += blue(src(x, y))
    }
    brightnessR /= src.width * src.height
    brightnessG /= src.width * src.height
    brightnessB /= src.width * src.height

    //Знаходимо дисперсію для всього зображення
    brightnessR = math.pow(brightnessR, 2)
    brightnessG = math.pow(brightnessG, 2)
    brightnessB = math.pow(brightnessB, 2)

    for (x <- 0 until src.width;
         y <- 0 until src.height)
    yield {
      sigmaR += math.pow(red(src(x, y)), 2) - brightnessR
      sigmaG += math.pow(green(src(x, y)), 2) - brightnessG
      sigmaB += math.pow(blue(src(x, y)), 2) - brightnessB
    }

    sigmaR /= src.width * src.height
    sigmaG /= src.width * src.height
    sigmaB /= src.width * src.height

    //Знаходимо корінь із дисперсії для знаходження відхилення
    sigmaR = math.sqrt(sigmaR)
    sigmaG = math.sqrt(sigmaG)
    sigmaB = math.sqrt(sigmaB)

    var count = 0
    val R = Array.fill(9) {
      0
    }
    val G = Array.fill(9) {
      0
    }
    val B = Array.fill(9) {
      0
    }
    for (x <- 1 until src.width - 1;
         y <- 1 until src.height - 1)
    yield {
      count = 0
      for (k <- -1 to 1;
           m <- -1 to 1)
      yield {
        R(count) = red(src(x + k, y + m))
        G(count) = green(src(x + k, y + m))
        B(count) = blue(src(x + k, y + m))
        count += 1
      }
      rc = searchNearSko(R, 4, c * sigmaR)
      gc = searchNearSko(G, 4, c * sigmaG)
      bc = searchNearSko(B, 4, c * sigmaB)
      dst.update(x, y, rgba(rc, gc, bc, alpha(src(x, y))))
    }
  }
}
