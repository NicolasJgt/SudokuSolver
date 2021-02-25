import scala.util.Random

class Sudoku (startConfig_ : Array[Array[Int]]){

  var modele = startConfig_






  def solver(): Array[Array[Int]] = {

    fillXY(0,0)

    val workGrid = this.modele
    workGrid
  }


  def fillXY(coupleXY: Tuple2[Int, Int]) : Boolean = {

    val col = coupleXY._2
    val ligne = coupleXY._1

    if(col ==  8 && ligne == 8){
      return true
    }else if (this.modele(ligne)(col) !=0) {
      return fillXY(nextPosition(ligne, col))
    }else {
      for (i <- 1 to 9) {
        //print(this.modele(ligne).mkString("\n"," ","\n"))
        if (isPossibleAt(i, ligne, col)) {
          this.modele(ligne)(col) = i
          if (fillXY(nextPosition(ligne, col))) {
            return true
          }
          this.modele(ligne)(col) = 0

        }
      }
    }
    false

  }
  def nextPosition( x_ : Int , y_ : Int) : Tuple2[Int,Int] = {
    var x = x_
    var y = y_ + 1
    if(y == 9){
      y = 0
      x += 1
    }
    (x,y)
  }


  def isPossibleAt(number_ : Int , x_ : Int , y_ : Int) : Boolean = {
    var possible = true
    if(getLigne(x_).contains(number_) || getColonne(y_).contains(number_) || getQuadran(x_,y_).contains(number_)) possible = false
    possible
  }

  def getColonne(y: Int) : Array[Int] = this.modele.map(_(y))
  def getLigne(x: Int) : Array[Int] = this.modele(x)
  def getQuadran(x : Int, y : Int):Array[Int] = {

      this.modele((x/3)*3  ).slice((y/3)*3, (y/3)*3+3) ++
      this.modele((x/3)*3+1).slice((y/3)*3, (y/3)*3+3) ++
      this.modele((x/3)*3+2).slice((y/3)*3, (y/3)*3+3)

  }
  override def toString: String = {
    var p =  "|-----|-----|-----|\n"
    for (i <- 0 until 9) {
      p += this.modele(i).mkString("|"," ","|")
      if(i%3 == 2){p+= "\n|-----|-----|-----|"}
      p += "\n"
    }
    p
  }
}
