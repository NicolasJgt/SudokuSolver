object resSudoku {
  def main(args: Array[String]) =
  {
    var grille = Array(
      Array(11,21,31,41,51,61,71,81,91),
      Array(12,22,32,42,52,62,72,82,92),
      Array(13,23,33,43,53,63,73,83,93),
      Array(14,24,34,44,54,64,74,84,94),
      Array(15,25,35,45,55,65,75,85,95),
      Array(16,26,36,46,56,66,76,86,96),
      Array(17,27,37,47,57,67,77,87,97),
      Array(18,28,38,48,58,68,78,88,98),
      Array(19,29,39,49,59,69,79,89,99))

    var testS = Array(
      Array(5,3,0,0,7,0,0,0,0),
      Array(6,0,0,1,9,5,0,0,0),
      Array(0,9,8,0,0,0,0,6,0),
      Array(8,0,0,0,6,0,0,0,3),
      Array(4,0,0,8,0,3,0,0,1),
      Array(7,0,0,0,2,0,0,0,6),
      Array(0,6,0,0,0,0,2,8,0),
      Array(0,0,0,4,1,9,0,0,5),
      Array(0,0,0,0,8,0,0,7,9))
    /*
    var test = new Grille(grille)
    var col = test.getColonne(1)
    var lig = test.getLigne(1)
    var quadran = test.getQuadran(4,8)

    print(col.mkString("\n"))
    print("\n")
    print(lig.mkString(" "))
    print("\n")
    print(quadran.mkString(" "))
    print("\n")

    */

    var sudo = new Sudoku(testS)
    sudo.solver()
    print("\n")
    print(sudo)




  }
  
}

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