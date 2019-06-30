import scala.collection.immutable._
import scala.math.{abs, max, min}
import scala.concurrent.duration._

object MainClass {

  type Blocks = List[Int]
  def Blocks(xs: Int*) = List(xs: _*)

  type pieceType = Set[Blocks]
  def pieceType(xs: Blocks*) = Set(xs: _*)

  val gridWidth:Int = 10
  val gridHeight:Int = 24
  var grid : pieceType = pieceType()
  var score:Int = 0
  var touchBottom = false

  val pieceTypes : Array[pieceType] = Array(
    pieceType(Blocks(0,0), Blocks(0,1), Blocks(0,2), Blocks(0,3)),
    pieceType(Blocks(0,0), Blocks(1,0), Blocks(1,1), Blocks(2,1)),
    pieceType(Blocks(1,0), Blocks(0,1), Blocks(1,1), Blocks(2,1)),
    pieceType(Blocks(1,0), Blocks(2,0), Blocks(0,1), Blocks(1,1)),
    pieceType(Blocks(0,0), Blocks(1,0), Blocks(0,1), Blocks(1,1)),
    pieceType(Blocks(1,0), Blocks(1,1), Blocks(1,2), Blocks(0,2)),
    pieceType(Blocks(0,0), Blocks(0,1), Blocks(0,2), Blocks(1,2))
  )

  def newPiece(): pieceType = {
    val random = new scala.util.Random
    val randomIndex = random.nextInt(7)

    centerPiece(pieceTypes(randomIndex))
  }

  def centerPiece(piece:pieceType): pieceType = {
    val blockWithMaxX = piece.maxBy(point=>point(0))
    val middle = (gridWidth - blockWithMaxX(0)) / 2
    translate(piece,List(middle,0))
  }

  def translate(piece: pieceType, transform: List[Int]): pieceType = {
    val newPiece = piece.map(block=>List((block(0)+transform(0)), (block(1)+ transform(1))))
    newPiece
  }

  def fall(piece: pieceType): pieceType = {
    translate(piece, List(0,1))
  }

  def checkCollision(piece: pieceType): Boolean = {
    if(piece.exists(block=> block(0) < 0)) {
      true
    }
    else if(piece.exists(block=> block(1) < 0)) {
      true
    }
    else if(piece.exists(block=> block(0) > gridWidth-1))  {
      true
    }
    else if(piece.exists(block=> block(1) > gridHeight-1)) {
      //game over
      touchBottom = true
      true
    }
    else {
      //piece.
      val intersect = grid.intersect(piece)
      if (intersect.isEmpty) {
        false
      }
      else {
        touchBottom = true
        true
      }
    }
  }

  def setInGrid(piece:pieceType): Boolean = {
    if(checkCollision(piece)) {
      false
    }
    else {

      var newGrid = grid ++ piece
      grid = newGrid
      true
    }
  }

  def rotateAntiClock(piece: pieceType): pieceType = {
    var left = piece.minBy(point=>point(0))
    var down = piece.minBy(point=>point(1))
    var tempPiece = translate(piece, List(-left(0), -down(1)))

    val size = getSizeDimensionsOfPiece(tempPiece)
    var newPiece = tempPiece.map(block=>List(block(1), (size-1) - block(0)))
    var newPiece2 = translate(newPiece, List(left(0), down(1)))

    newPiece2
  }

  def rotateClock(piece: pieceType): pieceType = {
    var left = piece.minBy(point=>point(0))
    var down = piece.minBy(point=>point(1))
    var tempPiece = translate(piece, List(-left(0), -down(1)))

    val size = getSizeDimensionsOfPiece(tempPiece)
    var newPiece = tempPiece.map(block=>List((size-1) - block(1), block(0)))
    var newPiece2 = translate(newPiece, List(left(0),down(1)))

    newPiece2
  }

  def getSizeDimensionsOfPiece(piece: pieceType): Int = {
    val blockWithMaxX = piece.maxBy(point=>point(0))
    val blockWithMaxY = piece.maxBy(point=>point(1))
    max(blockWithMaxX(0), blockWithMaxY(1)) + 1
  }

  def printGrid() = {
    val toBePrinted = (0 until (gridHeight)).map(y=>(0  until (gridWidth)).map(x=>{
      if(grid.contains(Blocks(x,y))) {
        '⬛'
      }
      else {
        '▢'
      }
    }))

    val str = toBePrinted.map(block=>block.mkString(" ")).mkString("\n")
    println("Score: " + score)
    println(str)
    println("Move left: A     Move right: S \nRotate right: R     Rotate left: T")
  }

  def getPoints(): Int = {
    var numPointsGot:Int = 0;
    (0 until gridHeight).map(i => {
      if(rowFilled(i)) {
        numPointsGot += 1
        removeRow(i)
        dropAboveRemoved(i)
      }
    })

    numPointsGot
  }

  def removeRow(row:Int) = {
    (0 until gridWidth).map(i => {
     var newGrid = grid - Blocks(i, row)
      grid = newGrid
    })
  }

  def dropAboveRemoved(row:Int) = {
    ((row-1) to 0 by -1).map(y => (0 until gridWidth).map(x => {
      if(grid.contains(Blocks(x,y))) {
        var newGrid = grid - Blocks(x,y)
        var newGrid2 = newGrid - Blocks(x,y)
        grid = newGrid + Blocks(x,y+1)
      }
    }))
  }

  def rowFilled(row:Int): Boolean = {
    var filled:Boolean = true
    (0 until gridWidth).map(i => {
      if (!grid.contains(Blocks(i, row))) {
       filled = false
      }
    })
    filled
  }

  def printPiece(piece: pieceType) = {
    val size = getSizeDimensionsOfPiece(piece)
    val toBePrinted = (0 until (size)).map(y=>(0  until (size)).map(x=>{
      if(piece.contains(List(x,y))) {
        '⬛'
      }
      else {
        '▢'
      }
      //(piece.contains(List(x,y)))? '⬛' : '▢'
    }))

     val str = toBePrinted.map(block=>block.mkString(" ")).mkString("\n")
    println(str)
    //println('▢')
  }
// ⬛
  // ▢
  def main(args: Array[String]): Unit = {
    var gameOver:Boolean = false
    score = 0

    println("Welcome to Tetris!!")

    var input = scala.io.StdIn.readLine("Press any character to start: ")

    while(!gameOver) {
      var currentPiece = newPiece();
      gameOver = !setInGrid(currentPiece)

      printGrid()

      val deadline = 40.seconds.fromNow

      while(touchBottom == false) {
        input = scala.io.StdIn.readLine("Input: ")
        var newPiece = currentPiece

        if(input == "A" || input == "a") {
         newPiece = translate(currentPiece, List(-1,0))
        }
        else if(input == "S" || input == "s") {
         newPiece = translate(currentPiece, List(1, 0))
        }
        else if(input == "R" || input == "r") {
         newPiece = rotateAntiClock(currentPiece)
        }
        else if(input == "T" || input == "t") {
          newPiece = rotateClock(currentPiece)
        }
        else if(!deadline.hasTimeLeft()) {
          newPiece = fall(currentPiece)
        }

        setInGrid(newPiece)
        currentPiece = newPiece
      }

      score += getPoints()
      touchBottom = false
    }

    println("Game Over!\n Final Score " + score)
  }
}