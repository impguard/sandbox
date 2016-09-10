class TicTacToe {
  var board = Array.ofDim[Int](9)

  def this(b:Array[Int]) = {
    this()
    board = b
  }

  def play(player:Integer, i:Integer, j:Integer):Boolean = {
    val index = i + 3 * j
    if (board(index) != 0) {
      return false
    }

    board(index) = player
    return true
  }

  def winner():Int = {
    val positions = Array((0, 1, 2), (3, 4, 5), (6, 7, 8),
                          (0, 3, 6), (1, 4, 7), (2, 5, 8),
                          (0, 4, 8), (2, 4, 6))

    positions foreach { pos =>
      if (board(pos._1) != 0 &&
          board(pos._1) == board(pos._2) &&
          board(pos._2) == board(pos._3)) {
        return board(pos._1)
      }
    }

    return 0
  }
}

object Main {
  def main(args: Array[String]) = {
    var test = new TicTacToe(Array(1, 1, 2,
                                   0, 1, 2,
                                   2, 2, 1))
    println("Winner is: " + test.winner)
  }
}
