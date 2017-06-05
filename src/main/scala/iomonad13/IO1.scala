package iomonad13

object IO1 {
  trait IO { self =>
    def run: Unit

    def ++(io: IO): IO = new IO {
      override def run: Unit = {
        self.run
        io.run
      }
    }
  }

  object IO {
    def empty: IO = new IO {
      override def run: Unit = ()
    }

    def PrintLine(msg: String): IO = new IO {
      override def run: Unit = println(msg)
    }
  }
}

case class Player(name: String, score: Int)

object Player {
  import IO1.IO
  import IO1.IO.PrintLine

  def contest(p1: Player, p2: Player): IO = PrintLine(winnerMsg(winner(p1, p2)))

  def winner(p1: Player, p2: Player): Option[Player] =
    if (p1.score > p2.score) Some(p1)
    else if (p1.score < p2.score) Some(p2)
    else None

  def winnerMsg(p: Option[Player]): String = p map {
    case Player(name, _) => s"$name is the winner!"
  } getOrElse "It's a draw."
}