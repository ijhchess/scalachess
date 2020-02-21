package chess

import scalaz.Validation.FlatMap._
import variant.Losers
import format.Forsyth
import format.pgn.Reader

class LosersVariantTest extends ChessTest {

  // Random PGN taken from FICS
  val fullGame =
    """[Event "FICS rated losers game"]
[Site "FICS freechess.org"]
[FICSGamesDBGameNo "470697506"]
[White "ducetray"]
[Black "Nukky"]
[WhiteElo "1724"]
[BlackElo "2069"]
[WhiteRD "69.9"]
[BlackRD "60.2"]
[TimeControl "180+0"]
[Date "2020-02-18"]
[Time "06:29:00"]
[Duration "0:05:43"]
[WhiteClock "0:03:00.000"]
[BlackClock "0:03:00.000"]
[Result "0-1"]
[LongResult "Black wins by losing all material"]

1. c4 f5 2. c5 f4 3. e3 fxe3 4. dxe3 h6 5. Qxd7+ Kxd7 6. c6+ bxc6 7. Bb5 cxb5 8. Nf3 h5 9. Ne5+ Ke8 10. Nc6 Nxc6 11. O-O Qd1 12. Rxd1 Bd7 13. Rxd7 Kxd7 14. a4 bxa4 15. Rxa4 g6 16. Rxa7 Rxa7 17. Na3 Rxa3 18. bxa3 Kd6 19. g4 hxg4 20. Kf1 Rxh2 21. f3 gxf3 22. Bb2 Rxb2 23. e4 e5 24. a4 Na5 25. Ke1 Kc5 26. Kd1 Rh2 27. Kc1 Kd4 28. Kb1 Kxe4 29. Ka1 g5 30. Kb1 Ba3 31. Ka1 g4 32. Kb1 Bc1 33. Kxc1 c5 34. Kb1 c4 35. Ka1 Kf5 36. Kb1 e4 37. Ka1 Ke5 38. Kb1 e3 39. Ka1 g3 40. Kb1 Rd2 41. Ka1 Nf6 42. Kb1 Ne4 43. Ka1 Nf2 44. Kb1 Nd3 45. Ka1 e2 46. Kb1 Nc1 47. Kxc1 Rd3 48. Kb1 Ke4 49. Ka1 Ke3 50. Kb2 f2 51. Ka1 g2 52. Ka2 e1=R 53. Kb2 Ra1 54. Kxa1 f1=R+ 55. Ka2 Ra1+ 56. Kxa1 c3 57. Ka2 c2 58. Ka1 c1=R+ 59. Ka2 Ra1+ 60. Kxa1 g1=R+ 61. Ka2 Ra1+ 62. Kxa1 Rd1+ 63. Ka2 Ra1+ 64. Kxa1 Nb3+ 65. Kb1 Na1 66. Kxa1  {Black wins by losing all material} 0-1
"""

  "Losers " should {

    "Allow an opening move for white taking into account a player may move without taking if possible" in {
      val startingPosition = Game(Losers)
      val afterFirstMove   = startingPosition.playMove(Pos.E2, Pos.E4, None)

      afterFirstMove must beSuccess.like {
        case newGame =>
          val fen = Forsyth >> newGame
          fen mustEqual "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b - - 0 1"
      }
    }

    "Not allow a player to make a non capturing move if a capturing move is available" in {
      val game             = Game(Losers)
      val gameAfterOpening = game.playMoves((Pos.E2, Pos.E4), (Pos.F7, Pos.F5))

      val invalidGame = gameAfterOpening flatMap (_.playMove(Pos.H2, Pos.H4))

      invalidGame must beFailure.like {
        case failMsg => failMsg mustEqual scalaz.NonEmptyList("Piece on h2 cannot move to h4")
      }
    }

    "A situation in losers should only present the capturing moves if the player can capture" in {
      val game             = Game(Losers)
      val gameAfterOpening = game.playMoves((Pos.E2, Pos.E4), (Pos.F7, Pos.F5))

      gameAfterOpening must beSuccess.like {
        case newGame =>
          newGame.situation.moves.size must beEqualTo(1)
          newGame.situation.moves.values.find(_.find(_.captures == false).nonEmpty) must beNone
      }

    }

    "Allow a capturing move to be made" in {
      val game = Game(Losers).playMoves((Pos.E2, Pos.E4), (Pos.F7, Pos.F5), (Pos.E4, Pos.F5))
      game must beSuccess
    }


    "Be drawn on a three move repetition" in {
      val game = Game(Losers)

      val moves         = List((Pos.G1, Pos.F3), (Pos.G8, Pos.F6), (Pos.F3, Pos.G1), (Pos.F6, Pos.G8))
      val repeatedMoves = List.fill(3)(moves).flatten

      val drawnGame = game.playMoveList(repeatedMoves)

      drawnGame must beSuccess.like {
        case g => g.situation.threefoldRepetition must beTrue
      }

    }


    "Win on a traditional stalemate where the player has no valid moves" in {
      val positionString = "1k6/p6R/7Q/8/8/8/8/4K2B w - - 0 1"
      val maybeGame      = fenToGame(positionString, Losers)

      val drawnGame = maybeGame flatMap (_.playMoves((Pos.H6, Pos.A6)))

      drawnGame must beSuccess.like {
        case game =>
          game.situation.end must beTrue
          game.situation.status must beSome.like {
            case state => state == Status.VariantEnd

          }
          game.situation.winner must beSome.like {
            case color =>
              color == Black;
          }
      }
    }

    "Stalemate is a win - second test" in {
      val fen       = "1k6/7R/4p3/p3P3/P7/8/4Q3/4K3 w - - 0 1"
      val maybeGame = fenToGame(fen, Losers)

      val drawnGame = maybeGame flatMap (_.playMoves((Pos.E2, Pos.A6)))

      drawnGame must beSuccess.like {
        case game =>
          game.situation.end must beTrue
          game.situation.status must beSome.like {
            case state => state == Status.VariantEnd

          }
          game.situation.winner must beSome.like {
            case color =>
              color == Black;
          }
      }
    }

    "Win on a traditional checkmate where the player has no valid moves" in {
      val positionString = "1k6/7R/4p3/p3P3/P7/8/8/4K1Q1 w - - 0 1"
      val maybeGame      = fenToGame(positionString, Losers)

      val drawnGame = maybeGame flatMap (_.playMoves((Pos.G1, Pos.G8)))

      drawnGame must beSuccess.like {
        case game =>
          game.situation.end must beTrue
          game.situation.winner must beSome.like {
            case color =>
              color == Black;
          }
      }
    }

    "Checkmate is a win - second test" in {
      val positionString = "2k5/7Q/8/8/8/3K2B1/8/8 w - - 0 1"
      val maybeGame      = fenToGame(positionString, Losers)

      val drawnGame = maybeGame flatMap (_.playMoves((Pos.H7, Pos.C7)))

      drawnGame must beSuccess.like {
        case game =>
          game.situation.end must beTrue
          game.situation.winner must beSome.like {
            case color =>
              color == Black;
          }
      }
    }
  }

}
