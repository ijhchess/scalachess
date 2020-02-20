package chess
package variant

case object Losers
    extends Variant(
      id = 11,
      key = "losers",
      name = "Losers",
      shortName = "Lose",
      title = "Lose all your pieces (or get stalemated/checkmated) to win the game.",
      standardInitialPosition = true
    ) {

  def pieces = Standard.pieces
  

  // In this variant, a player must capture if a capturing move is available
  override def validMoves(situation: Situation) = {
    val allMoves       = super.validMoves(situation)
    val capturingMoves = allMoves.view mapValues (_.filter(_.captures)) filterNot (_._2.isEmpty)

    (if (!capturingMoves.isEmpty) capturingMoves else allMoves).to(Map)
  }

  override def valid(board: Board, strict: Boolean) =
    board.pieces.size >= 2 && board.pieces.size <= 32

  // In losers, the goal is to lose your pieces or get stalemated/checkmated, so the winner is the current player if they have no legal moves
  override def winner(situation: Situation): Option[Color] =
    if (specialEnd(situation)) Some(situation.color) else None

  override def specialEnd(situation: Situation) = {
    // The game ends with a win when one player manages to lose all their pieces or is in stalemate
    situation.board.piecesOf(situation.color).isEmpty || situation.moves.isEmpty
  }

  // In losers, it is valuable for your opponent to have pieces.
  override def materialImbalance(board: Board): Int = board.pieces.values.foldLeft(0) {
    case (acc, Piece(color, _)) => acc + color.fold(-2, 2)
  }
}