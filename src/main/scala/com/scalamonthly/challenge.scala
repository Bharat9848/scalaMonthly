package com.scalamonthly

import cats.data.NonEmptyList
import cats.parse.{Parser, Parser1}
import cats.parse.Parser._
import cats.parse.Rfc5234.{char => _, _}
import cats.syntax.all._
import com.scalamonthly.model.Move.Castle

object challenge {

    import model._

    def parse(input: String): Either[Parser.Error, Game] = parser.parse(input).map(_._2)

  private val fileParser: Parser[File] = {
    charIn(List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')).map {
      case 'a' => File.A
      case 'b' => File.B
      case 'c' => File.C
      case 'd' => File.D
      case 'e' => File.E
      case 'f' => File.F
      case 'g' => File.G
      case 'h' => File.H
    }
  }

  private val rankParser: Parser[Rank] = {
    charIn(List('1', '2', '3', '4', '5', '6', '7', '8')).map {
      case '1' => Rank.One
      case '2' => Rank.Two
      case '3' => Rank.Three
      case '4' => Rank.Four
      case '5' => Rank.Five
      case '6' => Rank.Six
      case '7' => Rank.Seven
      case '8' => Rank.Eight
    }
  }

  val squareParser: Parser[Square] = (fileParser ~ rankParser).map(Square.tupled)

  val pieceParser: Parser[PieceType] = {
    charIn(List('K', 'Q', 'R', 'B', 'N')).map {
      case 'K' => PieceType.King
      case 'Q' => PieceType.Queen
      case 'R' => PieceType.Rook
      case 'B' => PieceType.Bishop
      case 'N' => PieceType.Knight
    }
    }

  val disambParser: Parser[Option[Disambiguator]] = squareParser.backtrack.orElse(fileParser.backtrack.orElse(rankParser.backtrack)).map {
    case square: Square => Disambiguator.FileAndRankSource(square)
    case file: File => Disambiguator.FileSource(file)
    case rank: Rank => Disambiguator.RankSource(rank)
  }.?


  val checkParser = char('+').as(CheckStatus.Check).orElse1(char('#').as(CheckStatus.Checkmate)).?

  val castleParser: Parser[Castle] = {
    val kingSide = (char('O') ~ char('-') ~ char('O')).string.as(Castle.KingSide)
    val queenSide = (char('O') ~ char('-') ~ char('O') ~ char('-') ~ char('O')).string.as(Castle.QueenSide)
    queenSide.backtrack.orElse(kingSide)
  }
  val captureParser = char('x').?


    def moveMapper(piece: Option[PieceType], disambiguator: Option[Disambiguator], capture: Boolean, square: Square, checkStatus: Option[CheckStatus], promotion:Option[PieceType]) = {
      (piece, disambiguator, capture, square, checkStatus, promotion) match {
        case (Some(piece), dis, capture, sq, checkStatusOpt, None) => Move.Standard(piece, sq, dis, capture, checkStatusOpt)
        case (None, dis, capture, sq, checkStatusOpt, None) => {
          val fileOpt = dis.map {
            case Disambiguator.FileSource(file) => file
          }
          Move.PawnMove(fileOpt, sq, capture, checkStatusOpt)
        }
        case (None, dis, capture, sq, checkStatusOpt, Some(promotedPiece)) => {
          val fileOpt = dis.map {
            case Disambiguator.FileSource(file) => file
          }
          Move.Promotion(Move.PawnMove(fileOpt, sq, capture, checkStatusOpt), promotedPiece)
        }
      }
    }
  val whiteSpaceParser = charIn(List(' ', '\t', '\n')).rep

  val moveParser: Parser[Move] = {
          val promotionParser = (char('=').soft *> pieceParser).?
          val woutDisambParser = ((((pieceParser.? ~ captureParser) ~ squareParser)~ promotionParser) ~ checkParser).map {
            case((((pieceOpt, capture), square), promotionOpt), check) => moveMapper(pieceOpt, None, capture.nonEmpty, square, check, promotionOpt)
            }
          val standerdMoveParser = (((((pieceParser.? ~ disambParser) ~ captureParser)~  squareParser) ~ promotionParser) ~ checkParser).map{
            case(((((pieceOpt, disOpt), capture), square), promotionOpt), check) => moveMapper(pieceOpt, disOpt, capture.nonEmpty, square, check, promotionOpt)
          }
    standerdMoveParser.backtrack.orElse(woutDisambParser.backtrack.orElse(castleParser))
  }

  val outcomeParser: Parser[Outcome] = {
    val win = char('1').string
    val loose = char('0').string
    val draw = (char('1') ~ char('/') ~ char('2')).string
    val result = draw.backtrack.orElse(win.orElse(loose))
    ((result.soft <*char('-')) ~ result).map {
      case("1", "0") => Outcome.WhiteWins
      case("0", "1") => Outcome.BlackWins
      case("1/2", "1/2") => Outcome.Draw
      case _ => Outcome.Unknown
    }
  }
  val turnParser: Parser[Turn] = ((moveParser<*whiteSpaceParser) ~ moveParser.?.orElse(char('*').as(Option.empty[Move]))).map {
    case (wMove, Some(bMove)) => Turn.FullTurn(wMove, bMove)
    case (wMove, None) => Turn.PartialTurn(wMove)
  }

  private val parser: Parser[Game] = {
    val dot = char('.')
    val outcome = outcomeParser.?

    ((((digit.rep1 ~ dot ~ whiteSpaceParser).string *> turnParser) <*whiteSpaceParser) ~ outcome).rep1
      .map( turnToOut => {
        val outcome = turnToOut.last._2.getOrElse(Outcome.Unknown)
        val turns = turnToOut.map(_._1)
        Game(turns, outcome)
      })
  }
}