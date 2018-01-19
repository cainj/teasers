package randoms

import scala.language.postfixOps
import scala.util.Try

object Poker extends App {

  type Cards = List[Card]

  val TypeOfHands = Map[String, Int](
    "straightFlush" -> 1,
    "fourOfKind" -> 2,
    "fullHouse" -> 3,
    "flush" -> 4,
    "straight" -> 5,
    "threeOfKind" -> 6,
    "twoPair" -> 7,
    "onePair" -> 8)

  /*
   * Given a set of 5 playing card identifiers such as 2H, 7C, QS, 10D, 2D;
   * determine if this hand is better than some other hand, according to the rules of poker.
   *
   * Hands will be a string with 5 cards comma separated,
   * each card will have 1-2 digits or JQKA and a suit indicator C,D,S,H (i.e. 10C, KH)
   *
   * Possible Hand Types Below:
   *   Straight flush
   *   Four of a kind
   *   Full house
   *   Flush
   *   Straight
   *   Three of a kind
   *   Two pair
   *   One pair
   *
   * The goal of this is to compare between the hand types.
   * Comparing 2 of the same type (i.e. 2 straights) to determine a winner is outside the scope
   * and will not be tested.
   *
   * Implement hand1WinsOverHand2 method and return whether or not the first hand wins over the second hand.
   */
  def hand1WinsOverHand2(hand1Str: String, hand2Str: String): Boolean =
    evaluate(convertToCards(hand1Str)) < evaluate(convertToCards(hand2Str))

  private[this] def convertToCards(cards: String): Cards = {
    val s = cards.split(',') map { _.trim }
    if (s.length != 5) throw new IllegalStateException("Illegal hand.")
    else
      s map {
        case x if x.length == 3 => Card(x.substring(0, 2).toShort, x(2))
        case x if x(0) == 'A' => Card(14, x(1))
        case x if x(0) == 'K' => Card(13, x(1))
        case x if x(0) == 'Q' => Card(12, x(1))
        case x if x(0) == 'J' => Card(11, x(1))
        case x if 0 < x(0).toString.toShort && x(0).toString.toShort < 11 => Card(x(0).toString.toShort, x(1))
        case _ => throw new IllegalStateException("No Wild Cards!")
      } toList
  }

  private[this] def evaluate(cards: Cards): Int = {
    val sorted = cards.sortWith(_.value < _.value)

    val straight = (1 to 4).forall{ next => sorted(next).value - sorted(next - 1).value == 1 }
    val flush = sorted.forall(_.suite == sorted.head.suite)

    if (straight && flush)
      TypeOfHands("straightFlush")
    else {
      val grouped = sorted.groupBy(_.value)
      if (grouped.size == 2 && grouped.exists(_._2.lengthCompare(4) == 0))
        TypeOfHands("fourOfKind")
      else if (flush)
        TypeOfHands("flush")
      else if (grouped.size == 2 && grouped.exists(_._2.lengthCompare(3) == 0) && grouped.exists(_._2.lengthCompare(2) == 0))
        TypeOfHands("fullHouse")
      else if (straight)
        TypeOfHands("straight")
      else if (grouped.size == 3 && grouped.exists(_._2.lengthCompare(3) == 0))
        TypeOfHands("threeOfKind")
      else if (grouped.size == 3 && grouped.exists(_._2.lengthCompare(2) == 0))
        TypeOfHands("twoPair")
      else if (grouped.size == 4)
        TypeOfHands("onePair")
      else
        Short.MaxValue
    }
  }

  implicit class CompareTwoPokerHands(hand1: String) {
    def winsOver(hand2: String): Unit = {
      val result = if (hand1WinsOverHand2(hand1, hand2)) "Correct" else "Incorrect"
      println(s"$result -> hand [$hand1] wins over [$hand2]")
    }
  }

  println("Poker Hand comparison")
  "8C,9C,10C,JC,QC" winsOver "6S,7H,8D,9H,10D" // straight flush
  "4H,4D,4C,4S,JS" winsOver "6C,6S,KH,AS,AD" // four of a kind
  "5C,3C,10C,KC,7C" winsOver "6C,6D,6H,9C,KD" // flush
  "4H,4D,4C,KC,KD" winsOver "9D,6S,KH,AS,AD" // full house
  "2C,3C,4S,5S,6S" winsOver "6C,6D,6H,9C,KD" // straight
  "7C,7D,7S,3H,4D" winsOver "9S,6S,10D,AS,AD" // three of a kind
  "8C,8H,10S,KH,KS" winsOver "2S,2D,JH,7S,AC" // two pair
  "AC,AH,3C,QH,10C" winsOver "3S,2D,KH,JS,AD" // one pair
  "8C,9C,10C,JC,QC" winsOver "8C,9C,10C,JC,QC" //test 1 Incorrect
  "2C, 2S, 2H, 2D, KH" winsOver "6H, 7H, 8H, 9H, 10H" //test 2 Incorrect
  Try { "2C, 4H, 3D, 5C" winsOver "6H, 7H, 8H, 9H, 10H" } getOrElse println("Deal Again")
  Try { "2C, 4H, 3D, 5C, ZJ" winsOver "6H, 7H, 8H, 9H, 10H" } getOrElse println("Deal Again")
}

case class Card(value: Short, suite: Char)
