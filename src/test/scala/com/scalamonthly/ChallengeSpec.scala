package com.scalamonthly

import cats.implicits._
import scala.util.Try

object ChallengeSpec extends weaver.SimpleIOSuite {

  import challenge._
  import model._

  pureTest("encrypt") {
    val shiftBy = 11
    val input = "This is a test. Testing, this is a test."
    val output = encrypt(input, shiftBy)
    expect.same("Estd td l epde. Epdetyr, estd td l epde.", output)
  }

  pureTest("encrypt and decrypt") {
    val shiftBy = 10
    val input = "This is a test. Testing, this is a test."
    val output = decrypt(encrypt(input, shiftBy), shiftBy)
    expect.same(Some(input), output)
  }

  pureTest("brute force decrypt - 1") {
    val shiftBy = 12
    val input = "Apple’s fiscal second quarter earnings come with a legal backdrop: the company faces an imminent courtroom battle with Fortnite developer Epic Games. Depositions from both companies have already been filed, and senior executives are expected to provide extensive testimony starting next week."
    val output = crackTheCode[Try, String](encrypt(input, shiftBy)).get
    expect.same(input, output)
  }

  pureTest("brute force decrypt - 2") {
    val shiftBy = 20
    val input = "It is not the critic who counts; not the man who points out how the strong man stumbles, or where the doer of deeds could have done them better. The credit belongs to the man who is actually in the arena, whose face is marred by dust and sweat and blood; who strives valiantly; who errs, who comes short again and again, because there is no effort without error and shortcoming; but who does actually strive to do the deeds; who knows great enthusiasms, the great devotions; who spends himself in a worthy cause; who at the best knows in the end the triumph of high achievement, and who at the worst, if he fails, at least fails while daring greatly, so that his place shall never be with those cold and timid souls who neither know victory nor defeat."
    val output = crackTheCode[Try, String](encrypt(input, shiftBy)).get
    expect.same(input, output)
  }

  pureTest("brute force decrypt - 3") {
    val shiftBy = 26
    val input = "The ideal set up by the Party was something huge, terrible, and glittering—a world of steel and concrete, of monstrous machines and terrifying weapons—a nation of warriors and fanatics, marching forward in perfect unity, all thinking the same thoughts and shouting the same slogans, perpetually working, fighting, triumphing, persecuting—three hundred million people all with the same face."
    val output = crackTheCode[Try, String](encrypt(input, shiftBy)).get
    expect.same(input, output)
  }

  pureTest("brute force decrypt - 4") {
    val shiftBy = 5
    val input = "That's the problem. We let people say stuff, and they say it so much that it becomes okay to them and normal for us. What's the point of having a voice if you're gonna be silent in those moments you shouldn't be?"
    val output = crackTheCode[Try, String](encrypt(input, shiftBy)).get
    expect.same(input, output)
  }

  private final case class Book(title: String, author: String, quote: String)

  pureTest("brute force decrypt - 5 - nonString") {
    val shiftBy = 17
    val quote = "Hyping your product to get funding while concealing your true progress and hoping that reality will eventually catch up to the hype continues to be tolerated in the tech industry."
    val input = Book("Bad Blood: Secrets and Lies in a Silicon Valley Startup", "John Carreyrou", quote)
    implicit val bookCodec: StringCodec[Book] = new StringCodec[Book] {
      def encode(book: Book): String = s"${book.title}|${book.author}|${book.quote}"
      def decode(in: String): Option[Book] = in.split('|').toList match {
        case title :: author :: quote :: Nil => Some(Book(title, author, quote))
        case _ => None
      }
    }
    val output = crackTheCode[Try, Book](encrypt(input, shiftBy)).get
    expect.same(input, output)
  }

  pureTest("brute force decrypt - 6 - unable to decode error") {
    val shiftBy = 17
    val quote = "Hyping your product to get funding while concealing your true progress and hoping that reality will eventually catch up to the hype continues to be tolerated in the tech industry."
    val input = Book("Bad Blood: Secrets and Lies in a Silicon Valley Startup", "John Carreyrou", quote)
    implicit val bookCodec: StringCodec[Book] = new StringCodec[Book] {
      def encode(book: Book): String = s"${book.title}|${book.author}|${book.quote}"
      def decode(in: String): Option[Book] = None
    }
    val output = crackTheCode[Try, Book](encrypt(input, shiftBy))
    expect(output.isFailure)
  }

}