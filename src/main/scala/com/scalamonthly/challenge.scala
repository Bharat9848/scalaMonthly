package com.scalamonthly

import cats.ApplicativeThrow
import cats.syntax.all._

object challenge {

  import model._

  /**
    * Implement encryption using a caesar cipher that shifts the input (once stringified)
    * by `shiftBy`. Leave all non-alpha characters in place. Preserve the case of the letters
    * as you encrypt them.
    *
    * @param input The thing to encrypt
    * @param shiftBy The amount to shift it by
    * @return The encrypted string
    */
  def encrypt[A: StringEncoder](input: A, shiftBy: Int): String = ???

  /**
    * The inverse of `encrypt`.
    * 
    * val input = "test"
    * decrypt(encrypt(input, 10), 10) == input // true
    *
    * @param input
    * @param shiftBy
    * @return An option of the decrypted value. Is optional because the decoder can fail.
    */
  def decrypt[A: StringDecoder](input: String, shiftBy: Int): Option[A] = ???

  /**
    * Crack the caesar cipher of the provided input. You are expected to
    * determine the encryption key (the `shiftBy`) and use that value to return
    * the original message.
    * 
    * IF the StringDecoder returns a `None`, raise an error inside the error
    * channel of `F` (using `ApplicativeThrow`).
    *
    * @param input An encrypted string
    * @return The decrypted message
    */
  def crackTheCode[F[_]: ApplicativeThrow, A: StringDecoder](input: String): F[A] = ???

}