package com.scalamonthly.may

import cats.ApplicativeThrow

import scala.collection.mutable

object challenge {

  import model._

  val dictionary = mutable.Set[String]();

  def stringRightShift(text: String, shiftBy: Int):String = {
    val shiftArr = text.toCharArray.map(ch => {
      if(ch.isLower){
        ('a' + (((ch - 'a') + shiftBy)%26)).toChar
      }else if(ch.isUpper){
        ('A' + (((ch - 'A') + shiftBy)%26)).toChar
      }else {
        ch
      }
    })
    new String(shiftArr)
  }
  /**
    * Implement encryption using a caesar cipher that shifts the input (once stringified)
    * by `shiftBy`. Leave all non-alpha characters in place. Preserve the case of the letters
    * as you encrypt them.
    *
    * @param input The thing to encrypt
    * @param shiftBy The amount to shift it by
    * @return The encrypted string
    */
  def encrypt[A: StringEncoder](input: A, shiftBy: Int): String = {
    val inString = StringEncoder[A].encode(input)
    inString.split(" ").map(s=> s.trim().replace(":","").replace(".","").replace(",","")).filter(str => str.length > 2).foreach(str =>{
    dictionary.add(str)
    })
    stringRightShift(inString, shiftBy)
  }


  def stringLeftShift(input: String, shiftBy: Int): String = {
    val shiftArr = input.toCharArray.map(ch => {
      if(ch.isLower){
        val offset = ((ch - 'a') - shiftBy);
        val offsetAdjusted = if(offset < 0)  (26+offset) else offset;
        ('a' + offsetAdjusted).toChar
      }else if(ch.isUpper){
        val offset = ((ch - 'A') - shiftBy);
        val offsetAdjusted = if(offset < 0)  (26 + offset) else offset;
        ('A' + offsetAdjusted).toChar
      }else {
        ch
      }
    })
    new String(shiftArr)
  }


  def checkDictionary(str: String): Boolean = {
//    println(s"dictionary = $dictionary")
    str.split(" ").map(s=>s.trim)
      .filter(word => word.length>0 && word.forall(c => c.isLetter))
      .filter(str => str.length > 2)
      .forall(dictionary.contains)
  }


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
  def decrypt[A: StringDecoder](input: String, shiftBy: Int): Option[A] = {
    val shiftedString = stringLeftShift(input, shiftBy)
    println(s"decrypting $shiftedString")
    if (checkDictionary(shiftedString)) {
      StringDecoder[A].decode(stringLeftShift(input, shiftBy))
    } else {
      None
    }

  }

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
  def crackTheCode[F[_]: ApplicativeThrow, A: StringDecoder](input: String): F[A] = {

    val result = (0 to 26).map(i => decrypt(input, i))
      .fold(Option.empty[A]) { case (None, Some(a)) => Some(a)
      case (Some(a), _) => Some(a)
      case _ => Option.empty[A]
      }
    result match {
      case Some(value) => ApplicativeThrow[F].pure(value)
      case None => ApplicativeThrow[F].raiseError(new Throwable("Not able to decrypt"))
    }
  }
}
