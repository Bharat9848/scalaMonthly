package com.scalamonthly.may

object model {

  trait Encoder[A, B] {
    def encode(in: A): B
  }

  object Encoder {
    def apply[A, B](implicit encoder: Encoder[A, B]): Encoder[A, B] = encoder
    implicit val stringStringEncoder: Encoder[String, String] = new Encoder[String, String] {
      def encode(in: String): String = in
    }
  }

  trait Decoder[A, B] {
    def decode(in: B): Option[A]
  }

  object Decoder {
    def apply[A, B](implicit decoder: Decoder[A, B]): Decoder[A, B] = decoder
    implicit val stringStringDecoder: Decoder[String, String] = new Decoder[String, String] {
      def decode(in: String): Option[String] = Some(in)
    }
  }

  type StringEncoder[A] = Encoder[A, String]
  object StringEncoder {
    def apply[A](implicit encoder: StringEncoder[A]): StringEncoder[A] = encoder
  }
  type StringDecoder[A] = Decoder[A, String]
  object StringDecoder {
    def apply[A](implicit decoder: StringDecoder[A]): StringDecoder[A] = decoder
  }

  trait StringCodec[A] extends StringEncoder[A] with StringDecoder[A]

}
