package com.scalamonthly

import java.util.UUID

import cats.Applicative
import monocle.macros.{GenLens, GenPrism}
import monocle.std.option.some
import com.scalamonthly.fundamentals.Animal.DogName
import monocle.{Optional, Prism, Traversal}
import com.scalamonthly.fundamentals.Animal.Dog
import monocle.function.Plated
import cats.implicits._
import com.scalamonthly.fundamentals.Tree.{Branch, Leaf}

object fundamentals {

  final case class FirstName(value: String) extends AnyVal
  final case class LastName(value: String) extends AnyVal
  final case class Name(firstName: FirstName, lastName: LastName)
  final case class Person(id: UUID, name: Name)
  final case class AccountHolder(primary: Person, secondary: Option[Person])
  final case class BankAccount(id: UUID, holder: AccountHolder, balance: Long)

  /**
    * Return the firstName of the primary account holder.
    */
  def one(ba: BankAccount): FirstName = GenLens[BankAccount](_.holder.primary.name.firstName).get(ba)

  /**
    * Update the firstName of the primary account holder to be "Mal"
    */
  def two(ba: BankAccount): BankAccount = {
    val lens = GenLens[BankAccount](_.holder.primary.name.firstName)
    lens.set(FirstName("Mal"))(ba)
  }

  /**
    * Update the lastName of the primary and secondary (if exists) account holders to be "Fischer"
    */
  def three(ba: BankAccount): BankAccount = {
    val fromAccountToHolder = GenLens[BankAccount](_.holder)
    val fromPersonToLastName = GenLens[Person](_.name.lastName)
    val fromHolderToPrimaryPerson = GenLens[AccountHolder](_.primary)
    val fromHolderToSecondry = GenLens[AccountHolder](_.secondary)
    val optionPrism = Prism.partial[Option[Person], Person]{case Some(x) => x}(p => Some(p))
    val bankAccPrimaryUpdate  = fromAccountToHolder.composeLens(fromHolderToPrimaryPerson).composeLens(fromPersonToLastName).set(LastName("Fischer"))
    val bankAccSecUpdate = fromAccountToHolder.composeLens(fromHolderToSecondry).composePrism(optionPrism).composeLens(fromPersonToLastName).set(LastName("Fischer"))
    bankAccPrimaryUpdate.compose(bankAccSecUpdate)(ba)
  }

  sealed abstract class Animal extends Product with Serializable
  object Animal {
    final case class DogName(value: String) extends AnyVal
    final case class Dog(name: DogName) extends Animal
    final case class Fish(length: Int) extends Animal
    final case class Turtle(age: Int) extends Animal
  }

  /**
    * Use a Prism to return the name of the animal if it is a dog, else return None.
    */
  def four(a: Animal): Option[DogName] = {
    Prism.partial[Animal, DogName]{case x:Dog => x.name}(Dog).getOption(a)
  }

  /**
    * Use a Prism to construct an Animal given a DogName.
    */
  def five(d: Animal.DogName): Animal = {
    Prism.partial[Animal, DogName]{case x:Dog => x.name}(Dog).apply(d)
  }

  final case class HouseholdOccupants(owner: Person, pet: Animal)
  final case class Household(id: UUID, occupants: HouseholdOccupants)
  
  /**
    * If the household pet is a dog, append " II" to its name and return the entire household.
    */
  def six(h: Household): Household = {
    val prismDogName = Prism.partial[Animal, DogName]{case x:Dog => x.name}(Dog)
    val lensPet = GenLens[Household](_.occupants.pet)
    lensPet.composePrism(prismDogName).modify(dName => DogName(dName.value + " II"))(h)
  }

  sealed abstract class Tree[+A] extends Product with Serializable
  object Tree {
    final case class Branch[A](data: A, left: Tree[A], right: Tree[A]) extends Tree[A]
    final case class Leaf[A](data: A) extends Tree[A]
  }

  /**
    * Add 1 to every leaf node inside of the tree.
    * 
    * Hint: Look at the Plated type-class from Monocle.
    */
 /* def seven(tree: Tree[Int]): Tree[Int] = {
    val t = new Traversal[Tree[Int] , Int]{
      override def modifyF[F[_]](f: Int => F[Int])(s: Tree[Int])(implicit evidence$1: Applicative[F]): F[Tree[Int]] =
        Applicative[F].map(s.pure){ f match {
          case Leaf(data) => f(data)
          case Tree.Branch(data, left, right) => Tree.Branch(data, left, right)
        }
        }
    }
    t.modify(i => i +1)(tree)
  }*/

 private implicit def treePlated[A]: Plated[Tree[A]] = Plated(
   new Traversal[Tree[A],Tree[A]] {
     def modifyF[F[_]: cats.Applicative](f: Tree[A] => F[Tree[A]])(s: Tree[A]): F[Tree[A]] = s match {
       case Tree.Leaf(d) => Tree.Leaf(d).pure[F].widen
       case Tree.Branch(d, l, r) => cats.Applicative[F].product(f(l), f(r)).map(res => Tree.Branch(d, res._1, res._2))
     }
   }
 )

  def seven(tree: Tree[Int]): Tree[Int] = {
    Plated.transform[Tree[Int]] {
      case b: Branch[Int] => b
      case Leaf(data) => Leaf(data + 1)
    }(tree)
  }

}