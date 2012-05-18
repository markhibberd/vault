## Vault

#### What is Vault?

**Vault** is the name for a software library written using the Scala programming language. Vault is compiled against version 2.9.1 of the Scala programming language. Vault is licenced under BSD3 (see `etc/LICENCE`).

The Vault library is used for interacting with persistence storage through the JDBC database API. Vault implements and exploits several computational concepts:

* Iteratee-based I/O.

* Monad values and monad transformer stacks.

* Effect-tracking in the type system (to a small, possibly inadmissible, extent).

* Abstract Algebraic Data Types (ADTs) by implementing *Smart Constructors*.

#### Introduction

Vault has been written to address the absence of a persistence API for the Java 2 Platform that is suitable for use in application development. It was inspired, in part, by this absence and encouraged further by the advances that have been made in the [Takusen](http://hackage.haskell.org/package/Takusen) software package.

The Scala programming language was chosen because it has certain desirable features:

* Transparent integration with existing Java libraries, permitting the use of the JDBC API and migration for existing Java applications.

* *Higher-order polymorphism*, permitting the representation of high-level concepts that are fundamental to general computing.

* Implicit dictionary passing, to avoid the many undesirable consequences of nominal subtyping, while retaining the advantages.

* First-class functions, permitting the expression of fundamental concepts without ceremony.

Vault has a dependency on the Scalaz library version 6, which is compiled against Scala version 2.9.1. Scalaz is a complementary library for general application development with the Scala programming language.

#### The Essence of Vault

At the centre of Vault are two data types:

1. `SqlConnect`

2. `RowConnect`

The unfortunate dichotomy of these two data types is a consequence of a known limitation in Scala. In particular, that they both share a common abstraction -- for a varying value `M`, they both share `ReaderT[java.sql.Connection, M, SqlValue[A]]`.

* For `SqlConnect`, the value for `M` is `Identity`.

* For `RowConnect`, the value for `M` is `Option`.

Worse, this dichotomy manifests itself throughout the Vault API, not just in these two data types.

These data types take a value representing a database connection and produce either a (polymorphic) result, or a `java.sql.SQLException`. These data types also form a monad by which they can be composed in many ways. To provide such an example, consider a common function which takes the form of lifting a binary function into a monad:

    (A => B => C) => M[A] => M[B] => M[C]

If we take the example of the SqlConnect monad, we get:

    (A => B => C) => SqlConnect[A] => SqlConnect[B] => SqlConnect[C]

This means that if we have a function takes takes an Apple, a Banana and produces a Cherry, then we can get a function that takes a `SqlConnect` producing Apples, a `SqlConnect` producing Bananas and get a `SqlConnect` producing Cherries.

This model of computation is expected to be very common in using the Vault API. The function described above is so common that it has a general description in computability theory, *lifting into a monad with arity 2*. It can be written using Scala as follows:

    def liftM2[A, B, C] =
      (f: A => B => C) =>
      (sa: SqlConnect[A]) =>
      (sb: SqlConnect[B]) =>
        for {
          a <- sa
          b <- sb
        } yield f(a)(b)

Monadic computations are not restricted to `SqlConnect`. In fact, there are several other monads in the Vault API that can be used to replace `SqlConnect` in the above expression:

* `RowConnect`

* `SqlValue`

* `RowValue`

* `SqlAccess`

* `RowAccess`

* `RowQueryConnect`

* `PossiblyNull`

There are other useful library functions that can be used on the above values as a consequence of being a monad. Discussing these further is beyond this document's scope, however, many of them can be found in the Scalaz library.

#### Example Usage of Vault

##### Constructing an Object from a Database Row

The `SqlAccess` and `RowAccess` data types are used to construct an object from a `Row`. The `RowAccess` data type has the potential to produce a `null` database value.

In order to produce an object of type `T`, we need to supply a value of type `SqlAccess[T]`. We do this by taking values of type `RowValue[T]` then indicating how we want `null` values to be handled. We do this by calling one of two methods:

1. `RowAccess.unifyNull` to indicate that we do not expect to encounter a `null` value for that database column. If `null` is encountered, the computation will discontinue with a `java.sql.SQLException` and a message. A custom exception message may be supplied with the `RowAccess.unifyNullWithMessage` method.

2. `RowAccess.possiblyNull` to indicate that we expect to encounter a `null` value for that database column. If `null` is encountered, the computation will produce a `PossiblyNull[T]` for the given `RowAccess[T]`. This allows you to deal with the `null` value appropriately for your custom data type.

Since `SqlAccess` (and `RowAccess`) forms a monad, we can combine the values in the Vault API to produce values for our custom data types. Supposing our own custom data type:

    case class Person(name: String, age: Int, address: PossiblyNull[String])

Note that while a `name` and an `age` are required attributes, the `address` is not.

We may use the `RowAccess.stringLabel` method to produce a value of the type `RowAccess[String]`. Since we do not expect `null` values in this column, we unify them with an exception should they be encountered:

    val nameAccess: SqlAccess[String] =
      stringLabel("name").unifyNull

We do similar for the `age` attribute:

    val ageAccess: SqlAccess[Int] =
      intLabel("age").unifyNull

However, for the `address` attribute, we specify the possibility for a `null` value:

    val addressAccess: SqlAccess[PossiblyNull[String]] =
      stringLabel("address").possiblyNull

This much is easy enough, however, we must also compose them to produce our desired result, which is a value of the type `SqlAccess[Person]`. Since `SqlAccess` forms a monad, we may use values of this type in a for-comprehension where producing our result becomes trivial:

    val personAccess: SqlAccess[Person] =
      for {
        name    <- nameAccess
        age     <- ageAccess
        address <- addressAccess
      } yield Person(name, age, address)

##### Combining with an Iteratee

Once we have the ability to produce our desired objects from a database row, we need to specify which rows we are interested in. We do this by using the iteratee interface provided by Scalaz. Iteratees exhibit certain desirable properties that we are interested in:

* They are composable. In particular, they form a monad.

* They do not necessarily require that we have traversed the data that we are interested in.

For example, if we suppose that we want to take the pair of:

* The first `Person` encountered (if there is one) in the results.

* A `List` of `List` of `Person` where each list contains adjacent Person values that have the same name attribute.

We would do this by combining two iteratee values (from the Scalaz iteratee library):

    val combined =
      for {
        fst <- IterV.peek[Person]
        grp <- IterV.groupBy(((_:Person).name).equaling)
      } yield (fst, grp)

Now that we have methods by which to construct a `Person` from a database row and now we have combined iteratees to indicate what we are interested in `(Option[Person], List[List[Person]])`, we need to get a database connection, specify the query, then produce the results.

    val connection: java.sql.Connection =
      ...
    val result =
      personAccess -||> combined <|-
        "SELECT * FROM Person".toSql finalyClose connection

Our final result, of the type `RowValue[(Option[Person], List[List[Person]])]`, contains one of:

* No value, with a potential message, since an unexpected `null` value was encountered.

* A value of the type `java.sql.SQLException` because there was a database error in executing the query and accessing the results.

* A value of the type `(Option[Person], List[List[Person]])` containing our desired results.

There are various methods on the `RowValue` data type that allow you to access these possibilities.

#### Limitations

##### Absence of Tracking of Effects

The Scala programming language does not track side-effects in its type system. This means that the following two program segments have the same type:

    val x: Int = { 7 }

    val y: Int = { println("hello"); 7 }

This lack of delineation in the types has many practical implications and this property is pervasive throughout the Vault API. The extent of these implications is beyond the scope of this document, but it is sufficient to caution the potential for accidental equivocation when using the Vault API.

Scalaz version 6 includes support for using types for effect-tracking, however, this support was added after the commencement of the development of Vault and so was not utilised in the Vault design. It is expected that future versions of Vault will incorporate this type-system feature.

##### Scalaz Iteratee Model

Scalaz implements the concept of iteratees, which were [recently introduced as a useful abstraction](http://okmij.org/ftp/papers/LL3-collections-enumerators.txt). At this time, it is generally accepted that this iteratee interface is useful, however, the details of the interface have not been generally agreed upon by the computer science community. Scalaz implements one such representation of the iteratee concept, which is known to have limitations with respect to the general case. Therefore, there are presumably use-cases for which the Vault API may become untenable.

***

**Copyright 2012 Ephox Pty Ltd**
