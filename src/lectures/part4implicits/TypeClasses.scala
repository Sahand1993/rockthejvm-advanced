package lectures.part4implicits

object TypeClasses extends App {
  trait HTMLWritable {
    def toHTML: String
  }

  case class User(name: String, age: Int, email: String) extends HTMLWritable {
    override def toHTML: String = s"<div>$name ($age yo) <a href=$email/> </div>"
  }

  val john = User("John", 32, "john@rockthejvm.com")
  println(john.toHTML)
  /*
  Disadvantages:
    1 - this only works for the types WE write.
    2 - this is only ONE implementation out of many possible ones.
   */

  // option 2 - pattern matching
  object HTMLSerializerPM {
    def serializeToHtml(value: Any) = value match {
      case User(n, a, e) =>
      case _ =>
    }
  }

  /*
  Disadvantages:
    1 - we lost the type safety
    2 - need to modify the code every time we add a new data structure
    3 - This is still one implementation for each given type
   */

  // option 3:
  trait HTMLSerializer[T] {
    def serialize(value: T): String
  }

  object UserSerializer extends HTMLSerializer[User] {
    def serialize(user: User): String = s"<div>${user.name} (${user.age} yo) <a href=${user.email}/> </div>"
  }
  println(UserSerializer.serialize(john))

  // 1 - we can define for other types
  import java.util.Date
  object DateSerializer extends HTMLSerializer[Date] {
    override def serialize(date: Date): String = s"<div>${date.toString}</div>"
  }

  // 2 - we can define MULTIPLE serializers
  object PartialUserSerializer extends HTMLSerializer[User] {
    override def serialize(user: User): String = s"<div>${user.name}</div>"
  }

  // HTMLSerializer is a TYPE CLASS
  // All the implementors of a type class are called type class instances.
  trait MyTypeClassTemplate[T] {
    def action(value: T): String
  }

  object MyTypeClassTemplate {
    def apply[T](implicit instance: MyTypeClassTemplate[T]) = instance
  }

  /**
    * Equality
    */
  trait Equal[T] {
    def apply(a: T, b: T): Boolean
  }

  object UserAgeEquality extends Equal[User] {
    override def apply(a: User, b: User): Boolean = a.age == b.age
  }

  object UserNameEquality extends Equal[User] {
    override def apply(a: User, b: User): Boolean = a.name == b.name
  }

  implicit object UserFullEquality extends Equal[User] {
    override def apply(a: User, b: User): Boolean = a.name == b.name && a.age == b.age && a.email == b.email
  }

  println(UserAgeEquality(User("Tom", 3, "tommy@hotmail.com"), User("Adam", 2, "adamsandler@sandler.com")))
  println(UserNameEquality(User("Tom", 2, "asdf@asdf.com"), User("Tom", 3, "234sd@hotmail.com")))

  // part 2 - implicits
  object HTMLSerializer {
    def serialize[T](value: T)(implicit serializer: HTMLSerializer[T]): String =
      serializer.serialize(value)

    def apply[T](implicit serializer: HTMLSerializer[T]) = serializer
  }

  implicit object htmlSerializer extends HTMLSerializer[User] {
    override def serialize(user: User): String = s"<div>${user.name}</div>"
  }
  println(HTMLSerializer.serialize(john))

  implicit object IntSerializer extends HTMLSerializer[Int] {
    override def serialize(value: Int): String = s"<div style=color:blue>$value</div>"
  }
  println(HTMLSerializer.serialize(1))

  // access to the entire type class interface
  println(HTMLSerializer[User].serialize(john))

  /**
    * Excercise: Implement the type class pattern for the Equality type class.
    */
  object Equal {
    def apply[T](a: T, b: T)(implicit equal: Equal[T]): Boolean = equal(a, b)
  }

  implicit object IntEqual extends Equal[Int] {
    override def apply(a: Int, b: Int): Boolean = a == b
  }
  println(Equal(1,1))
  println(Equal(User("Tom", 20, "tom@tom.se"), User("Tom", 20, "tom@tom.se"))) // uses the implicit UserFullEquality above
  // AD-HOC polymorphism


}
