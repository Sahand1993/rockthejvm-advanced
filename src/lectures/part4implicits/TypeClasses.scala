package lectures.part4implicits

object TypeClasses extends App {
  trait HTMLWritable {
    def toHtml: String
  }

  case class User(name: String, age: Int, email: String) extends HTMLWritable {
    override def toHtml: String = s"<div>$name ($age yo) <a href=$email/> </div>"
  }

  val john = User("John", 32, "john@rockthejvm.com")
  println(john.toHtml)
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

  // part 2 - implicits
  object HTMLSerializer {
    def serialize[T](value: T)(implicit serializer: HTMLSerializer[T]): String =
      serializer.serialize(value)

    def apply[T](implicit serializer: HTMLSerializer[T]) = serializer
  }

  implicit object HtmlSerializer extends HTMLSerializer[User] {
    override def serialize(user: User): String = s"<div>${user.name}</div>"
  }
  println(HTMLSerializer.serialize(john))

  implicit object IntSerializer extends HTMLSerializer[Int] {
    override def serialize(value: Int): String = s"<div style=color:blue>$value</div>"
  }
  println(HTMLSerializer.serialize(1))

  // access to the entire type class interface
  println(HTMLSerializer[User].serialize(john))

  implicit class HTMLEnrichment[T](value: T) {
    def toHTML(implicit serializer: HTMLSerializer[T]): String = serializer.serialize(value)
  }

  println(john.toHTML(UserSerializer))
  println(2.toHTML)
  println(john.toHTML(PartialUserSerializer))

  def htmlBoilerPlate[T](content: T)(implicit serializer: HTMLSerializer[T]): String = {
    s"<html><body>${content.toHTML(serializer)}</body></html>"
  }

  def htmlSugar[T : HTMLSerializer](content: T) = {
    //val serializer = implicitly[HTMLSerializer[T]]
    s"<html><body>${content.toHTML}</body></html>"
    //s"<html><body>${content.toHTML(serializer}</body></html>"
  }

  // implicitly
  case class Permissions(mask: String)
  implicit val defaultPermissions: Permissions = Permissions("0744")

  // in some other part of the code
  val standardperms = implicitly[Permissions] // super simple and smart implementation
}
