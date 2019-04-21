package lectures.part4implicits

  // HTMLSerializer is a TYPE CLASS
  // All the implementors of a type class are called type class instances.
  trait MyTypeClassTemplate[T] {
    def action(value: T): String
  }

  object MyTypeClassTemplate {
    def apply[T](implicit instance: MyTypeClassTemplate[T]) = instance
  }
