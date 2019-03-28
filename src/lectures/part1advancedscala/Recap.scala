package lectures.part1advancedscala

object Recap extends App {

  val aCondition: Boolean = false
  val aConditionedVal = if (aCondition) 42 else 65
  // instructions vs expressions

  val aCodeBlock = {
    if (aCondition) 54
    56
  }


}
