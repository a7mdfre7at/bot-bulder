import scala.annotation.tailrec

abstract class Response {
  val name: String
  val body: String
  val isBlocked: Boolean
  val isSaved: Boolean
  val conditions: List[Condition]
}


//######################################################
case class TextResponse(override val body: String, override val conditions: List[Condition]) extends Response {

  override val isSaved: Boolean = false

  override val name: String = ""

  override val isBlocked: Boolean = false

  override def toString: String = body
}


//######################################################
case class PromptResponse(override val body: String, override val conditions: List[Condition], override val isSaved: Boolean = false,
    override val name: String
) extends Response {
  override val isBlocked: Boolean = true

  override def toString: String = body
}


//######################################################
case class QuickResponse(
    override val body: String,
    val resposeOptions: List[String],
    override val conditions: List[Condition],
    override val isSaved: Boolean = false,
    override val name: String
) extends Response {
  override val isBlocked: Boolean = true

  override def toString: String = {
    @tailrec
    def loop(
        options: List[String],
        acc: StringBuilder = new StringBuilder(""),
        counter: Int = 1
    ): String = {
      options match {
        case Nil => acc.toString.trim
        case head :: tail =>
          loop(tail, acc.append(s"${counter}. ${head}\n"), counter + 1)
      }
    }

    s"${body}\n${loop(resposeOptions)}"
  }
}

// //######################################################
// case class CloseResponse(val body: String) extends Response {
//     override def toString(): String = body
// }

case class CloseResponse(override val body: String) extends Response {

  override val conditions: List[Condition] = List()

  override val isSaved: Boolean = false

  override val name: String = ""

  override val isBlocked: Boolean = false

  override def toString: String = body
}