import java.util.UUID

//######################################################
case class Condition(
    val expression: BaseExpression,
    val nextResponseID: UUID
)

//######################################################
abstract class BaseExpression


//######################################################
case class Predicate(op: LogicalOperator, val left: String, val right: String)
    extends BaseExpression


//######################################################
case class Expression(
    operator: Operator,
    left: BaseExpression,
    right: BaseExpression
) extends BaseExpression


//######################################################
enum Operator {
  case AND, OR//, NOT
}


//######################################################
enum LogicalOperator {
  case LessThan, LessThanOrEqual, GreaterThan, GreaterThanOrEqual, NotEqual,
    Equals
}


//######################################################
def truePredeicate: Predicate = Predicate(LogicalOperator.Equals, "true", "true")


//######################################################
// Recursive helper function
def evaluateExp(node: BaseExpression, ctx: Map[String, String]): Boolean =
  node match {
    case pred @ Predicate(_, _, _) => evaluatePredicate(pred, ctx)
    case Expression(op, left, right) =>
      op match {
        case Operator.AND => evaluateExp(left, ctx) && evaluateExp(right, ctx)
        case Operator.OR  => evaluateExp(left, ctx) || evaluateExp(right, ctx)
        // case Operator.NOT => !evaluateExp(left, ctx)
      }
  }


//######################################################
def evaluatePredicate(pred: Predicate, ctx: Map[String, String]): Boolean = {
    val value: String = ctx.getOrElse(pred.left, "true")
    if value == "true" then return true

    pred.op match
      case LogicalOperator.GreaterThan        => value > pred.right
      case LogicalOperator.GreaterThanOrEqual => value >= pred.right
      case LogicalOperator.LessThan           => value < pred.right
      case LogicalOperator.LessThanOrEqual    => value <= pred.right
      case LogicalOperator.NotEqual           => value != pred.right
      case LogicalOperator.Equals             => value == pred.right
}