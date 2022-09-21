import java.util.UUID

object TestApp extends App {

    abstract class Node

    case class Predicate(op: LogicalOperator, val right: String, val left: String) extends Node

    case class Expression(operator: Operator, left: Node, right: Node) extends Node

    enum Operator {
        case AND, OR, NOT
    }

    enum LogicalOperator {
        case LessThan, LessThanOrEqual, GreaterThan, GreaterThanOrEqual, NotEqual, Equals
    }

    // Recursive helper function
    def evaluateNode(node: Node): Boolean = 
        node match {
            case pred @ Predicate(op, leftVal, rightVal) => evaluatePredicate(pred)
            case Expression(op, left, right) => op match {
                case Operator.AND => evaluateNode(left) && evaluateNode(right)
                case Operator.OR => evaluateNode(left) || evaluateNode(right)
                case Operator.NOT => !evaluateNode(left)
            }
        }
    

    def evaluatePredicate(pred: Predicate): Boolean = pred.op match
            case LogicalOperator.GreaterThan => pred.right > pred.left
            case LogicalOperator.GreaterThanOrEqual => pred.right >= pred.left
            case LogicalOperator.LessThan => pred.right < pred.left
            case LogicalOperator.LessThanOrEqual => pred.right <= pred.left
            case LogicalOperator.NotEqual => pred.right != pred.left
            case LogicalOperator.Equals => pred.right == pred.left


    val x = Predicate(LogicalOperator.NotEqual, "2", "2")

    println(evaluateNode(x))
    println(UUID(0L, 0L).toString() == "00000000-0000-0000-0000-000000000000")
}


//def debug(str: String) = println(s"Debug: ${str}")