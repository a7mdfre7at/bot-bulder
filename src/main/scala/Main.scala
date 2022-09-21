import scala.collection.mutable
import scala.annotation.tailrec
import java.util.UUID
import scala.collection.mutable.ListBuffer

object Main extends App {
  
//  println("msg.toString()")
  val bot = buildBot()
  val context: Map[String, String] = Map()

  val (msgs, ctx) = BotRunner.run("Hi", context, bot)

  for msg <- msgs do
    println(msg.toString())

  val (msgs1, ctx1) = BotRunner.run("Ahmad Freihat", ctx, bot)

  for msg <- msgs1 do
    println(msg.toString())

  val (msgs2, ctx2) = BotRunner.run("20-60", ctx1, bot)

  for msg <- msgs2 do
    println(msg.toString())

  val (msgs3, ctx3) = BotRunner.run("Wine", ctx2, bot)

  for msg <- msgs3 do
    println(msg.toString())

  //###########################################################

  def randomID: UUID = UUID.randomUUID()

  def trueCondition(id: UUID): Condition = Condition(truePredeicate, id)

  def trueConditions(id: UUID): List[Condition] = List(trueCondition(id))

  def buildBot(): WhatsappBot = {

    lazy val greetingTxtMsg = randomID -> TextResponse("Hi, welcome to whatsaap bot!", trueConditions(namePmptMsg._1))

    lazy val namePmptMsg = randomID -> PromptResponse("Please enter your name:", trueConditions(ageQckRplMsg._1), true, "name")

    lazy val ageQckRplMsg = randomID -> QuickResponse(
      "Please select your age from below options: ",
      List("Below 20", "20-60", "Over 60"),
      trueConditions(juiceQckRplMsg._1),
      true,
      "age"
    )

    lazy val juiceQckRplMsg = randomID -> QuickResponse(
      "Please select a drink from below options: ",
      List("Orange juice", "Wine"),
      List(
        Condition(
          Expression(
            operator = Operator.AND,
            left = Predicate(LogicalOperator.Equals, "drink", "Wine"),
            right = Predicate(LogicalOperator.Equals, "age", "20-60")
          ),
          goAwayTxtMsg._1
        ),
        Condition(
          Expression(
            operator = Operator.AND,
            left = Predicate(LogicalOperator.Equals, "drink", "Orange juice"),
            right = Predicate(LogicalOperator.Equals, "age", "20-60")
          ),
          hereIsYourDrink._1
        ),
        trueCondition(forbidden._1)
      ),
      true,
      "drink"
    )

    lazy val goAwayTxtMsg = randomID -> CloseResponse("We don't have any wine. Go away")

    lazy val hereIsYourDrink = randomID -> CloseResponse("Here is your drink")

    lazy val forbidden = randomID -> CloseResponse("You're either too young or too old to drink anything. Go away")

    WhatsappBot(
      greetingTxtMsg._1,
      Map(
        greetingTxtMsg,
        namePmptMsg,
        ageQckRplMsg,
        juiceQckRplMsg,
        goAwayTxtMsg,
        hereIsYourDrink,
        forbidden
      )
    )
  }

  lazy val somethingWentWrong = TextResponse(
    "Something went wrong",
    List(Condition(truePredeicate, UUID(0L, 0L)))
  )

  def incomprehensibleMsg(nextId: UUID) = TextResponse(
    "Sorry, we didn't understand you well!",
    List(Condition(truePredeicate, nextId))
  )

  lazy val currentResponseIdKey: String = "currentResponseId"

  extension (uuid: UUID) {
    def isEmpty: Boolean = uuid.toString == "00000000-0000-0000-0000-000000000000"
  }

  trait BotRuntime[Bot, Context, ContactMessage, BotMessage] {
    def run(
        contactMessage: ContactMessage,
        ctx: Context,
        bot: Bot
    ): (List[BotMessage], Context)
  }

  object BotRunner extends BotRuntime[WhatsappBot, Map[String, String], String, Response] {

    override def run(contactMessage: String, ctx: Map[String, String], bot: WhatsappBot): (List[Response], Map[String, String]) = {

      def getCurrentResponse(id: UUID): Response =
        getNext(id)

      def getNext(nextResponseID: UUID) = {
        bot.responses.getOrElse(nextResponseID, somethingWentWrong)
      }

      def getNextResponse(nextResponseID: UUID, context: Map[String, String], accResponses: List[Response] = List()): (List[Response], Map[String, String]) = {
        val next = getNext(nextResponseID)
        val updatedCtx = context.updated(currentResponseIdKey, nextResponseID.toString)
        val newAccRes = accResponses.appended(next)

        if next.isBlocked then
          (newAccRes, updatedCtx)
        else
          getResponse(Option(nextResponseID), updatedCtx, newAccRes)
      }

      def getResponse(responseID: Option[UUID], context: Map[String, String], accRessponses: List[Response] = List()): (List[Response], Map[String, String]) = {

        val resId = responseID match
          case Some(id) => id
          case None => UUID(0L, 0L)
        
        if resId.isEmpty then
          val updatedCtx =
            context.updated(currentResponseIdKey, bot.firstResponse.toString)
          val current = bot.responses.getOrElse(bot.firstResponse, somethingWentWrong)
          val newAccRes = accRessponses.appended(current)

          if current.isBlocked then (newAccRes, updatedCtx)
          else getResponse(Option(bot.firstResponse), updatedCtx, newAccRes)

        else
          val current = bot.responses.getOrElse(resId, bot.responses.head._2)
          val conditions = current.conditions

          if !conditions.isEmpty && conditions.length == 1 && evaluateExp(conditions.head.expression, context) then
            val nextResponseID = conditions.head.nextResponseID
            val next = getNext(nextResponseID)
            val updatedCtx = context.updated(currentResponseIdKey, nextResponseID.toString)
            val newAccRes = accRessponses.appended(next)

            if next.isBlocked then
              (newAccRes, updatedCtx)
            else
              getResponse(Option(nextResponseID), updatedCtx, newAccRes)

          else (accRessponses.appended(somethingWentWrong), context)
      }

      // call  when response is blocked (waits for user msg)
      def evaluate(currentResponseConditions: List[Condition], evalCtx: Map[String, String], resId: UUID): (List[Response], Map[String, String]) = {

        if evaluateExp(currentResponseConditions.head.expression, evalCtx) then
          val currentLocalResponse = getCurrentResponse(resId)
          if currentLocalResponse.conditions.length == 1 then
            return getResponse(Option(resId), evalCtx)

          val nextResponseID = currentResponseConditions.head.nextResponseID
          val updatedCtx = evalCtx.updated(currentResponseIdKey, nextResponseID.toString)
          return getNextResponse(nextResponseID, updatedCtx)

        if currentResponseConditions.tail.isEmpty then
          val wrongInputCondition = trueConditions(resId)
          List(TextResponse("Sorry, we didn't understand you well!", wrongInputCondition))

        evaluate(currentResponseConditions.tail, evalCtx, resId)
      }

      val currentResponseIDStr: String = ctx.getOrElse(currentResponseIdKey, "")
      if currentResponseIDStr.isEmpty then
        return getResponse(None, ctx)

      val currentResponseID: UUID = UUID.fromString(currentResponseIDStr)
      val currentResponse = getCurrentResponse(currentResponseID)

      if currentResponse.isInstanceOf[CloseResponse] then
        return (List(), ctx)

      val updatedCtx =
        if currentResponse.isSaved then
          ctx.updated(currentResponse.name, contactMessage)
        else ctx

      if currentResponse.isBlocked then 
        val result = currentResponse match
          case QuickResponse(_, ops, _, _, _) => {
            val cMsg = contactMessage.toLowerCase()
            val isWrongInput = !ops.exists(s => s.toLowerCase() == cMsg)
            if isWrongInput then (List(incomprehensibleMsg(currentResponseID)), updatedCtx)
            else evaluate(currentResponse.conditions, updatedCtx, currentResponseID)
          }
          case _ => evaluate(currentResponse.conditions, updatedCtx, currentResponseID)

        return result
      
      getResponse(None, updatedCtx)
    }
  }

  case class WhatsappBot(firstResponse: UUID, responses: Map[UUID, Response])




}