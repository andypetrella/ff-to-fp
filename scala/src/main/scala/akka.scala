package be.wajug

object showAkka extends App {
  import akka.actor._
  import akka.util.duration._
  import akka.actor.SupervisorStrategy._
  import com.typesafe.config._
  import scala.io._
  import java.io.{File, FileWriter}

  //custom exception
  case class MyException[A](initialValue:A) extends Exception


  //convertor actor
  class Convertor extends Actor {

    val output = {
      val o = File.createTempFile("akka", ".out")
      println(o.getPath)
      new FileWriter(o)
    }

    var toBeAdapted = 0
    var adapted = 0

    var errors = 0

    def receive = {
      case ReadFile(f:File) => {
        Source.fromFile(f).getLines.zipWithIndex.foreach { case (line, i) =>
          toBeAdapted = toBeAdapted + 1
          val adapterRef = context.actorOf(Props[LineAdapter], "adapter_" + i)
          adapterRef ! AdaptLine(line)
        }
      }
      case LineAdapted(l) => {
        output.append(l+"\n")

        adapted = adapted + 1
      }

      case Done => {
        output.close
        self ! PoisonPill
      }

      case Error => {
        errors += 1
        sender ! PoisonPill
      }

      case Errors => sender ! errors

      case Count => sender ! adapted

    }

    override val supervisorStrategy = OneForOneStrategy(maxNrOfRetries = -1) {
      case r: MyException[_] => Restart
      case d: RuntimeException => Restart
    }

  }



  //adapter actor
  class LineAdapter extends Actor {

    def adapt(l:String):String = l.reverse

    def receive = {
      case AdaptLine(l:String) => {
        if (scala.util.Random.nextBoolean) {
          sender ! LineAdapted(adapt(l))
        } else {
          if (scala.util.Random.nextBoolean) {
            throw new RuntimeException("Bang!")
          } else {
            throw new MyException(l)
          }
        }
      }
    }

    override def preRestart(reason: Throwable, message: Option[Any]) {
      message.foreach{m =>
        reason match {
          case MyException(l:String) => {
            sender ! LineAdapted(adapt(l))
          }
          case _ => sender ! Error
        }
      }
    }
  }



  //Messages

  case class ReadFile(f:File)
  case class AdaptLine(l:String)
  case class LineAdapted(l:String)
  case object Error
  case object Errors
  case object Count
  case object Done



  // GO !

  //config
  val config = ConfigFactory.load()
  val system = ActorSystem("MySystem", config)

  //and actor ref
  val convertor:ActorRef = system.actorOf(Props[Convertor], "convertor")


  //ask to process
  val file = args.toList.headOption getOrElse ("/home/noootsab/src/talks/FF-to-FP/scala/src/main/resources/text.txt")
  convertor ! ReadFile(new File(file))



  //some progress checks
  import akka.util.duration._
  import akka.util.Timeout
  import akka.pattern.ask
  implicit val timeout = Timeout(5 seconds)
  //Could use Akka schedule for that
  (1 to 100) foreach { _ =>
    if (!system.isTerminated) {
      (convertor ? Count) onComplete println
      Thread.sleep(1)
    }
  }


  //termination
  println("Every thing should be ok...")
  (convertor ? Count) onComplete println
  (convertor ? Errors) onComplete println
  convertor ! Done


  //stop
  system.shutdown()

}