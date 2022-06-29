package net.degoes.zio

import zio._

object ForkJoin extends ZIOAppDefault {

  val printer =
    Console.printLine(".").repeat(Schedule.recurs(10))

  /**
   * EXERCISE
   *
   * Using `ZIO#fork`, fork the `printer` into a separate fiber, and then
   * print out a message, "Forked", then join the fiber using `Fiber#join`,
   * and finally, print out a message "Joined".
   */
  val run =
    for {
      fiber <- printer.fork
      _     <- Console.printLine("Forked")
      _     <- fiber.join
      _     <- Console.printLine("Joined")
    } yield ()
//    ZIO.fiberIdWith(Console.printLine(_)) *> printer
}

object ForkInterrupt extends ZIOAppDefault {

  val infinitePrinter =
    Console.printLine(".").forever

  /**
   * EXERCISE
   *
   * Using `ZIO#fork`, fork the `printer` into a separate fiber, and then
   * print out a message, "Forked", then using `ZIO.sleep`, sleep for 100
   * milliseconds, then interrupt the fiber using `Fiber#interrupt`, and
   * finally, print out a message "Interrupted".
   */
  val run =
    for {
      fiber <- infinitePrinter.fork
      _     <- ZIO.sleep(10.millis)
      exit  <- fiber.interrupt
      _     <- Console.printLine(exit)
    } yield ()
}

object ParallelFib extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Rewrite this implementation to compute nth fibonacci number in parallel.
   */
  def fib(n: Int): UIO[BigInt] = {
    def loop(n: Int, original: Int): UIO[BigInt] =
      if (n <= 1) ZIO.succeed(n)
      else
        ZIO.suspendSucceed {
          loop(n - 1, original).zipWithPar(loop(n - 2, original))(_ + _)
        }

    loop(n, n)
  }

  val run =
    (for {
      _ <- Console.printLine(
            "What number of the fibonacci sequence should we calculate?"
          )
      n <- Console.readLine.orDie.flatMap(input => ZIO.attempt(input.toInt)).eventually
      f <- fib(n)
      _ <- Console.printLine(s"fib(${n}) = ${f}")
    } yield ())
}

object TimeoutExample extends ZIOAppDefault {
  def fib(n: Int): UIO[Int] =
    if (n <= 1) ZIO.succeed(n)
    else
      ZIO.suspendSucceed {
        fib(n - 1).zipWith(fib(n - 2))(_ + _)
      }

  /**
   * EXERCISE
   *
   * Use `ZIO#timeout` to add a timeout to the following code so that
   * it doesn't run for more than 10 milliseconds.
   *
   * Print out a message if it timed out.
   */
  lazy val run = fib(200).timeout(10.millis).flatMap {
    case None         => Console.printLine("I need more time")
    case Some(answer) => Console.printLine(answer)
  }
}

object RaceExample extends ZIOAppDefault {
  def loadFromCache: Task[String] =
    ZIO.succeed("Loaded from cache!").delay(1.second)

  def loadFromDB: Task[String] =
    ZIO.succeed("Loaded from DB!").delay(500.millis)

  /**
   * EXERCISE
   *
   * Use `ZIO#race` to race the preceding two tasks and print out the
   * winning success value.
   *
   */
  lazy val run = run2
//    loadFromCache
//      .race(loadFromDB)
//      .tap(Console.printLine(_))

  // First which finish (even with error)
  lazy val run2 =
    loadFromCache.either.race(loadFromDB.either).tap(Console.printLine(_))
}

object AlarmAppImproved extends ZIOAppDefault {

  import java.io.IOException
  import java.util.concurrent.TimeUnit

  lazy val getAlarmDuration: ZIO[Any, IOException, Duration] = {
    def parseDuration(input: String): IO[NumberFormatException, Duration] =
      ZIO
        .attempt(
          Duration((input.toDouble * 1000.0).toLong, TimeUnit.MILLISECONDS)
        )
        .refineToOrDie[NumberFormatException]

    val fallback = Console.printLine("You didn't enter a number of seconds!") *> getAlarmDuration

    for {
      _        <- Console.printLine("Please enter the number of seconds to sleep: ")
      input    <- Console.readLine
      duration <- parseDuration(input) orElse fallback
    } yield duration
  }

  /**
   * EXERCISE
   *
   * Create a program that asks the user for a number of seconds to sleep,
   * sleeps the specified number of seconds using ZIO.sleep(d), concurrently
   * prints a dot every second that the alarm is sleeping for, and then
   * prints out a wakeup alarm message, like "Time to wakeup!!!".
   */
  val run = {
    val getAlarmDur: UIO[Duration] =
      Console
        .readLine("Enter seconds to sleep: ")
        .flatMap(line => ZIO.attempt(Duration(line.toInt, TimeUnit.SECONDS)).refineToOrDie[NumberFormatException])
        .eventually

    val infinitePrinter: ZIO[Any, IOException, Nothing] =
      (Console.print(".") *> ZIO.sleep(1.second)).forever

    val printWakeUp: IO[IOException, Unit] = Console.printLine("Time to wakeup!!!")
    for {
      d     <- getAlarmDur
      fiber <- infinitePrinter.fork
      _     <- fiber.interrupt.delay(d) *> printWakeUp
    } yield ()

    // version with race
//    for {
//      d <- getAlarmDur
//      _ <- ZIO.sleep(d).race(infinitePrinter)
//      _ <- printWakeUp
//    } yield ()
  }

}

object ParallelZip extends ZIOAppDefault {

  def fib(n: Int): UIO[Int] =
    if (n <= 1) ZIO.succeed(n)
    else
      ZIO.suspendSucceed {
        (fib(n - 1) zipWithPar fib(n - 2))(_ + _)
      }

  /**
   * EXERCISE
   *
   * Compute fib(10) and fib(13) in parallel using `ZIO#zipPar`, and display
   * the result.
   */
  val run =
    fib(10).tap(Console.printLine(_))
}

/**
 * The Ref data type is a way for ZIO effects to utilize state. It is basically
 * a concurrent-safe version of Scala's own `var`, but integrated into ZIO.
 */
object RefExample extends ZIOAppDefault {
  import zio.Random._

  import zio.Clock._
  import zio.stm._

//  for {
//    ref <- Ref.make(10)
//    v   <- ref.get
//    _   <- Console.printLine(v)
//    _   <- ref.set(11)
//    v   <- ref.get
//    _   <- Console.printLine(v)
//    _   <- ref.update(_ + 1)
//    v   <- ref.get
//    _   <- Console.printLine(v)
//  } yield ()

  /**
   * Some state to keep track of all points inside a circle,
   * and total number of points.
   */
  final case class PiState(
    inside: Ref[Long],
    total: Ref[Long]
  )

  /**
   * A function to estimate pi.
   */
  def estimatePi(inside: Long, total: Long): Double =
    (inside.toDouble / total.toDouble) * 4.0

  /**
   * A helper function that determines if a point lies in
   * a circle of 1 radius.
   */
  def insideCircle(x: Double, y: Double): Boolean =
    Math.sqrt(x * x + y * y) <= 1.0

  /**
   * An effect that computes a random (x, y) point.
   */
  val randomPoint: ZIO[Any, Nothing, (Double, Double)] =
    nextDouble zip nextDouble

  /**
   * EXERCISE
   *
   * Using `Ref#update`, make a function that adds a point into the state.
   * If the point is inside the circle then increment `PiState#inside`. In any
   * case, increment `PiState#total`.
   */
  def addPoint(point: (Double, Double), piState: PiState): UIO[Unit] = ???

  def incrementer(ref: Ref[Int], n: Int): UIO[Unit] =
    ref.update(_ + 1).repeatN(n - 1)
//    ref.get.flatMap(x => ref.set(x + 1)).repeatN(n - 1)

  def runForks(ref: Ref[Int]) =
    ZIO
      .foreachDiscard(1 to 100)(_ => incrementer(ref, 100).fork)

  val introductionToRef =
    for {
      ref <- Ref.make(0)
      _   <- runForks(ref)
      _   <- ref.get.tap(Console.printLine(_)).delay(1.second)
    } yield ()

  /**
   * EXERCISE
   *
   * Build a multi-fiber program that estimates the value of `pi`. Print out
   * ongoing estimates continuously until the estimation is complete.
   */
  val run = ???
}

object PromiseExample extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Do some computation that produces an integer. When yare done, complete
   * the promise with `Promise#succeed`.
   */
  def doCompute(result: Promise[Nothing, Int]): UIO[Unit] =
    result.succeed(42).unit.delay(5.second)

  /**
   * EXERCISE
   *
   * Fork the above computation in a separate fiber, giving it a promise
   * that it can use, and then wait for the promise to be completed,
   * using `Promise#await`.
   */
  lazy val waitForCompute: ZIO[Any, Nothing, Unit] =
    for {
      p <- Promise.make[Nothing, Int]
      _ <- doCompute(p).fork
      _ <- Console.printLine("Started computation in backround").orDie
      v <- p.await
      _ <- Console.printLine(v).orDie
    } yield ()

  val run =
    waitForCompute
}

object FiberRefExample extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Make the child increment the ref and see how the output of the program
   * changes.
   */
  def makeChild(ref: FiberRef[Int]) =
    for {
      _ <- ref.get.debug("child initial value")
      _ <- ref.update(_ + 1)
      _ <- ref.get.debug("child after update")
    } yield ()

  val run =
    for {
      ref    <- FiberRef.make[Int](0, identity(_), _ + _)
      _      <- ref.get.debug("parent before fork")
      child1 <- makeChild(ref).fork
      child2 <- makeChild(ref).fork
      _      <- ref.get.debug("parent after fork")
      _      <- child1.join
      _      <- child2.join
      _      <- ref.get.debug("parent after join")
    } yield ()
}
