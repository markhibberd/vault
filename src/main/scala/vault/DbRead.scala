package vault

import java.sql.Connection
import scalaz._, Scalaz._
import scalaz.stream._, Process.Sink, Process.ProcessTC
import scalaz.concurrent._

case class DbRead(connection: Connection, chunk: Int, trace: Sink[Task, DbLog]) {
  def withChunkSize(size: Int): DbRead =
    copy(chunk = size)

  def withStandardOutTrace: DbRead =
    copy(trace = io.stdOutLines.map(_.contramap((_: DbLog).toString)))

  def withAccumulatedTrace(buffer: scala.collection.mutable.ArrayBuffer[DbLog]): DbRead =
    copy(trace = ((l: DbLog) => Task.delay { buffer += l; () }).pure[ProcessTC[Task]#f])
}

object DbRead {
  def DefaultChunkSize = 100

  def connect(connection: Connection): DbRead =
    DbRead(connection, DefaultChunkSize, ((l: DbLog) => Task.now { () }).pure[ProcessTC[Task]#f])

  def capture(connection: Connection, buffer: scala.collection.mutable.ArrayBuffer[DbLog]): DbRead =
    connect(connection).withAccumulatedTrace(buffer)
}
