package scratch

import scala.collection.mutable

class Distributor(servers: Seq[Server]) {
  private val serverToAssignedExecutionTime = mutable.Map[Server, Long]()

  def assign(task: Task): Unit = {
    val serverWithLeastExecutionTime = servers.minBy(server => totalAssignedExecutionTime(server))

    serverWithLeastExecutionTime.assign(task)

    addAssignedExecutionTime(serverWithLeastExecutionTime, task.executionTime)
  }

  def totalAssignedExecutionTime(server: Server): Long = {
    serverToAssignedExecutionTime.getOrElse(server, 0L)
  }

  def addAssignedExecutionTime(server: Server, taskTime: Long): Unit = {
    serverToAssignedExecutionTime(server) = totalAssignedExecutionTime(server) + taskTime
  }
}

case class Task(executionTime: Long)

class Server {
  var totalExecutionTime: Long = 0

  def assign(task: Task): Unit = {
    totalExecutionTime += task.executionTime
  }
}

