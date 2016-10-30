package scratch

import org.scalatest.{FeatureSpec, GivenWhenThen, Matchers}

import scala.util.Random

class DistributeWorkloadEvenlySpec extends FeatureSpec with Matchers with GivenWhenThen {

  feature("Distribute tasks evenly across a set of servers by their estimated execution time") {

    scenario("a single server receives all tasks") {
      Given("a single server")
      val server = new Server()

      And("a set of tasks with random execution times")
      val taskCount = 100
      val tasks = generateTasks(taskCount)
      val totalTaskExecutionTime = sumTaskExecutionTime(tasks)

      When("the tasks are distributed")
      val distributor = new Distributor(Seq(server))
      tasks foreach distributor.assign

      Then("the single server executes all tasks")
      server.totalExecutionTime shouldBe totalTaskExecutionTime
    }

    scenario("a set of servers receives an even distribution of tasks") {
      Given("a set of servers")
      val serverCount = 2 + Random.nextInt(100)
      val servers = 0 until serverCount map { _ => new Server }

      And("and a random number of tasks that can be evenly distributed")
      val tasksPerServer = 100 + Random.nextInt(100)
      val tasks = generateTasks(serverCount * tasksPerServer)
      val totalTaskExecutionTime = sumTaskExecutionTime(tasks)

      When("the tasks are distributed")
      val distributor = new Distributor(servers)
      tasks foreach distributor.assign

      Then("each server executed the same amount")
      val averageExecutionTimePerServer = totalTaskExecutionTime / serverCount
      servers foreach { server =>
        Math.abs(server.totalExecutionTime - averageExecutionTimePerServer) should be <= maxTaskTime
      }
    }
  }

  val minTaskTime = 100L
  val maxTaskTime = 5000L

  def generateTasks(taskCount: Int): IndexedSeq[Task] = {
    0 until taskCount map { _ =>
      Task(minTaskTime + Random.nextInt((maxTaskTime - minTaskTime).toInt))
    }
  }

  def sumTaskExecutionTime(tasks: IndexedSeq[Task]): Long = {
    tasks.foldLeft(0L)((total, task) => total + task.executionTime)
  }
}
