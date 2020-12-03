package central

case class RunnableGraph(graphId: String, nodes: List[TaskRunnable], dependencies: List[(String, String)])

object RunnableGraph {
  // build runnable graph from task graph
  def fromTaskGraph(expInstance: ExpInstance, taskGraph: TaskGraph): RunnableGraph =
    RunnableGraph(
      graphId = taskGraph.graphId,
      nodes = taskGraph.nodes.map { u => TaskRunnable.fromInstance(expInstance, u) },
      dependencies = taskGraph.dependencies
    )
}
