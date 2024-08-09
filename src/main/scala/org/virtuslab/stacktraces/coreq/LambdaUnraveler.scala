package org.virtuslab.stacktraces.coreq

import scala.quoted.*
import scala.tasty.inspector.*
import tastyquery.Trees.DefDef

class LambdaUnraveler private (
  private val previousLambdas: List[DefDef],
  private val previousFrames: Set[String],
  private val counter: Int = 0
):
  def getNextLambdaAndState(defdefs: List[DefDef], frameName: String): (Option[DefDef], LambdaUnraveler) =
    if defdefs.nonEmpty && !previousFrames.contains(frameName) && previousLambdas == defdefs then
      (Some(defdefs.reverse(counter)), new LambdaUnraveler(previousLambdas, previousFrames + frameName, counter + 1))
    else if defdefs.nonEmpty then
      (Some(defdefs.last), new LambdaUnraveler(defdefs, Set(frameName), 1))
    else
      (None, new LambdaUnraveler(defdefs, Set(frameName), 0))


object LambdaUnraveler:
  def apply(defdefs: List[DefDef]): LambdaUnraveler =
    new LambdaUnraveler(defdefs, Set.empty, 0)
