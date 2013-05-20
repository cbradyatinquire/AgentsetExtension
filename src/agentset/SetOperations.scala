package agentset


import org.nlogo.api._


/**
 * Created by IntelliJ IDEA.
 * User: cbrady
 * Date: 5/16/13
 * Time: 5:28 PM
 * To change this template use File | Settings | File Templates.
 */
class SetOperations extends DefaultClassManager {
  /**
   * Loads the primitives in the extension. This is called once per model compilation.
   *
   * @param pm The manager to transport the primitives to NetLogo
   */
  override def load( pm: PrimitiveManager )  {
    pm.addPrimitive("intersection", Intersection)
    pm.addPrimitive("difference", Difference)
    pm.addPrimitive("union", Union)
    pm.addPrimitive("sym-difference", SymDifference)
    pm.addPrimitive("groups-of", GroupsOfSizeN)
    pm.addPrimitive("make-n-groups", NGroups)
    pm.addPrimitive("group-by", GroupByReporter )
  }


  def buildAgentSetFrom(k: AgentKind, inp: Set[Agent]): org.nlogo.api.AgentSet = {
    val asBuilder = new org.nlogo.agent.AgentSetBuilder( k, inp.size )
    val it = inp.toIterator
    it.foreach(x => asBuilder.add( x.asInstanceOf[org.nlogo.agent.Agent] ))
    asBuilder.build()
  }


  def performBinarySetOp( set1: AgentSet, set2: AgentSet, f: (Set[Agent], Set[Agent]) => Set[Agent] ): org.nlogo.api.AgentSet = {
    val k1 = set1.kind
    val k2 = set2.kind

    if ( k2 != k1 ) {
      throw new ExtensionException("AgentSets must be of the same kind.\nThe first argument was of kind " + k1.toString + " while the second was of kind " + k2.toString + ".")
    }
    else {
      import scala.collection.JavaConverters._
      val sa = set1.agents.asScala.toSet
      val sb = set2.agents.asScala.toSet

      buildAgentSetFrom(k1, f(sa, sb))
    }
  }

  trait BinarySetReporter extends DefaultReporter {
    def functionToApply: (Set[Agent], Set[Agent]) => Set[Agent]
    override def getSyntax: Syntax = Syntax.reporterSyntax( Array(Syntax.AgentsetType, Syntax.AgentsetType), Syntax.AgentsetType )
    override def report(args: Array[Argument], ctxt: Context): AnyRef = {
      val set1 = args(0).getAgentSet
      val set2 = args(1).getAgentSet
      performBinarySetOp(set1, set2, (x: Set[Agent], y: Set[Agent]) => functionToApply(x, y) )
    }
  }

  object Intersection extends BinarySetReporter {
    override def functionToApply = (x: Set[Agent], y: Set[Agent]) => x intersect y
  }

  object Difference extends BinarySetReporter {
     override def functionToApply = (x: Set[Agent], y: Set[Agent]) => x diff y
  }

  object Union extends BinarySetReporter {
    override def functionToApply = (x: Set[Agent], y: Set[Agent]) => x union y
  }

  object SymDifference extends BinarySetReporter {
    override def functionToApply = (x: Set[Agent], y: Set[Agent]) => (x diff y) union (y diff x)
  }

  object NGroups extends DefaultReporter {
    override def getSyntax: Syntax = Syntax.reporterSyntax( Array(Syntax.AgentsetType, Syntax.NumberType), Syntax.ListType )

    override def report(args: Array[Argument], ctxt: Context): AnyRef = {
      val agentSet = args(0).getAgentSet
      val k = agentSet.kind
      val numBins = args(1).getIntValue
      if ( numBins < 1) {
        throw new ExtensionException("Number of bins must be greater than or equal to one.")
      }

      import scala.collection.JavaConverters._
      val agents = agentSet.agents.asScala.toList
      val agentMap = agents.zipWithIndex groupBy {case (agent, index) => index % numBins}

      val listBuilder = new LogoListBuilder()

      agentMap.map {
        case (value, list) => list
      }.map {
        case tupleList => tupleList.map{ case (agent,index) => agent }
      }.foreach ( x => listBuilder add buildAgentSetFrom(k, x.toSet)  )

      listBuilder.toLogoList
    }
  }

  object GroupsOfSizeN extends DefaultReporter {
    override def getSyntax: Syntax = Syntax.reporterSyntax( Array(Syntax.AgentsetType, Syntax.NumberType), Syntax.ListType )

    override def report(args: Array[Argument], ctxt: Context): AnyRef = {
      val agset = args(0).getAgentSet
      val k = agset.kind
      val groupSize = args(1).getIntValue
      if ( groupSize < 1) {
        throw new ExtensionException("Size of group must be greater than or equal to one.")
      }

      import scala.collection.JavaConverters._
      val sa = agset.agents.asScala.toSet
      val bins = sa.grouped(groupSize)
      val lbuilder = new LogoListBuilder()

      bins.foreach( x => lbuilder.add( buildAgentSetFrom(k, x)) )
      lbuilder.toLogoList
    }
  }


  object GroupByReporter extends DefaultReporter {
    override def getSyntax: Syntax = Syntax.reporterSyntax( Array(Syntax.AgentsetType, Syntax.ReporterTaskType), Syntax.ListType )

    override def report( args:Array[Argument], ctxt: Context): AnyRef = {

      val agentSet = args(0).getAgentSet
      val k = agentSet.kind
      import scala.collection.JavaConverters._
      val sAgentSet = agentSet.agents.asScala.toSet

      val task = args(1).getReporterTask

      val valueMap = sAgentSet.map( agent => Array[AnyRef](agent) ).groupBy( singleton => task.report(ctxt, singleton))
      val flattened = valueMap.mapValues( x => x.flatten )
      val lbuilder = new LogoListBuilder()

      flattened.foreach{
        case (value, agentset) =>
          val innerBuilder = new LogoListBuilder
          innerBuilder.add( value )
          innerBuilder.add( buildAgentSetFrom(k, agentset.asInstanceOf[Set[Agent]]) )
          lbuilder.add( innerBuilder.toLogoList )
      }
      lbuilder.toLogoList
    }
  }


}
