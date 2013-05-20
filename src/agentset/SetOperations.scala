package agentset


//import org.nlogo.app.App
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
    //pm.addPrimitive("group-by", GroupByReporter )
    //see comments below. threading issues.
  }

  //def makeAgentSetFrom(k: AgentKind, inp: Set[Agent]): org.nlogo.api.AgentSet = {
    //  org.nlogo.agent.AgentSet.fromArray(k, inp.toArray(new Agent[] ))   //.toArray(new Agent[inp.size] )
    //}

  def buildAgentSetFrom(k: AgentKind, inp: Set[Agent]): org.nlogo.api.AgentSet = {
    val asBuilder = new org.nlogo.agent.AgentSetBuilder( k, inp.size )
    val it = inp.toIterator
    while ( it.hasNext ) {
      asBuilder.add( it.next().asInstanceOf[org.nlogo.agent.Agent] )
    }
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
        val agset = args(0).getAgentSet
        val k = agset.kind
        val numBins = args(1).getIntValue
        if ( numBins < 1) {
          throw new ExtensionException("Number of bins must be greater than or equal to one.")
        }

        import scala.collection.JavaConverters._
        val agList = agset.agents.asScala.toList
        val mapOfAgList = agList.zipWithIndex groupBy( x => x._2 % numBins )

        val lbuilder = new LogoListBuilder()
        mapOfAgList.foreach{ y  =>
          val z = y._2
          val w = z.unzip
          lbuilder add buildAgentSetFrom(k, w._1.toSet)  }

        lbuilder.toLogoList
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


  //NOT USED -- need to find out how to deal with the threading issues.
  /*
  object GroupByReporter extends DefaultReporter {
      override def getSyntax: Syntax = Syntax.reporterSyntax( Array(Syntax.AgentsetType, Syntax.StringType), Syntax.ListType )

      override def report(args: Array[Argument], ctxt: Context): AnyRef = {
          val agset = args(0).getAgentSet
          val k = agset.kind
          val reporterstring = args(1).getString

          val sa = agset.agents.asScala.toSet
          val bins = sa.groupBy( x => applyNumericAgentReporter(x.asInstanceOf[org.nlogo.agent.Agent], reporterstring) )
          val lbuilder = new LogoListBuilder()

          bins.foreach( x  => {
               val lb2 = new LogoListBuilder()
               lb2 add Double.box(x._1)
               lb2 add buildAgentSetFrom(k, x._2)
               lbuilder.add( lb2.toLogoList )
             }
          )
          lbuilder.toLogoList
      }
  }

  //problem --> the report call hangs the application.  need a version of report that is like commandLater()
  def applyNumericAgentReporter( x:org.nlogo.agent.Agent, str:String ): Double = {
    val aggregated = "[" + str + "] of " + x.toString
    try {
      App.app.report( aggregated ).asInstanceOf[Double]
    }
   catch {
     case e:Exception => {
       println(e.getStackTrace)
       Double.box(0.0)
     }
    }
  }
  */


}
