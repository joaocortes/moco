/*******************************************************************************
 * SAT4J: a SATisfiability library for Java Copyright (C) 2004, 2012 Artois University and CNRS
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 *  http://www.eclipse.org/legal/epl-v10.html
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU Lesser General Public License Version 2.1 or later (the
 * "LGPL"), in which case the provisions of the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of the LGPL, and not to allow others to use your version of
 * this file under the terms of the EPL, indicate your decision by deleting
 * the provisions above and replace them with the notice and other provisions
 * required by the LGPL. If you do not delete the provisions above, a recipient
 * may use your version of this file under the terms of the EPL or the LGPL.
 *
 * Contributors:
 *   CRIL - initial API and implementation
 *   João O'Neill Cortes, INESC
 *******************************************************************************/
package org.sat4j.moco.problem;


import org.sat4j.moco.util.Log;
import java.util.HashMap;
import java.util.Vector;
import java.util.HashSet;
import java.util.TreeSet;
import java.util.Hashtable;
import java.util.PriorityQueue;
import java.util.SortedMap;
import java.util.ArrayList;
import java.util.SortedSet;
import java.util.TreeMap


import org.sat4j.core.ReadOnlyVec;
import org.sat4j.core.ReadOnlyVecInt;
import org.sat4j.core.VecInt;
import org.sat4j.moco.util.Real;
import org.sat4j.moco.pb.PBFactory;
import org.sat4j.moco.pb.PBSolver;
import org.sat4j.moco.problem.GenTotalEncoder.SumTree.Node;
import org.sat4j.specs.IVecInt;
import org.sat4j.specs.ContradictionException;

/**
 * Class with the implementation of the generalized totalizor encoder.
 * Notice that the differential is both a value and an index starting at 0
 * @author Joao O'Neill Cortes
 */


 public class GenTotalEncoder implements GoalDelimeter {

     class SumTree {

	 private int upperLimit = -1;
	 private Node parent = null;
	 private ArrayList<Node> nodes = new ArrayList<Node>();
	 private PriorityQueue<Node> unlinkedNodes = new PriorityQueue<Node>((a,b) -> a.nodeSum - b.nodeSum);

	 class Node {
	     class NodeVars{
		 class NodeVar {
		     private int kD;
		     private int id;

		     public NodeVar(int kD, int upperLimit){
			 this.setKD(kD, upperLimit);
			 this.setId(-1);
		     }

		     public int getId(){return this.id;}
		     public int getKD(){return this.kD;}

		     public void setKD(int newKD, int upperLimit){
			 this.kD = newKD;
			 this.kD = this.cutValue(upperLimit);
		     }
		     public void setId(int id){
			 assert this.id == -1;
			 // id = GenTotalEncoder.this.newSVar(iObj, kD);
			 this.id = id;
		     }
		     private int cutValue(int upperLimit){
			 return this.kD > upperLimit + 1? this.kD : upperLimit + 1;
		     }
		     public void baptize(int id, int upperLimit){
			 if(this.getKD()<= upperLimit)
			     this.setId(id);
		     }
		 }
		 private PriorityQueue<Node> containerUsed = new PriorityQueue<Node>((a,b) -> a.nodeSum - b.nodeSum);
		 private PriorityQueue containerAll = null;
		 private ArrayList<NodeVar> containerUnused = null;

		 public NodeVars(){
		     this.container = new HashMap<Integer, NodeVar>();
		     this.lastClausified = 0;
		 }

		 public void add(int kD, int upperLimit){
		     this.container.putIfAbsent(kD, new NodeVar(kD, upperLimit));
		 }

		 public void addWhileClausing(int kD, int upperLimit){
		     this.container.putIfAbsent(kD, new NodeVar(kD, upperLimit));
		 }

	     }	     

	     NodeVars nodeVars = null;
	     private int nodeSum = 0;
	     private Node left = null;
	     private Node right = null;
	     
	     public Node(int weight, int upperLimit){
		 this.nodeSum = weight;
		 this.left = null; 
		 this.right = null;
		 this.nodeVars = new NodeVars();
		 this.nodeVars.add(0, upperLimit);
		 this.nodeVars.add(this.nodeSum, upperLimit);
	     }
	     
	     public Node(Node left, Node right, int upperLimit){
		 this.left = left;
		 this.right = right;
		 this.nodeVars =  new ArrayList<NodeVar>();
		 this.nodeVars.add(new NodeVar(0, upperLimit ));
		 this.generateVars(upperLimit);
		 this.nodeSum = left.nodeSum + right.nodeSum;
	     }


	     private void generateVars(int upperLimit){
		 Vector<Integer> values =  new Vector<Integer>() ;
		 for( NodeVar nodeVarLeft : left.nodeVars)
		     for(NodeVar  nodeVarRight : right.nodeVars)
			 values.add(nodeVarLeft.getKD() + nodeVarRight.getKD());
		 values.sort(null);
		 for(int value : values) {
		     this.nodeVars.add(new NodeVar(value, upperLimit));
		 }
	     }




	 }

	 
	 public SumTree(int[] leafWeights, int upperLimit){
	     this.upperLimit = upperLimit;
	     
	     for(int weight : leafWeights){
		Node node =  new Node(weight);
	     this.unlinkedNodes.add(node);
	     }
	     linkTree();
	     this.parent = this.unlinkedNodes.poll();
	 }
	 
	 //TODO what happens if there is only one node left in unlinkedNodes
	 public void linkTree(){
	     while(this.unlinkedNodes.poll() != null){
		 Node leftNode = unlinkedNodes.poll();
		 this.nodes.add(leftNode);
		 Node rightNode = unlinkedNodes.poll();
		 this.nodes.add(rightNode);
		 Node parentNode = new Node(leftNode, rightNode, this.upperLimit);
		 unlinkedNodes.add(parentNode);
	     }
	 }

	 // public void updateUpperKD(int upperKD){
	 //     this.upperLimit = upperKD;
	 //     Node currentNode = this.parent;


	     
	 // }
     }

     /**
      *Tree used to encode the goal limits
      */
     private SumTree[] sumTrees = null;
     
     /**
      *Acess to the instance to be solved
      */
    private Instance instance = null;
     /**
      *Access to the solver being used
      */
    private PBSolver solver = null;

    /**
     *The inverse index map for the S(um) variables. For each ID, a
     *value that is an array vector with the value of the goal and the
     *value of the sum
     */
    private Hashtable<Integer,int[]> sVariablesInverseIndex  = new Hashtable<Integer, int[]>();

    private int firstVariable = 0;

    /**
     * Creates an Instance of the generalized totalizor encoder
     * @param instance, the pseudo boolean instance
     * @param solver, the solver to be updated
     */

    public GenTotalEncoder(Instance instance, PBSolver solver) {
	Log.comment(5, "in GenTotalEncoder");
	this.instance = instance;	
	this.solver = solver;
	this.firstVariable = this.solver.nVars() + 1;

	for(int iObj = 0;iObj< instance.nObjs(); ++iObj){
	    this.UpdateCurrentK(iObj,0);
	}
	Log.comment(5, "done");
    }



     /**
      *TODO check the second indice
      */
     public int getIObjFromY(int id){
	 assert this.isY(id);
	 return this.sVariablesInverseIndex.get(id)[0];
     }

     /**
      *TODO check the second indice
      */
     public int getKDFromY(int id){
	 assert this.isY(id);
	 return this.sVariablesInverseIndex.get(id)[1];

     }


     //TODO wrong: the second argument should be kD, not iKD
     public int getY(int iObj, int iKD){
	 return this.sumTrees[iObj].parent.nodeVars.get(iKD).getId();
     }


     public boolean isY(int literal){
	 int id = this.solver.idFromLiteral(literal);
	 if(this.sVariablesInverseIndex.containsKey(id))
	     return true;
	 return false;
     }

     public void prettyPrintVecInt(IVecInt vecInt){
	 for(int j = 0; j < vecInt.size(); ++j)
	     this.prettyPrintLiteral(vecInt.get(j));

	 return;
     }

    public void prettyPrintLiteral(int literal){
	int sign =(literal>0)? 1: -1;
	int id =  literal * sign;
	// if(this.isY(id)){
	//     int iObj = this.getIObjFromY(id);
	//     int kd = this.getKDFromY(id);
	//     Log.comment(6, literal + "->" + "STop[" + iObj + ", " + kd +"] ");
	//     return;
	// }
	 
	if(this.isY(id)){
	    int iObj = this.getIObjFromY(id);
	    int kd = this.getKDFromY(id);
	    Log.comment(6, literal + "->" + "Y[" + iObj + ", " + kd +"] ");
	    return;
	}
	if(id < this.firstVariable){
	    Log.comment(6, (sign>0? "+":"-")+"X["+id+"] ");
	}
    }


     /**
      *Return the ID of a freshly created solver's variable
      */
     public int newSVar(int sum, int iObj){
	 this.solver.newVar();
	 int id = this.solver.nVars();
	 this.sVariablesInverseIndex.put(id, new int[]{sum, iObj});
	 return id;
     }


     
	 }
	 
	 while(currentNode.left != currentNode){
	     baptizeSubTree(currentNode.left, iObj, newUpperLimit);
	     baptizeSubTree(currentNode.right, iObj, newUpperLimit);
	 }
     }
     
     public void baptizeSumTrees(int newUpperLimit){
	 int iObj = 0;
	 SumTree sumTree = this.sumTrees[iObj];
	 SumTree.Node currentNode = sumTree.parent;
	 baptizeSubTree(currentNode, iObj, newUpperLimit);

     }

     //TODO
     public void UpdateCurrentK(int iObj, int upperKD){

     }


 }



