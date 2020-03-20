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
import java.util.Hashtable;
import java.util.Vector;
import java.util.HashSet;
import java.util.TreeSet;
import java.util.Iterator;

import java.util.PriorityQueue;
import java.util.SortedMap;
import java.util.ArrayList;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.Map;
import java.util.Collection;


import org.sat4j.core.ReadOnlyVec;
import org.sat4j.core.ReadOnlyVecInt;
import org.sat4j.core.VecInt;
import org.sat4j.moco.util.Real;
import org.sat4j.moco.pb.PBSolver;
import org.sat4j.moco.problem.GenTotalEncoder.SumTree.Node;
import org.sat4j.specs.IVecInt;

/**
 * Class with the implementation of the generalized totalizor encoder.
 * Notice that the differential is both a value and an index starting at 0
 * @author Joao O'Neill Cortes
 */


public class GenTotalEncoder extends GoalDelimeter {

    /**
     *The inverse index map for the partial sum variables. For each ID, a
     *value that is an array vector with the value of the goal and the
     *value of the sum
     */
	protected Hashtable<Integer,int[]> auxVariablesInverseIndex  = new Hashtable<Integer, int[]>();

    class SumTree {

	private int iObj = -1;
	private int upperLimit = -1;
	private Node parent = null;
	private ArrayList<Node> nodes = new ArrayList<Node>();
	private PriorityQueue<Node> unlinkedNodes = new PriorityQueue<Node>((a,b) -> a.nodeSum - b.nodeSum);


	class Node {
	    class NodeVars{
			   
		private TreeMap<Integer, NodeVar> containerAll = null;
		 
		class NodeVar {
		    private int kD;
		    private Integer id= null;

		    public NodeVar(int kD){
			this.setKD(kD);
		    }

		    public Integer getId(){return this.id;}
		    public int getKD(){return this.kD;}

		    public void setKD(int newKD){
			this.kD = newKD;
			this.kD = this.cutValue();
		    }

		    private int cutValue(){
			return this.kD > upperLimit + 1? upperLimit + 1 : this.kD;
		    }

		    /**
		     *Return the ID of a freshly created auxiliar variable
		     */
		    protected void setFreshId(){
			assert this.id == -1;
			solver.newVar();
			int id = solver.nVars();
			auxVariablesInverseIndex.put(id, new int[]{kD, iObj});
		    }
		}

		public NodeVars(){
		    this.containerAll = new TreeMap<Integer, NodeVar>();
		}

		public void add(int kD, int upperLimit){
		    this.containerAll.putIfAbsent(kD, new NodeVar(kD));
		}

		public NodeVar get(int value){
		    return this.containerAll.get(value);
		}
		public NodeVar addParsimoneously(int kD){
		    NodeVar nodeVar = this.containerAll.get(kD);
		    if(nodeVar == null){
			NodeVar newNodeVar = new NodeVar(kD);
			int effectiveKD = newNodeVar.kD;
			this.containerAll.put(effectiveKD, newNodeVar);
			nodeVar = newNodeVar;
		    }
		    return nodeVar;
		}

		/**
		 * Given iKD, returns the Id of the ceiling nodevar,
		 * that is, the id of the entry imediately above iKD.
		 */
		public int getCeilingId(int iKD){
		    return this.containerAll.ceilingEntry(iKD).getValue().id;
		}


		/**
		 * Given iKD, returns the value of the ceiling
		 * nodevar, that is, the values of the entry
		 * imediately above iKD.
		 * @param iKD
		 * @return value
		 */

		public int getCeilingValue (int iKD){
		    return this.containerAll.ceilingEntry(iKD).getValue().id;
		}

		public SortedMap<Integer, NodeVars.NodeVar> currentTail(){
		    return this.containerAll.tailMap(upperLimit);
		}


	    }	     
	    NodeVars nodeVars = null;
	    private int nodeSum = 0;
	    private Node left = null;
	    private Node right = null;
	     
	    public Node(int weight, int id){
		this.nodeSum = weight;
		this.left = null; 
		this.right = null;
		this.nodeVars = new NodeVars();
		this.nodeVars.addParsimoneously(0);
		NodeVars.NodeVar nodeVar = this.nodeVars.addParsimoneously(this.nodeSum);
		nodeVar.id = weight > 0 ? id: -id;
	    }
	     
	    public Node(Node left, Node right){
		this.left = left;
		this.right = right;
		this.nodeVars =  new NodeVars();
		this.nodeVars.addParsimoneously(0);
		this.nodeSum = left.nodeSum + right.nodeSum;
	    }


	    /**
	     *Generates new variables freed by updating the upper
	     *limit from currentUpperLimit to newUpperLimit. Upper
	     *limits are the largest possible attainable value
	     */
	    private void generateVars(int currentUpperLimit,int newUpperLimit){
		Vector<Integer> values =  new Vector<Integer>() ;
		Map<Integer, NodeVars.NodeVar> leftNewVars = this.left.nodeVars.containerAll.tailMap(currentUpperLimit);
		Map<Integer, NodeVars.NodeVar> rightNewVars = this.right.nodeVars.containerAll.tailMap(currentUpperLimit);

		for( NodeVars.NodeVar leftNodeVar : leftNewVars.values())
		    for( NodeVars.NodeVar rightNodeVar : rightNewVars.values())
			values.add(leftNodeVar.getKD() + rightNodeVar.getKD());
		 
		values.sort(null);
		for(int value : values) 
		    this.nodeVars.add(value, newUpperLimit);
	    }
	}

	public void setUpperLimit(int newUpperLimit){
	    this.upperLimit = newUpperLimit;
	}


	//TODO what happens if there is only one node left in unlinkedNodes
	public void linkTree(){
	    int size = unlinkedNodes.size();
	    while(size >=2){
	 	Node leftNode = unlinkedNodes.poll();
		this.nodes.add(leftNode);
		Node rightNode = unlinkedNodes.poll();
		this.nodes.add(rightNode);
		Node parentNode = new Node(leftNode, rightNode);
		unlinkedNodes.add(parentNode);
		size--;
	    }
	}

	public SumTree(int iObj, int[] leafWeights, int upperLimit){
	    this.iObj = iObj;
	    this.upperLimit = upperLimit;
	    int iX = 0;
	    for(int weight : leafWeights){
		Node node =  new Node(weight, iX);
		this.unlinkedNodes.add(node);
		iX ++;
	    }
	    linkTree();
	    this.parent = this.unlinkedNodes.poll();
	}
	 


    }

    /**
     *Tree used to encode the goal limits
     */

    private SumTree[] sumTrees = null;
     


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
	this.sumTrees = new SumTree[this.instance.nObjs()];
	    
	for(int iObj = 0, nObj = instance.nObjs() ;iObj< nObj; ++iObj){
	    Objective ithObj = this.instance.getObj(iObj);
	    ReadOnlyVec<Real> ithObjCoeffsReal = ithObj.getSubObjCoeffs(0);
	    int[] ithObjCoeffsInt = new int[ithObjCoeffsReal.size()];

	    for(int iX = 0, nX = ithObjCoeffsReal.size(); iX < nX; ++iX){
		ithObjCoeffsInt[iX] = Math.round(ithObjCoeffsReal.get(iX).asInt());
		ithObjCoeffsInt[iX] = ithObjCoeffsInt[iX] > 0 ? ithObjCoeffsInt[iX]: -ithObjCoeffsInt[iX];
}
	    this.sumTrees[iObj] = new SumTree(iObj ,ithObjCoeffsInt, -1);
	}

	for(int iObj = 0, nObj = instance.nObjs() ;iObj< nObj; ++iObj)
	    this.UpdateCurrentK(iObj,0);

	Log.comment(5, "done");
    }






     /**
      *TODO check the second indice
      */
     public int getIObjFromY(int id){
	 assert this.isY(id);
	 return this.auxVariablesInverseIndex.get(id)[0];
     }

     /**
      *TODO check the second indice
      */
     public int getKDFromY(int id){
	 assert this.isY(id);
	 return this.auxVariablesInverseIndex.get(id)[1];

     }


     //TODO wrong: the second argument should be kD, not iKD
     public int getY(int iObj, int iKD){
	 return this.sumTrees[iObj].parent.nodeVars.getCeilingId(iKD);
     }

     public boolean isY(int literal){
	 int id = this.solver.idFromLiteral(literal);
	 for(SumTree sumTree: this.sumTrees)
	     if(sumTree.parent.nodeVars.containerAll.containsKey(id))
		 return true;
	 return false;
     }



    public String prettyFormatVariable(int literal){
	int sign =(literal>0)? 1: -1;
	int id =  literal * sign;

	if(this.isY(id)){
	    int iObj = this.getIObjFromY(id);
	    int kd = this.getKDFromY(id);
	    return "Y[" + iObj + ", " + kd +"]"+ "::" + literal + " ";
	}
	 
	
	
	if(id < this.firstVariable){
	    return (sign>0? "+":"-")+"X["+id+"] ";
	}
	return "";
    }

    private boolean addClauseSequential(Node root){
	boolean change = false;
	boolean first = true;
	Node.NodeVars.NodeVar past;
	    Collection<Node.NodeVars.NodeVar> tail =
		root.nodeVars.currentTail().values();
	    Iterator<Node.NodeVars.NodeVar> it = tail.iterator();
	    past = it.next();
	    for(Node.NodeVars.NodeVar current: tail ){
		if(first)
		    first = false;
		else{
		    IVecInt clause = new VecInt(new int[] {-current.id, past.id});
		    AddClause(clause);
		    change = true;
		    
		}
	    };
	    return change;
    }

     public boolean addClausesSumTree(int iObj, int newUpperLimit){
	 boolean change = false;
	 SumTree ithObjSumTree = this.sumTrees[iObj];
	 	    change = change || addClausesSubSumTree(ithObjSumTree, ithObjSumTree.parent, newUpperLimit);
		    change = change || addClauseSequential(ithObjSumTree.parent);
		    return change;
     }


    


    
    private boolean addClausesFirstPartial(Node parent, Node first, Node second, int newUpperLimit){
	boolean change = false;
	Collection<Node.NodeVars.NodeVar> firstTail =
		first.nodeVars.currentTail().values();

	    Collection<Node.NodeVars.NodeVar> secondAll =
		first.nodeVars.containerAll.values();

	    for(Node.NodeVars.NodeVar firstVar : firstTail){
		for(Node.NodeVars.NodeVar secondVar : secondAll ){
		    Node.NodeVars.NodeVar parentVar =
			parent.nodeVars.addParsimoneously(firstVar.kD + secondVar.kD);
		    if(parentVar.getId() == null)
			parentVar.setFreshId();
		    IVecInt clause = new VecInt(new int[] {-firstVar.id, -secondVar.id, parentVar.id});
		    AddClause(clause);
		    change = true;
		}
	    }
	    return change;
	}


    public boolean addClausesSubSumTree(SumTree sumTree, Node currentNode, int newUpperLimit){
	boolean change = false;
	Node left = currentNode.left;
	Node right = currentNode.right;
	if(left != null) {
	    change = change || addClausesSubSumTree(sumTree, left, newUpperLimit);
	    change = change || addClausesSubSumTree(sumTree, right, newUpperLimit);
	    change = change || addClausesFirstPartial(currentNode, left, right, newUpperLimit);    
	    change = change || addClausesFirstPartial(currentNode, right, left, newUpperLimit);    
	}
	return change;
    }



    public void UpdateCurrentK(int iObj, int upperKD){
	boolean change = false;
	while(!change && upperKD < this.instance.getObj(iObj).getWeightDiff()){
	    change = addClausesSumTree(iObj, upperKD);
	    upperKD++;
	}
	this.sumTrees[iObj].setUpperLimit(upperKD-1);
    }

     public int getCurrentKD(int iObj){
	 return this.sumTrees[iObj].upperLimit;
     }
 }



