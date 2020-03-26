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
import java.util.Hashtable;
import java.util.Iterator;

import java.util.PriorityQueue;
import java.util.SortedMap;
import java.util.ArrayList;
import java.util.TreeMap;
import java.util.Collection;


import org.sat4j.core.ReadOnlyVec;
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
	/**
	 *must old the desired upperLimit, at any time. Desired is
	 *purposefully
	 */
	private int upperLimit = -1;
	/**
	 *Must old the last but effective upperLimit.
	 */
	private int olderUpperLimit = -1;
	/**
	 *The root of the SumTree.
	 */
	private Node parent = null;
	/**
	 *List of nodes. Simple array.
	 */
	private ArrayList<Node> nodes = new ArrayList<Node>();

	/**
	 *List of unlinked nodes. Discardable.
	 */
	private PriorityQueue<Node> unlinkedNodes = new PriorityQueue<Node>((a,b) -> a.nodeSum - b.nodeSum);

	/**
	 *Node of a SumTree.
	 */
	class Node {

	    /**
	     *Container of the variables associated with the SumTree
	     */
	    class NodeVars{

		/**
		 * Ordered map.key is the nodeSum, value is the variable.
		 */
		private TreeMap<Integer, NodeVar> containerAll = null;
		 
		/**
		 *variable representation. 
		 */
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
			assert this.id == null;
			solver.newVar();
			this.id = solver.nVars();
			auxVariablesInverseIndex.put(this.id, new int[]{kD, iObj, nodeName});
		    }
                   protected boolean newVariable(){
		       if(this.kD > olderUpperLimit)
			   return true;
		       return false;
		    }

		}
		public NodeVars(){
		    this.containerAll = new TreeMap<Integer, NodeVar>();
		}
		public void add(int kD){
		    this.containerAll.put(kD, new NodeVar(kD));
		}
		public NodeVar get(int value){
		    return this.containerAll.get(value);
		}
		/**
		 * Only adds a new nodeVar if there isn't one
		 * already. The kD value is normalized by the current
		 * upperLimit. A value exceeding the upperLimit is
		 * mapped to the upperLimit.
		 * @param kD
		 */

		public NodeVar addParsimoneously(int kD){
		    NodeVar nodeVar = this.containerAll.get(kD);
		    if(nodeVar == null){
			NodeVar newNodeVar = new NodeVar(kD);
			nodeVar = newNodeVar;
			int effectiveKD = newNodeVar.kD;
			this.containerAll.put(effectiveKD, newNodeVar);
			newNodeVar.setFreshId();
			// The dummy zero clause
			if(kD == 0)
			    AddClause( new VecInt(new int[] {newNodeVar.getId()}));
		    }
		    return nodeVar;
		}

		/**
		 * Given iKD, returns the Id of the ceiling nodevar,
		 * that is, the id of the entry with a key that is
		 * larger or equal to iKD.
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
		    return this.containerAll.tailMap(olderUpperLimit + 1);
		}

	    }	     

	    NodeVars nodeVars = null;
	    private int nodeSum = 0;
	    private Node left = null;
	    private Node right = null;
	    private int leafID = 0;
	    private int nodeName = 0;
	    
	    private boolean isActive(){
		if(this.nodeVars.containerAll.size() == 1)
		    return false;
		return true;
		
	    }

	    private boolean activateLeafNode(){
		if(!this.isActive())
		    if(this.nodeSum <= upperLimit){
			NodeVars.NodeVar nodeVar = this.nodeVars.addParsimoneously(this.nodeSum);
			nodeVar.id = this.nodeSum > 0? this.leafID: - this.leafID;
			auxVariablesInverseIndex.put(this.leafID, new int[]{nodeVar.kD, iObj, this.nodeName});
			return true;
		    }
		return false;
	    }

	    public Node(int weight, int iX){
		nodes.add(this);
		int sign = 1;
		if(weight < 0)
		     sign = -1;
		this.nodeName = nodes.size()-1;
		this.nodeSum = sign * weight;
		this.left = null; 
		this.right = null;
		this.nodeVars = new NodeVars();
		this.leafID = sign * iX;


	    }
	     
	    public Node(Node left, Node right){
		nodes.add(this);
		this.nodeName = nodes.size()-1;
		this.left = left;
		this.right = right;
		this.nodeVars =  new NodeVars();
		this.nodeSum = left.nodeSum + right.nodeSum;
	    }



	}

	public void setOlderUpperLimit(){
	    this.olderUpperLimit = this.upperLimit;
	}
	public void setUpperLimit(int newUpperLimit){
	    this.upperLimit = newUpperLimit;
	}
	/**
	 *Links the SumTree, in such a fashion that at any time all
	 *unlinked nodes are lighter than any linked node.
	 */
	public void linkTree(){
	    int size = unlinkedNodes.size();
	    while(size >=2){
	 	Node leftNode = unlinkedNodes.poll();
		Node rightNode = unlinkedNodes.poll();
		Node parentNode = new Node(leftNode, rightNode);
		unlinkedNodes.add(parentNode);
		size--;
	    }
	}

	public SumTree(int iObj, int[] leafWeights, int upperLimit){
	    this.iObj = iObj;
	    this.upperLimit = upperLimit;
	    int iX = 1;
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
     *Trees used to encode the goal limits
     */

    private SumTree[] sumTrees = null;
     

    /**
     *index of the first auxiliar variable
     */
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
	    }
	    this.sumTrees[iObj] = new SumTree(iObj ,ithObjCoeffsInt, -1);
	}

	for(int iObj = 0, nObj = instance.nObjs() ;iObj< nObj; ++iObj){
	    for(Node node: this.sumTrees[iObj].nodes)
		node.nodeVars.addParsimoneously(0);
	this.UpdateCurrentK(iObj,0);
	}
	Log.comment(5, "done");
    }

    public int getCurrentKD(int iObj){
	return this.sumTrees[iObj].upperLimit;
    }


    /**
     * get iObj from an Y variable
     */

    public int getIObjFromY(int id){
	assert this.isY(id);
	return getIObjFromS(id);
    }

    /**
     * get kD from an Y variable
     */
    public int getKDFromY(int id){
	assert this.isY(id);
	return this.getKDFromS(id);

    }

    /**
     * get iObj from an S variable
     */

    public int getIObjFromS(int id){
	assert this.isS(id);
	return this.auxVariablesInverseIndex.get(id)[1];
    }


    /**
     * get kD from an S variable
     */

    public int getKDFromS(int id){
	assert this.isS(id);
	return this.auxVariablesInverseIndex.get(id)[0];

    }

    /**
     * get name of the node, from an S variable
     */

    public int getNameFromS(int id){
	assert this.isS(id);
	return this.auxVariablesInverseIndex.get(id)[2];
    }


    /**
     *Tricky. This is not a real getter. Given kD, it returns the id
     *of the variable with a smaller kD, yet larger or equal to kD.
     */
    public int getY(int iObj, int kD){
	return this.sumTrees[iObj].parent.nodeVars.getCeilingId(kD);
    }


    /**
     *Checks if the variable in the literal is an Y variable.
     */
    public boolean isY(int literal){
	int id = this.solver.idFromLiteral(literal);
	if(!isS(literal))
	    return false;
	for(SumTree sumTree: this.sumTrees)
	    for(SumTree.Node.NodeVars.NodeVar nodeVar: sumTree.parent.nodeVars.containerAll.values())
		if(nodeVar.getId() == id)
		return true;
	return false;
    }

    /**
     *Checks if a variable is an X(original) variable.
     */
    public boolean isX(int literal){
	int id = this.solver.idFromLiteral(literal);
	if(id < this.firstVariable)
	    return true;
	return false;
    }

    /**
     *Checks if a variable is an S variable, given a literal.
     */

    public boolean isS(int literal){
	if(isX(literal))
	    return false;
	return true;
    }

    /**
     *Pretty print the variable in literal. 
     */
    public String prettyFormatVariable(int literal){
	int sign =(literal>0)? 1: -1;
	int id =  literal * sign;

	if(this.isY(id)){
	    int iObj = this.getIObjFromS(id);
	    int kD = this.getKDFromS(id);
	    int k = kD; // + this.instance.getObj(iObj).getMinValue();
	    return "Y[" + iObj + ", " + k +"]"+ "::" + literal + " ";
	}
	 
	if(isX(id)){
	    return (sign>0? "+":"-")+"X["+id+"] ";
	}
	/**
	 *Then, it is S!
	 */
	int iObj = this.getIObjFromS(id);
	int kD = this.getKDFromS(id);
	int k = kD ;// + this.instance.getObj(iObj).getMinValue();
	int name = this.getNameFromS(id);
	return "S[" + iObj +  ", " + name  +", "+ k +"]"+ "::" + literal + " ";
    }


    /**
     * Add the sequential clauses. This are the clauses of the form v1
     *=> v2,where v2 belongs to the same node and is associated to a
     *smaller value kD.
     */
    private boolean addClauseSequential(Node root){
	boolean change = false;
	boolean first = true;
	Node.NodeVars.NodeVar past;
	Collection<Node.NodeVars.NodeVar> tail =
	    root.nodeVars.currentTail().values();
	Iterator<Node.NodeVars.NodeVar> it = tail.iterator();
	if(it.hasNext()){
	    past = it.next();
	    for(Node.NodeVars.NodeVar current: tail ){
		if(first){
		    first = false;
		    past = current;
		}
		else{
		    IVecInt clause = new VecInt(new int[] {-current.id, past.id});
		    AddClause(clause);
		    change = true;
		    past = current;
		}
	    };
	    
	}
	
	return change;
    }

    /**
     * This adds the clause that makes this an GTE. That is, v1 v2 =>
     * v3, where kD of v3 is the (corrected) sum kD of v1 and v2
     */
    
    private boolean addSumClauses(Node parent, Node first, Node second){
	boolean change = false;
	Collection<Node.NodeVars.NodeVar> firstAll =
	    first.nodeVars.containerAll.values();

	Collection<Node.NodeVars.NodeVar> secondAll =
	    second.nodeVars.containerAll.values();

	for(Node.NodeVars.NodeVar firstVar : firstAll){
	    for(Node.NodeVars.NodeVar secondVar : secondAll ){
		Node.NodeVars.NodeVar parentVar =
		    parent.nodeVars.addParsimoneously(firstVar.kD + secondVar.kD);
		if(parentVar.getId() == null)
		    parentVar.setFreshId();
		if(parentVar.newVariable()) {
		    IVecInt clause = new VecInt(new int[] {-firstVar.id, -secondVar.id, parentVar.id});
		    AddClause(clause);
		    change = true;
		}
	    }
	}
	return change;
    }
    /**
     *Adds all clauses, respecting the current upperLimit, that complete the semantics of the GTE 
     */
    public boolean addClausesSumTree(int iObj){
	boolean change = false;
	SumTree ithObjSumTree = this.sumTrees[iObj];
	change = addClausesSubSumTree(ithObjSumTree, ithObjSumTree.parent) || change;
	change = addClauseSequential(ithObjSumTree.parent) || change;
	return change;
    }
    /**
     *Recursive helper of addClausesSumTree
     */

    public boolean addClausesSubSumTree(SumTree sumTree, Node currentNode){
	boolean change = false;
	Node left = currentNode.left;
	Node right = currentNode.right;
	if(currentNode.leafID != 0){
	    currentNode.activateLeafNode();
	}
	else{
	    change = addClausesSubSumTree(sumTree, left) || change;
	    change = addClausesSubSumTree(sumTree, right) || change;
	    change = addSumClauses(currentNode, left, right) || change;    
	    // change = addSumClauses(currentNode, right, left) || change;    
	}
	return change;
    }
    /**
     *Updates the semantics, in such a way that everything is valid
     *for any value kD less or equal to upperKD. Notice that it may
     *well be extend more, depending on the possible sums
     */

    public void UpdateCurrentK(int iObj, int upperKD){
	boolean change = false;
	this.sumTrees[iObj].setOlderUpperLimit();
	while(!change && upperKD < this.instance.getObj(iObj).getWeightDiff()){
	    this.sumTrees[iObj].setUpperLimit(upperKD);
	    change = addClausesSumTree(iObj);
	    upperKD++;
	}
    }

}
