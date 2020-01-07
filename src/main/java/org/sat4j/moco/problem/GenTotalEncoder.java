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
import java.util.HashSet;
import java.util.Hashtable;
import java.util.PriorityQueue;
import java.util.ArrayList;
import java.util.Set;
import org.sat4j.core.ReadOnlyVec;
import org.sat4j.core.ReadOnlyVecInt;
import org.sat4j.core.VecInt;
import org.sat4j.moco.util.Real;
import org.sat4j.moco.pb.PBFactory;
import org.sat4j.moco.pb.PBSolver;
import org.sat4j.specs.IVecInt;
import org.sat4j.specs.ContradictionException;

/**
 * Class with the implementation of the sequetial encoder.
 * Notice that the differential is both a value and an index starting at 0
 * @author Joao O'Neill Cortes
 */


 public class GenTotalEncoder {




     class SumTree {

	 private int iObj = 0;
	 private Node parent = null ;
	 private ArrayList<Node> nodes = new ArrayList<Node>();
	 private PriorityQueue<Node> unlinkedNodes = new PriorityQueue<Node>((a,b) -> a.nodeSum - b.nodeSum);

	 class Node {
	     private HashMap<Integer, Integer> nodeVars = null;
	     private int nodeSum = 0;
	     private Node left = null;
	     private Node right = null;
	     
	     public Node(int weight){
		 this.nodeSum = weight;
		 this.left = null; 
		 this.right = this.left;
		 this.nodeVars = new HashMap<Integer, Integer>();
		 int id = GenTotalEncoder.this.newSVar(this.nodeSum);
		 this.nodeVars.put(this.nodeSum, id);
		 SumTree.this.unlinkedNodes.add(this);
	     }
	     
	     public Node(Node left, Node right){
		 SumTree.this.unlinkedNodes.add(this);
		 this.left = left;
		 this.right = right;
		 this.nodeSum = left.nodeSum + right.nodeSum;
		 this.nodeVars =  new HashMap<Integer, Integer>();
		 HashSet<Integer> values = new HashSet<Integer>() ;
		 for(Integer valueLeft : left.nodeVars.values())
		     for(Integer valueRight : right.nodeVars.values())
			 values.add(valueLeft + valueRight);
		 for(int value : values) {
		     int id = GenTotalEncoder.this.newSVar(value);    
		     this.nodeVars.put(value, id);
		 }
	     }
	 }

	 
	 public SumTree(Node node){
	     this.nodes.add(node);
	     this.parent = node;
	 }

	 public SumTree(SumTree treeLeft, SumTree treeRight){
	     this.parent = new Node(treeLeft.parent, treeRight.parent);
	     this.nodes.add(parent);
	 }
	 public boolean linkTree(){
	     while(!this.unlinkedNodes.isEmpty()){
		 Node leftNode = unlinkedNodes.poll();
		 Node rightNode = unlinkedNodes.poll();
		 Node parentNode = new Node(leftNode, rightNode);
		 unlinkedNodes.add(parentNode);
	     }
	     return true;
	 }
     }

     /**
      *Tree used to encode the goal limits
      */
     private SumTree sumTree = null;
     
     /**
      *current upper limits, for each goal
      */
     private int[] currentKDs = null;

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
     * Creates an Instance of the sequential encoder
     * @param instance, the pseudo boolean instance
     * @param solver, the solver to be updated
     */

    public GenTotalEncoder(Instance instance, PBSolver solver) {
	Log.comment(5, "in GenTotalEncoder");
	this.instance = instance;	
	this.solver = solver;
	this.firstVariable = this.solver.nVars() + 1;
	this.currentKDs = new int[this.instance.nObjs()];
	for(int iObj = 0;iObj< instance.nObjs(); ++iObj){
	    this.setInitializedKD(iObj,-1);
	    this.UpdateCurrentK(iObj,0);
	}
	Log.comment(5, "done");
    }
     private void setInitializedKD(int iObj, int kD){};
     private void UpdateCurrentK(int iObj, int kD){};

     /**
      *Return the ID of a freshly solver's variable
      */
     public int newSVar(int sum){
	 
	 this.solver.newVar();
	 int id = this.solver.nVars();
	 this.sVariablesInverseIndex.put(id, new int[]{sum, this.sumTree.iObj});
	 return id;
     }
 }



