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

import java.lang.Math;
import java.util.Hashtable;

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

 public class SeqEncoder {

     /** 
      * IDs of the S(equential) variables used to enforce the semantics of the sequential encoder
      */
     private int[][][] idsS = null;

     /** 
      * IDs of the B(locking) variables used to allow incrementality
      */
     private int[][] idsB = null;
     // private ConstrID topConstraint = null;

     /** 
      * Current top differential k's for all objective functions
      */
     private int[] currentDKs = null;

     private Instance instance = null;
     private PBSolver solver = null;

     /**
      *The inverse index map for the S variables
      */
     private Hashtable<Integer,int[]> yVariablesInverseIndex  = new Hashtable<Integer, int[]>();

     /**
      *The inverse index map for the S variables
      */
     private Hashtable<Integer,int[]> sVariablesInverseIndex  = new Hashtable<Integer, int[]>();

     /**
      *The inverse index map for the blocking variables
      */
     private Hashtable<Integer, Integer> bVariablesInverseIndex  = new Hashtable<Integer, Integer>();

     /**
     * Creates an Instance of the sequential encoder
     * @param instance, the pseudo boolean instance
     * @param solver, the solver to be updated
     */

     public SeqEncoder(Instance instance, PBSolver solver) {

   	 this.instance = instance;	
	 this.solver = solver;

	 this.initializeIdsS();
	 this.initializeIdsB();

	 this.currentDKs = new int[this.instance.nObjs()];
	 for(int i = 0; i < this.instance.nObjs(); ++i){
	     this.currentDKs[i] = -1;
	 }

	 for(int i = 0; i < this.instance.nObjs(); ++i){
	 this.extendUpperIdsBInK(i, 0);
	 this.extendUpperIdsSInK(i, 0);
	 this.currentDKs[i]=0;
	 }

	 this.ClausesIndependentOfK();

    }



     /**
      * Initialize the container of the Blocking variables
      */
     
     private void initializeIdsB(){
	this.idsB = new int[this.instance.nObjs()][];
	for(int iObj = 0;iObj< instance.nObjs(); ++iObj){
	    Objective ithObj = instance.getObj(iObj);
	    this.idsB[iObj] = new int[ithObj.getWeightDiff()];

	}
     }

     /**
      * Initialize the container of the Sequential variables
      */

     private void initializeIdsS(){
	this.idsS = new int[this.instance.nObjs()][][];
	for(int i = 0;i< instance.nObjs(); ++i){
 	    Objective ithObj = instance.getObj(i);
	    this.idsS[i] = new int[ithObj.getTotalLits()][];
	    for(int kD = 0; kD < ithObj.getTotalLits();++kD){
		int[] array = new int[ithObj.getWeightDiff()];
		this.idsS[i][kD] = array;
	    }
	}
     }

     /**
      * Get the ithObj, ithX obj, kD S variable ID.
      */

     public int getS(int iObj, int iX, int kD){
	 return	 this.idsS[iObj][iX][kD];
     }
 

     /**
      * Set the ithObj, ith Literal ith kD S variable to
      *@param iObj, the index of the objective
      *@param iX, the index of the literal from the objective
      *@param iKD, the index of the current differential k
      *@param id, the id of the variable S created
      * */

     public void setS(int iObj, int iX, int iKD, int id){
	 this.idsS[iObj][iX][iKD] = id;
	 // this.sVariablesInverseIndex.put(this.solver.nVars(), new int[] {iObj,iK});
     }


     /**
      * Get the ithObj, ithX obj, kD Y variable ID.
      *@param iObj, the index of the objective
      *@param iX, the index of the literal from the objective
      *@param iKD, the index of the current differential k
      */

     public int getY(int iObj, int kD){
	 int nLits = this.instance.getObj(iObj).getTotalLits();
	 return	 this.idsS[iObj][nLits-1][kD];
     }

     /**
      * Set the ithObj,  ith kD Y variable to
      *@param iObj, the index of the objective
      *@param iKD, the index of the current differential k
      *@param id, the id of the variable Y
      * */

     public void setY(int iObj, int iKD, int id){
	 this.yVariablesInverseIndex.put(id, new int[] {iObj,iK});
     }

     /**
      * Set the ithObj,  ith kD upper differential k
      *@param iObj, the index of the objective
      *@param iKD, the index of the current differential k
      * */

     public void setCurrentUpperDK(int iObj, int kD){
	 this.currentDKs[iObj] = kD;
     }

     /**
      * Get the ithObj, ith differential k, Blocking  variable
      *@param iObj, the index of the objective
      *@param iKD, the index of the current differential k
      * */

     public int getB(int iObj, int iKD){
	 return	 this.idsB[iObj][iKD];
     }

     /**
      * Set the ithObj, ith kD id of a blocking variable to id
      *@param iObj, the index of the objective
      *@param iKD, the index of the current differential k
      *@param the new blocking variable id
      **/

     public void setB(int iObj, int iKD, int id){
	 this.idsB[iObj][iKD] = id;

     }


     /**
      * Get the ithObj current top dK
      * @param iObj, the objective function
      */

     public int getDK(int iObj){
	 return this.currentDKs[iObj];
     }




     private void AddClause(IVecInt setOfLiterals){
	 try{
	     this.solver.addConstr(PBFactory.instance().mkClause(setOfLiterals));
	 }
	 catch (ContradictionException e) {
	     System.out.println("contradiction when adding clause: ");
	     for(int j = 0; j < setOfLiterals.size(); ++j)
		 System.out.print(" " + setOfLiterals.get(j) + " " );
	     System.out.println();
	     return;
	 }
     }
    /**
     *My little method. It should add the hard constraints needed for
     *the sequential encoding. I need to add the variables before I can 
     *@param iObj The objective index
     *@param kBefore The previous max value for the objective
     *@param kNow The desired max value for the objective 
     */

    
    public void SequentialEncoderUpdateK(int iObj , int afterK ){

	assert this.currentDKs[iObj] < afterK;
	extendUpperIdsSInK(iObj, afterK);
	extendUpperIdsBInK(iObj, afterK);
	IfLessAlsoMore(iObj, afterK);
	IfLessAndIthXAtLeastIthW(iObj, afterK);
	blockingVariableB(iObj, afterK);
	IfLowNotX(iObj, afterK);
	this.currentDKs[iObj] = afterK;

    }

     /**
      * Generate the upper limit assumptions
      */
    public IVecInt generateUpperBoundAssumptions( ){
	IVecInt assumptions = new VecInt(new int[]{});
	for(int iObj = 0; iObj < instance.nObjs(); ++iObj){
	    assumptions.push(-this.getY(iObj, this.currentDKs[iObj] + 1));
	}
	return assumptions;
    }
     
     /**
      * Add Clauses of type 4 in "On Using Incremental Encodings in..'"
      */
     private void ClausesIndependentOfK(){
	 for(int iObj = 0;iObj< this.instance.nObjs(); ++iObj){
	     Objective ithObj = this.instance.getObj(iObj);
	     extendUpperIdsSInK(iObj, ithObj.getMaxCoeff());
	     int ithObjNLit = ithObj.getTotalLits();
	     ReadOnlyVecInt ithObjLits = ithObj.getSubObjLits(0);
	     ReadOnlyVec<Real> ithObjCoeffs = ithObj.getSubObjCoeffs(0);
	     //	     assert ithObjNLit ==ithObjLits.size();
	     for (int iX = 0 ; iX < ithObjNLit; ++iX){
		 int ithXW = Math.round(ithObjCoeffs.get(iX).asInt());
		 int sign = (ithXW > 0)? 1: -1;
		 ithXW = sign * ithXW;
		 int x =  sign * ithObjLits.get(iX);
		 for (int kD  = this.currentDKs[iObj];  kD < ithXW; ++kD){
		     int s = this.getS(iObj, iX, kD);
		     IVecInt clauseSet = new VecInt(2);
		     clauseSet.push(-x);
		     clauseSet.push(s);
		     this.AddClause(clauseSet);
		 }
	     }
	 }
	 
     }



     /**
      * Clause of type 8 in "On Using Incremental Encodings in..'"
      */
     private void IfLessAlsoMore(int iObj,int afterK){

	int nLit = this.instance.getObj(iObj).getTotalLits();
	 for (int iX = 1 ; iX < nLit-1; ++iX){
	     for (int kD  = this.currentDKs[iObj];  kD < afterK; ++kD){
		 IVecInt clauseSet = new VecInt(2);
		 int s1 = this.getS(iObj, iX-1, kD);
		 int s2 = this.getS(iObj, iX, kD);
		 clauseSet.push(-s1);
		 clauseSet.push(s2);
		 this.AddClause(clauseSet);
	     }
	 }
     }

     /**
      * Clause of type 9 in "On Using Incremental Encodings in..'"
      */
     private void IfLessAndIthXAtLeastIthW(int iObj,int afterK){

	 int ithObjNLit = this.instance.getObj(iObj).getTotalLits();
	 ReadOnlyVecInt ithObjLits = this.instance.getObj(iObj).getSubObjLits(0);
	 ReadOnlyVec<Real> ithObjCoeffs = this.instance.getObj(iObj).getSubObjCoeffs(0);
	 assert ithObjNLit ==ithObjLits.size();

	 for (int iX = 1 ; iX < ithObjNLit; ++iX){
	     int ithXW = ithObjCoeffs.get(iX).asInt();
	     int sign = (ithXW > 0)? 1: -1;
	     ithXW = sign * ithXW;
	     int x = sign * ithObjLits.get(iX);
	     if(this.currentDKs[iObj]- ithXW >= 0)
	     for (int kD  = this.currentDKs[iObj]- ithXW;  kD <= afterK - ithXW ; ++kD){
		 IVecInt clauseSet = new VecInt(3);
		 int s1 = -this.getS(iObj, iX-1, kD);
		 int s2 = this.getS(iObj, iX, kD + ithXW);
		 clauseSet.push(s1);
		 clauseSet.push(x);
		 clauseSet.push(s2);
		 this.AddClause(clauseSet);
	     }
	 }
     }



     /**
      * Clause of type 10 in "On Using Incremental Encodings in..'"
      */
     private void IfLowNotX(int iObj,int afterK){

	 int ithObjNLit = this.instance.getObj(iObj).getTotalLits();
	 ReadOnlyVecInt ithObjLits = this.instance.getObj(iObj).getSubObjLits(0);
	 ReadOnlyVec<Real> ithObjCoeffs = this.instance.getObj(iObj).getSubObjCoeffs(0);
	 assert ithObjNLit ==ithObjLits.size();

	 for (int iX = 1 ; iX < ithObjNLit; ++iX){
	     int ithXW = ithObjCoeffs.get(iX).asInt();
	     int sign = (ithXW > 0)? 1: -1;
	     ithXW = sign * ithXW;
	     if( afterK + 1 - ithXW > 0){
	     IVecInt clauseSet = new VecInt(3);
	     int b = this.getB(iObj, afterK);
	     int s = this.getS(iObj, iX-1, afterK + 1 - ithXW);
	     int x = ithObjLits.get(iX);
	     clauseSet.push(b);
	     clauseSet.push(-s);
	     clauseSet.push(- sign * x);
	     this.AddClause(clauseSet);
	     }
	 }
     }

     /**
      * Clause of type 11 in "On Using Incremental Encodings in..'"
      */
     private void blockingVariableB(int iObj,int afterK){
	 int beforeK = this.currentDKs[iObj];
	     IVecInt clauseSet = new VecInt(1);
	 clauseSet.push(this.getB(iObj, beforeK));
	     this.AddClause(clauseSet);
     }


     private int newVar(){
	     this.solver.newVars(1);
	     return solver.nVars();
     }


     /** 
      *  extend the B variables in k
      */

     private void extendUpperIdsBInK(int iObj, int afterK){

	     for(int kD = this.currentDKs[iObj]+1; kD < afterK + 1; ++kD){
	     this.solver.newVars(1);
	     this.setB(iObj, kD, this.solver.nVars());	     
	 }
	 
     }

     /** 
      * extend the S variables in the differential k index
      */

     public void extendUpperIdsSInK(int iObj, int afterDK){
	 
	 int nLit = this.instance.getObj(iObj).getTotalLits();
	 for (int iX = 0 ; iX < nLit; ++iX){
	     for(int dK = this.currentDKs[iObj]+1; dK < afterDK +1 ; ++dK){
		 this.setS(iObj, iX, dK, this.newVar());
	     }
	 }
	 extendUpperIdsYinK(iObj, afterDK);	 
}


     /** 
      * extend the S variables in the differential k index
      */

private void extendUpperIdsYinK(int iObj, int afterDK){
	 int nLit = this.instance.getObj(iObj).getTotalLits();
	     for(int dK = this.currentDKs[iObj]+1; dK < afterDK +1 ; ++dK){
		 this.setY(iObj, dK, this.getS(iObj, nLit, afterDK ));
	 }
     }



     /**
      * Get the objective from an S variable
      * @param literal
      */

     public int getObjFromYVariable(int literal){
	 assert this.isY(literal);
	 return this.yVariablesInverseIndex.get(literal)[0] ;
     }

     /**
      * return the value of the differential k from an S variable
      * @param literal
      */

     public int getDKfromYVariable(int literal){
	 assert this.isY(literal);
	 return this.yVariablesInverseIndex.get(literal)[1] ;
     }


     /**
      *Checks if literal is true
      *@param literal
      */
     private boolean literalIsTrue(int i){
	 if(i > 0)
	     return true;
	 return false;
     }

     /**
      *Checks if literal is an S variable
      *@param literal
      */

     public boolean isY(int literal){
	 if(this.yVariablesInverseIndex.containsKey(literal) || this.yVariablesInverseIndex.containsKey(-literal))
	     return true;
	 return false;
     }

     public void blockDominatedRegion(IVecInt newSolution){
	 IVecInt newHardClause = new VecInt(new int[] {});
	 for(int i = 0; i < newSolution.size(); ++i){
	     int literal = newSolution.get(i);
	     if(this.isY(literal) && this.literalIsTrue(literal))
		 newHardClause.push(literal);
	 }
	 this.AddClause(newHardClause);
     }

}

