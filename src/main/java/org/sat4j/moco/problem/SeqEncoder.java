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
      * Current top limits dK for all objective functions
      */
     private int[] currentKDs = null;

     private Instance instance = null;
     private PBSolver solver = null;

     /**
     * Creates an Instance of the sequential encoder
     * @param lits The literals in the constraint's left-hand side.
     * @param coeffs The coefficients in the constraint's left-hand side.
     * @param rhs The constraint's right-hand side.
     */


     public SeqEncoder(Instance instance, PBSolver solver) {

   	 this.instance = instance;	
	 this.solver = solver;

	 this.initializeIdsS();
	 this.initializeIdsB();

	 this.currentKDs = new int[this.instance.nObjs()];
	 for(int i = 0; i < this.instance.nObjs(); ++i){
	     this.currentKDs[i] = -1;
	 }

	 for(int i = 0; i < this.instance.nObjs(); ++i){
	 this.ExtendTopIdsBInK(i, 0);
	 this.ExtendTopIdsSInK(i, 0);
	 this.currentKDs[i]=0;
	 }

	 this.ClausesIndependentOfK();

    }


     
     private void initializeIdsB(){
	this.idsB = new int[this.instance.nObjs()][];
	for(int iObj = 0;iObj< instance.nObjs(); ++iObj){
	    Objective ithObj = instance.getObj(iObj);
	    this.idsB[iObj] = new int[ithObj.getWeightDiff()];

	}
     }

     /**
      * Initialize the container of the S variables
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
      * */

     public void setS(int iObj, int iX, int iKD, int id){
	 this.idsS[iObj][iX][iKD] = id;

     }

     /**
      * Get the ithObj, ith kD B variable
      * */

     public int getB(int iObj, int iKD){
	 return	 this.idsB[iObj][iKD];
     }



     /**
      * Set the ithObj, ith kD B variable to d
      * */

     public void setB(int iObj, int iKD, int id){
	 this.idsB[iObj][iKD] = id;

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

	assert this.currentKDs[iObj] < afterK;
	ExtendTopIdsSInK(iObj, afterK);
	ExtendTopIdsBInK(iObj, afterK);
	IfLessAlsoMore(iObj, afterK);
	IfLessAndIthXAtLeastIthW(iObj, afterK);
	blockingVariableB(iObj, afterK);
	IfLowNotX(iObj, afterK);
	this.currentKDs[iObj] = afterK;

    }


 
     /**
      * Add Clauses of type 4 in "On Using Incremental Encodings in..'"
      */
     private void ClausesIndependentOfK(){
	 for(int iObj = 0;iObj< this.instance.nObjs(); ++iObj){
	     Objective ithObj = this.instance.getObj(iObj);
	     ExtendTopIdsSInK(iObj, ithObj.getMaxCoeff());
	     int ithObjNLit = ithObj.getTotalLits();
	     ReadOnlyVecInt ithObjLits = ithObj.getSubObjLits(0);
	     ReadOnlyVec<Real> ithObjCoeffs = ithObj.getSubObjCoeffs(0);
	     //	     assert ithObjNLit ==ithObjLits.size();
	     for (int iX = 0 ; iX < ithObjNLit; ++iX){
		 int ithXW = Math.round(ithObjCoeffs.get(iX).asInt());
		 int sign = (ithXW > 0)? 1: -1;
		 ithXW = sign * ithXW;
		 int x =  sign * ithObjLits.get(iX);
		 for (int kD  = this.currentKDs[iObj];  kD < ithXW; ++kD){
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
	     for (int kD  = this.currentKDs[iObj];  kD < afterK; ++kD){
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
	     if(this.currentKDs[iObj]- ithXW >= 0)
	     for (int kD  = this.currentKDs[iObj]- ithXW;  kD <= afterK - ithXW ; ++kD){
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
	 int beforeK = this.currentKDs[iObj];
	     IVecInt clauseSet = new VecInt(1);
	 clauseSet.push(this.getB(iObj, beforeK));
	     this.AddClause(clauseSet);
     }


     /** 
      *  Extend the B variables in k
      */

private void ExtendTopIdsBInK(int iObj, int afterK){

	     for(int kD = this.currentKDs[iObj]+1; kD < afterK + 1; ++kD){
	     this.solver.newVars(1);
	     this.setB(iObj, kD, this.solver.nVars());	     
	 }
	 
     }


     /** 
      * Extend the S variables in k
      */


     private void ExtendTopIdsSInK(int iObj, int afterK){
	 
	 int nLit = this.instance.getObj(iObj).getTotalLits();
	 for (int iX = 0 ; iX < nLit; ++iX){
	     for(int kD = this.currentKDs[iObj]+1; kD < afterK +1 ; ++kD){
		 this.solver.newVars(1);
		 this.setS(iObj, iX, kD, this.solver.nVars());	     
	     }
	 }
	 
     }
}

