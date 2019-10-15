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
import org.sat4j.core.Vec;
import org.sat4j.core.VecInt;
import org.sat4j.moco.util.Real;
import org.sat4j.moco.pb.PBFactory;
import org.sat4j.moco.pb.PBSolver;
import org.sat4j.moco.pb.ConstrID;
import org.sat4j.specs.ContradictionException;
import org.sat4j.specs.IVec;
import org.sat4j.specs.IVecInt;

/**
 * Class with the implementation of the sequetial encoder. 
 * @author Miguel Terra-Neves
 */

 public class SeqEncoder {

     /** 
      * IDs of the variables used to enforce the semantics of the sequential encoder
      */
     private Instance instance = null;
     private int[][][] idsS = null;
     // private ConstrID topConstraint = null;
     private PBSolver solver = null;
     private int[] currentKs = null;
     private int[][] idsB = null;
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
	this.currentKs = new int[this.instance.nObjs()];	
	this.ClausesIndependentOfK();

    }


     /**
      * Clause of type 4 in "On Using Incremental Encodings in..'"
      */
     private void ClausesIndependentOfK(){



	 for(int iObj = 0;iObj< this.instance.nObjs(); ++iObj){
	     Objective ithObj = this.instance.getObj(iObj);
	     int ithObjNLit = ithObj.getTotalLits();
	     ReadOnlyVecInt ithObjLits = ithObj.getSubObjLits(0);
	     ReadOnlyVec<Real> ithObjCoeffs = ithObj.getSubObjCoeffs(0);
	     assert ithObjNLit ==ithObjLits.size();

	     for (int iX = 0 ; iX < ithObjNLit-1; ++iX){
		 int ithXW = Math.round(ithObjCoeffs.get(iX).asInt());
		 for (int k  = 1;  k < ithXW ; ++k){

		 IVecInt clauseSet = new VecInt(2);
		 clauseSet.push(-ithObjLits.get(iX));
		 clauseSet.push(this.getS(iObj, iX, k));

		 try {
		     this.solver.addConstr(PBFactory.instance().mkClause(clauseSet));
		 }
		 catch (ContradictionException e) {
		 }
		 }
	     }
	     
	 }

}
     private void initializeIdsB(){
	this.idsB = new int[this.instance.nObjs()][];
	for(int iObj = 0;iObj< instance.nObjs(); ++iObj){
	    Objective ithObj = instance.getObj(iObj);
	    this.idsB[iObj] = new int[ithObj.getTotalWeight()];

	}
     }


     private void initializeIdsS(){
	this.idsS = new int[this.instance.nObjs()][][];
	for(int i = 0;i< instance.nObjs(); ++i){
	    Objective ithObj = instance.getObj(i);
	    this.idsS[i] = new int[ithObj.getTotalLits()][];
	    for(int k = 0; k < ithObj.getTotalLits();++k)
		this.idsS[i][k] = new int[ithObj.getTotalWeight()];
	}
     }

     public int getS(int iObj, int iX, int iK){
	 return	 this.idsS[iObj][iX][iK];
     }

     public void setS(int iObj, int iX, int iK, int id){
	 this.idsS[iObj][iX][iK] = id;

     }


     public int getB(int iObj, int iK){
	 return	 this.idsB[iObj][iK];
     }

     public void setB(int iObj, int iK, int id){
	 this.idsB[iObj][iK] = id;

     }

    /**
     *My little method. It should add the hard constraints needed for
     *the sequential encoding. I need to add the variables before I can 
     *@param iObj The objective index
     *@param kBefore The previous max value for the objective
     *@param kNow The desired max value for the objective 
     */

    
    public void SequentialEncoderUpdateK(int iObj , int afterK ){

	assert this.currentKs[iObj] < afterK;

	ExtendTopIdsSInK(iObj, afterK);
	ExtendTopIdsBInK(iObj, afterK);
	IfLessAlsoMore(iObj, afterK);
	IfLessAndIthXAtLeastIthW(iObj, afterK);
	blockingVariableB(iObj, afterK);

	IfLowNotX(iObj, afterK);

	this.currentKs[iObj] = afterK;

    }



     /**
      * Clause of type 11 in "On Using Incremental Encodings in..'"
      */
     private void blockingVariableB(int iObj,int afterK){
	 int beforeK = this.currentKs[iObj];
	     IVecInt clauseSet = new VecInt(1);
	 clauseSet.push(this.getB(iObj, beforeK));
	 try {
	     this.solver.addConstr(PBFactory.instance().mkClause(clauseSet));
	 }
	 catch (ContradictionException e) {
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

	 for (int iX = 1 ; iX < ithObjNLit-1; ++iX){
	     int ithXW = Math.round(ithObjCoeffs.get(iX).asInt());
		 IVecInt clauseSet = new VecInt(3);

		 clauseSet.push(this.getB(iObj, afterK));
		 clauseSet.push(-this.getS(iObj, iX-1, afterK + 1 - ithXW));
		 clauseSet.push(-ithObjLits.get(iX));
		 try {
		     this.solver.addConstr(PBFactory.instance().mkClause(clauseSet));
		 }
		 catch (ContradictionException e) {
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

	 for (int iX = 1 ; iX < ithObjNLit-1; ++iX){
	     int ithXW = Math.round(ithObjCoeffs.get(iX).asInt());
	     for (int k  = this.currentKs[iObj]- ithXW;  k <= afterK - ithXW; ++k){
		 IVecInt clauseSet = new VecInt(3);
		 clauseSet.push(-this.getS(iObj, iX-1, k));
		 clauseSet.push(-ithObjLits.get(iX));
		 clauseSet.push(this.getS(iObj, iX, k + ithXW));
		 try {
		     this.solver.addConstr(PBFactory.instance().mkClause(clauseSet));
		 }
		 catch (ContradictionException e) {
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
	     for (int k  = this.currentKs[iObj];  k < afterK; ++k){
		 IVecInt clauseSet = new VecInt(2);
		 clauseSet.push(-this.getS(iObj, iX-1, k));
		 clauseSet.push(this.getS(iObj, iX, k));
		 try {
		     this.solver.addConstr(PBFactory.instance().mkClause(clauseSet));
		 }
		 catch (ContradictionException e) {
		 }
	     }
	 }
     }


private void ExtendTopIdsBInK(int iObj, int afterK){

	     for(int k = this.currentKs[iObj]; k < afterK; ++k){
	     this.solver.newVars(1);
	     this.setB(iObj, k, this.solver.nVars());	     
	 }
	 
     }


     private void ExtendTopIdsSInK(int iObj, int afterK){
	 this.solver.newVars(1);
	 int nLit = this.instance.getObj(iObj).getTotalLits();
	 for (int iX = 0 ; iX < nLit-1; ++iX){
	     for(int k = this.currentKs[iObj]; k < afterK; ++k){
	     this.solver.newVars(1);
	     this.setS(iObj, iX, k, this.solver.nVars());	     
	     }
	 }
	 
     }
    

}
