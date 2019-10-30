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
import java.util.Set;

import org.sat4j.core.ReadOnlyVec;
import org.sat4j.core.ReadOnlyVecInt;
import org.sat4j.core.VecInt;
import org.sat4j.moco.util.Real;
import org.sat4j.moco.pb.ConstrID;
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
     * Current top initialized differential k's for all objective functions
     */

    private int[] currentDKs = null;
    /** 
     * Current top differential k's for all objective functions
     */
    private int[] initializedDKs = null;

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

    private Hashtable<ConstrID, Integer> bVariablesInverseIndex  = new Hashtable<Integer, int[]>();

    /**
     *First solver variable that pertains to the sequential encoding
     */

    private int firstVariable = 0;

    /**
     * Creates an Instance of the sequential encoder
     * @param instance, the pseudo boolean instance
     * @param solver, the solver to be updated
     */

    public SeqEncoder(Instance instance, PBSolver solver) {

	this.instance = instance;	
	this.solver = solver;
	this.firstVariable = solver.nVars() + 1;
	this.initializeIdsS();
	this.initializeIdsB();
	this.initializedDKs = new int[this.instance.nObjs()];
	this.currentDKs = new int[this.instance.nObjs()];
	for(int i = 0; i < this.instance.nObjs(); ++i){
	    this.initializedDKs[i] = -1;
	}

	for(int iObj = 0; iObj < this.instance.nObjs(); ++iObj){
	    this.UpdateCurrentK(iObj, 0);	 
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
	    this.idsB[iObj] = new int[ithObj.getWeightDiff() + 1];
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
		// + 1 necessary: remember dK is simultaneously a
		// value and an index
		int[] array = new int[ithObj.getWeightDiff() + 1];
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
     *@param iDK, the index of the current differential k
     *@param id, the id of the variable S created
     * */

    public void setS(int iObj, int iX, int iDK, int id){
	this.idsS[iObj][iX][iDK] = id;
	this.sVariablesInverseIndex.put(id, new int[] {iObj,iX,iDK});
    }


    /**
     * Get the ithObj, ithX obj, kD Y variable ID.
     *@param iObj, the index of the objective
     *@param iX, the index of the literal from the objective
     *@param iDK, the index of the current differential k
     */

    public int getY(int iObj, int kD){
	int nLits = this.instance.getObj(iObj).getTotalLits();
	return	 this.idsS[iObj][nLits-1][kD];
    }

    /**
     * Set the ithObj,  ith kD Y variable to
     *@param iObj, the index of the objective
     *@param iDK, the index of the current differential k
     *@param id, the id of the variable Y
     * */

    public void setY(int iObj, int iDK, int id){
	this.yVariablesInverseIndex.put(id, new int[] {iObj,iDK});
    }


    /**
     * Get the ithObj current upper limit on the initialized differential k
     *@param iObj, the index of the objective
     *@return  ithObj 
     */

    public int getInitializedDK(int iObj){
	return	 this.initializedDKs[iObj];
    }

    /**
     * Set the ithObj,  ith kD upper differential k
     *@param iObj, the index of the objective
     *@param iDK, the index of the current differential k
     * */

    public void setInitializedDK(int iObj, int kD){
	this.initializedDKs[iObj] = kD;
    }

    /**
     * Get the ithObj current upper limit on the initialized differential k
     *@param iObj, the index of the objective
     *@return  ithObj 
     */

    public int getCurrentDK(int iObj){
	return	 this.currentDKs[iObj];
    }

    /**
     * Set the ithObj,  ith kD upper differential k
     *@param iObj, the index of the objective
     *@param iDK, the index of the current differential k
     * */

    public void setCurrentDK(int iObj, int kD){
	this.currentDKs[iObj] = kD;
    }

    /**
     * Get the ithObj, ith differential k, Blocking  variable
     *@param iObj, the index of the objective
     *@param iDK, the index of the current differential k
     * */

    public int getB(int iObj, int iDK){
	return	 this.idsB[iObj][iDK];
    }

    /**
     * Set the ithObj, ith kD id of a blocking variable to id
     *@param iObj, the index of the objective
     *@param iDK, the index of the current differential k
     *@param the new blocking variable id
     **/

    public void setB(int iObj, int id){
	this.idsB[iObj][dK] = id;
	this.bVariablesInverseIndex.put(id, new int[] {iObj});
    }


    /**
     *Adds the conjunctio of setOfLiterals
     *@param setOfliterals
     */

    private void AddClause(IVecInt setOfLiterals){
	for(int i = 0; i < setOfLiterals.size(); ++i)
	    this.prettyPrintVariable(setOfLiterals.get(i));
	System.out.println();
	try{
	    this.solver.addConstr(PBFactory.instance().mkClause(setOfLiterals));
	} catch (ContradictionException e) {
	    System.out.println("contradiction when adding clause: ");
	    for(int j = 0; j < setOfLiterals.size(); ++j)
		System.out.print(" " + setOfLiterals.get(j) + " " );
	    System.out.println();
	    return;
	}
    }

    /**
     *Adds the conjunctio of setOfLiterals
     *@param setOfliterals
     */

    private ConstrID AddRemovableClause(IVecInt setOfLiterals){

	ConstrID constrainId = null;

	for(int i = 0; i < setOfLiterals.size(); ++i)
	    this.prettyPrintVariable(setOfLiterals.get(i));
	System.out.println();
	try{
	    constrainId = this.solver.addRemovableConstr(PBFactory.instance().mkClause(setOfLiterals));
	} catch (ContradictionException e) {
	    System.out.println("contradiction when adding clause: ");
	    for(int j = 0; j < setOfLiterals.size(); ++j)
		System.out.print(" " + setOfLiterals.get(j) + " " );
	    System.out.println();
	    return constrainId;
	}
	return null;
    }

    /**
     *My little method. It should add the hard constraints needed for
     *the sequential encoding. I need to add the variables before I can 
     *@param iObj The objective index
     *@param kBefore The previous max value for the objective
     *@param kNow The desired max value for the objective 
     */
    
    public void UpdateCurrentK(int iObj , int afterDK ){

	if(this.getInitializedDK(iObj)< afterDK ){
	    // Y variables are also extended at 
	    this.extendInitializedIdsSInK(iObj, afterDK); 
	    this.setInitializedDK(iObj, afterDK);
	}
	 
	if(this.getCurrentDK(iObj) < afterDK){
	    this.IfLessAlsoMore(iObj, afterDK);
	    this.IfLessAndIthXAtLeastIthW(iObj, afterDK);
	    this.IfLowNotX(iObj, afterDK);
	    this.blockingVariableB(iObj, afterDK);
	    this.setCurrentDK(iObj, afterDK);
	}
    }

     
    /**
     * Add Clauses of type 4 in "On Using Incremental Encodings in..'"
     */
    private void ClausesIndependentOfK(){

	for(int iObj = 0;iObj< this.instance.nObjs(); ++iObj){
	    Objective ithObj = this.instance.getObj(iObj);
	    this.extendInitializedIdsSInK(iObj, ithObj.getMaxAbsCoeff());
	    int ithObjNLit = ithObj.getTotalLits();
	    ReadOnlyVecInt ithObjLits = ithObj.getSubObjLits(0);
	    ReadOnlyVec<Real> ithObjCoeffs = ithObj.getSubObjCoeffs(0);
	    //	     assert ithObjNLit ==ithObjLits.size();
	    for (int iX = 0 ; iX < ithObjNLit-1; ++iX){
		int ithXW = Math.round(ithObjCoeffs.get(iX).asInt());
		int sign = (ithXW > 0)? 1: -1;
		ithXW = sign * ithXW;
		int x =  sign * ithObjLits.get(iX);
		for (int kD  = this.getCurrentDK(iObj) ;  kD <= ithXW; ++kD){
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

	for (int iX = 1 ; iX < ithObjNLit - 1; ++iX){
	    int ithXW = ithObjCoeffs.get(iX).asInt();
	    int sign = (ithXW > 0)? 1: -1;
	    ithXW = sign * ithXW;
	    int x = sign * ithObjLits.get(iX);
	    int lowerLimit = this.currentDKs[iObj]- ithXW + 1;
	    lowerLimit = (lowerLimit >= 0)? lowerLimit: 0;
	    int upperLimit = afterK - ithXW ;
	    upperLimit = (upperLimit >= 0)? upperLimit: 0;
	    for (int kD  = lowerLimit;  kD <= upperLimit ; ++kD){
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
	assert ithObjNLit == ithObjLits.size();

	for (int iX = 1 ; iX < ithObjNLit ; ++iX){
	    int ithXW = ithObjCoeffs.get(iX).asInt();
	    int sign = (ithXW > 0)? 1: -1;
	    ithXW = sign * ithXW;
	    int kIndex = afterK + 1 - ithXW;
	    kIndex = (kIndex > 0)? kIndex: 0;
	    if( afterK + 1 - ithXW > 0){
		IVecInt clauseSet = new VecInt(2);
		int s = this.getS(iObj, iX-1, kIndex);
		int x = ithObjLits.get(iX);
		clauseSet.push(-s);
		clauseSet.push(- sign * x);
		this.setB(iObj, this.AddRemovableClause(clauseSet));
	    }
	}
    }

    /**
     * Clause of type 11 in "On Using Incremental Encodings in..'"
     */
    private void blockingVariableB(int iObj,int afterK){
	int beforeK = this.currentDKs[iObj];
	IVecInt clauseSet = new VecInt(1);
	this.solver.removeConstr(this.idsB);
    }

    private int newVar(){
	this.solver.newVars(1);
	return solver.nVars();
    }


    /** 
     *  extend the B variables in k
     */

    private void extendInitializedIdsBInK(int iObj, int afterK){

	for(int kD = this.getInitializedDK(iObj)+1; kD < afterK + 1; ++kD){
	    this.solver.newVars(1);
	    this.setB(iObj, kD, this.solver.nVars());	     
	}
	 
    }

    /** 
     * extend the S variables in the differential k index
     */

    public void extendInitializedIdsSInK(int iObj, int afterDK){

	int nLit = this.instance.getObj(iObj).getTotalLits();
	for(int dK = this.initializedDKs[iObj]+1; dK < afterDK +1 ; ++dK){
	    for (int iX = 0 ; iX < nLit; ++iX){
		/* System.out.println(iObj + " " + iX + " " + dK + " " + this.newVar()); */
		this.setS(iObj, iX, dK, this.newVar());
	    }
	}
	this.extendInitializedIdsYinK(iObj, afterDK);
    }


    /** 
     * extend the Y variables in the differential k index until afterDK. Assumes
     * the S variables are already extended accordingly
     * @param iObj
     * @param afterDK
     */

    private void extendInitializedIdsYinK(int iObj, int afterDK){
	int nLit = this.instance.getObj(iObj).getTotalLits();
	for(int dK = this.initializedDKs[iObj]+1; dK < afterDK +1 ; ++dK){
	    this.setY(iObj, dK, this.getS(iObj, nLit-1, afterDK ));
	}
    }



    /**
     * Get the objective from an Y variable
     * @param literal
     */

    public int getObjFromYVariable(int literal){
	assert this.isY(literal);
	literal = (literal>0)? literal: -literal;
	return this.yVariablesInverseIndex.get(literal)[0] ;
    }

    /**
     * return the value of the differential k from an S variable
     * @param literal
     */

    public int getDKFromYVariable(int literal){
	assert this.isY(literal);
	literal = (literal>0)? literal: -literal;
	return this.yVariablesInverseIndex.get(literal)[1] ;
    }

    /**
     * Get the objective from an Y variable
     * @param literal
     */

    public int getObjFromSVariable(int literal){
	assert this.isS(literal);
	literal = (literal>0)? literal: -literal;
	return this.sVariablesInverseIndex.get(literal)[0] ;
    }

    /**
     * return the value of the differential k from an S variable
     * @param literal
     */

    public int getDKFromSVariable(int literal){
	assert this.isS(literal);
	literal = (literal>0)? literal: -literal;
	return this.sVariablesInverseIndex.get(literal)[2] ;

    }
    /**
     * Get the objective from an B variable
     * @param literal
     */

    public int getObjFromBVariable(int literal){
	assert this.isB(literal);
	literal = (literal>0)? literal: -literal;
	return this.bVariablesInverseIndex.get(literal)[0] ;
    }

    /**
     * return the value of the differential k from an B variable
     * @param literal
     */

    public int getDKFromBVariable(int literal){
	assert this.isS(literal);
	literal = (literal>0)? literal: -literal;
	return this.bVariablesInverseIndex.get(literal)[1] ;

    }

    /**
     * return the value of the literal index
     * @param literal
     */

    public int getXFromSVariable(int literal){
	assert this.isS(literal);
	literal = (literal>0)? literal: -literal;
	return this.sVariablesInverseIndex.get(literal)[1] ;
    }


    /**
     *Checks if literal is an Y variable
     *@param literal
     */

    public boolean isY(int literal){
	literal = (literal>0)? literal: -literal;
	if(this.yVariablesInverseIndex.containsKey(literal))
	    return true;
	return false;
    }

    /**
     *Checks if literal is an S variable
     *@param literal
     */

    public boolean isS(int literal){
	literal = (literal>0)? literal: -literal;
	if(this.sVariablesInverseIndex.containsKey(literal))
	    return true;
	return false;
    }
    /**
     *Checks if literal is an B variable
     *@param literal
     */

    public boolean isB(int literal){
	literal = (literal>0)? literal: -literal;
	if(this.bVariablesInverseIndex.containsKey(literal))
	    return true;
	return false;
    }

    public void prettyPrintVariable(int literal){
	int sign =(literal>0)? 1: -1;
	int id =  literal * sign;
	if(this.isY(id)){
	    int iObj = this.getObjFromSVariable(id);
	    int iX = this.getXFromSVariable(id);
	    int dK = this.getDKFromSVariable(id);
	    System.out.print(literal + "->" + "Y[" + iObj + ", " + iX + ", " + dK +"] ");
	    return;
	}
	 
	if(this.isS(id)){
	    int iObj = this.getObjFromSVariable(id);
	    int iX = this.getXFromSVariable(id);
	    int dK = this.getDKFromSVariable(id);
	    System.out.print(literal + "->" + "S[" + iObj + ", " + iX + ", " + dK +"] ");
	    return;
	}
	if(this.isB(id)){
	    int iObj = this.getObjFromBVariable(id);
	    int dK = this.getDKFromBVariable(id);
	    System.out.print(id + "->" + "B[" + iObj + ", " + dK +"] ");
	    return;
	}
	if(id < this.firstVariable){
	    System.out.print(sign+"X["+id+"] ");
	}
    }

    public void printS(){
	for(int i = 0; i < this.sVariablesInverseIndex.size(); ++i){
	    Set<Integer> keys = this.sVariablesInverseIndex.keySet();
	    for(Integer IntegerKey: keys){
		int key = IntegerKey;
		int iObj = this.getObjFromSVariable(key);
		int iX = this.getXFromSVariable(key);
		int dK = this.getDKFromSVariable(key);
		System.out.println(key + "->" + "S[" + iObj + ", " + iX + ", " + dK +"]");
	    }
	}
    }
}

