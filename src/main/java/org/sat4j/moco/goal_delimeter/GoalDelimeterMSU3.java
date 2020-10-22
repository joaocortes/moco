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
import org.sat4j.moco.util.Log;
import java.util.Hashtable;
import java.util.Iterator;

import java.util.PriorityQueue;
import java.util.ArrayList;
import java.util.TreeMap;
import java.util.Collection;
import java.util.HashMap;

import org.sat4j.core.ReadOnlyVec;

import org.sat4j.core.ReadOnlyVecInt;
import org.sat4j.core.VecInt;
import org.sat4j.moco.util.Real;
import org.sat4j.moco.pb.PBSolver;
import org.sat4j.moco.problem.Instance;
import org.sat4j.moco.problem.Objective;
import org.sat4j.moco.problem.GenTotalEncoder.SumTree;
import org.sat4j.moco.problem.GenTotalEncoderMSU3.SumTree.Node;
import org.sat4j.specs.IVecInt;


/**
 * Interface that contains MSU3 like abilities for GoalDelimeters. 
 * @author Joao O'Neill Cortes
 */

public abstract class GoalDelimeterMSU3<PIndex extends Index> extends GoalDelimeter<PIndex>{
    boolean change = false;


    /**
     *signals that the MSU3 flavour is active
     */
    protected boolean MSU3 = false;

    /**
     * Upper bound, exclusive
     */
    private int[] UpperBound = null;

    private HashMap<Integer, Boolean> coveredLiterals = null;

    public GoalDelimeterMSU3(Instance instance, PBSolver solver, boolean MSU3){
	super(instance, solver);
	this.MSU3 = MSU3;
	this.UpperBound = new int[this.instance.nObjs()];
	if(MSU3){
	    this.coveredLiterals = new HashMap<Integer, Boolean>(this.solver.nVars());
	    for(int iObj = 0, nObj = this.instance.nObjs();iObj < nObj; iObj++){
		Objective ithObjective = this.instance.getObj(iObj);
		ReadOnlyVecInt objectiveLits = ithObjective.getSubObjLits(0);
		ReadOnlyVec<Real> objectiveCoeffs = ithObjective.getSubObjCoeffs(0);
		int sign = 1;
		int ithAbsoluteWeight;
		for(int iX = 0, nX = ithObjective.getTotalLits(); iX <nX; iX ++){
		    int ithX = objectiveLits.get(iX);
		    ithAbsoluteWeight = objectiveCoeffs.get(iX).asInt();
		    sign = (ithAbsoluteWeight > 0? 1 : -1);
		    ithAbsoluteWeight *= sign;
		    this.coveredLiterals.putIfAbsent(-sign * ithX, true);
		}
	    }
	}else
	    this.coveredLiterals = new HashMap<Integer, Boolean>(0);

	 

    }

    protected int getUpperBound(int iObj){
	return this.UpperBound[iObj];
    }

     boolean uncoverXs(IVecInt explanationX) {
	// Log.comment(5, "{ UnsatSatMSU3.uncoverXs");
	for(int i = 0, n = explanationX.size(); i< n; i++)
	    this.coveredLiterals.remove(explanationX.get(i));
	this.updateAllUncoveredMaxKD();
	this.logUncoveredMaxKD();
	// Log.comment(5, "}");
	return change;
    }

    //TODO: this must be put inside a goalManager. 
	public void updateUncoveredMaxKD(int iObj){
	    int a = 0;
	    Objective ithObjective = instance.getObj(iObj); // 
	    ReadOnlyVecInt objectiveLits = ithObjective.getSubObjLits(0);
	    ReadOnlyVec<Real> objectiveCoeffs = ithObjective.getSubObjCoeffs(0);
	    int sign = 1;
	    int ithAbsoluteWeight;
	    for(int iX = 0, nX = ithObjective.getTotalLits(); iX < nX; iX ++){
		int ithX = objectiveLits.get(iX);
		ithAbsoluteWeight = objectiveCoeffs.get(iX).asInt();
		sign = (ithAbsoluteWeight > 0? 1 : -1);
		ithAbsoluteWeight *= sign;
		if(coveredLiterals.get(-sign * ithX) == null)
		    a += ithAbsoluteWeight;
	    }
	    this.setMaxUncoveredKD(iObj, a);
	}
    public void updateAllUncoveredMaxKD(){
	for(int i = 0, n = this.getInstance().nObjs(); i < n; i++)
	    this.updateUncoveredMaxKD(i);

};

    public void logUncoveredMaxKD(){
	String logUpperLimit = "uncovered max: ["+this.getUncoveredMaxKD(0);
	for(int iObj = 1; iObj < this.instance.nObjs(); ++iObj)
	    logUpperLimit +=", "+this.getUncoveredMaxKD(iObj) ;//+ this.instance.getObj(iObj).getMinValue())
	//..log
	
	logUpperLimit +="]" + ", covered literals: " + this.coveredLiterals.size();
	Log.comment(2, logUpperLimit );
    }

    abstract public int getUncoveredMaxKD(int iObj);
    abstract public void setMaxUncoveredKD(int iObj, int a);

    public IVecInt generateUpperBoundAssumptions(IVecInt explanation, boolean checkChange){
	if(!this.preAssumptionsExtend(explanation) & checkChange)
	    return null;
	
	IVecInt assumptions = new VecInt(new int[]{});
	
	for(int iObj = 0; iObj < this.instance.nObjs(); ++iObj){
	    int IthUpperBound = this.nextKDValue(iObj, getUpperKD(iObj));
	    Objective ithObjective = this.instance.getObj(iObj);
	    if(this.getUpperKD(iObj)  != IthUpperBound){
		int newY = -this.getY(iObj, IthUpperBound);
		if(newY!=0)
		    assumptions.push(newY);
	    }

	    ReadOnlyVecInt objectiveLits = ithObjective.getSubObjLits(0);
	    ReadOnlyVec<Real> objectiveCoeffs = ithObjective.getSubObjCoeffs(0);
	    int sign;
	    int ithAbsoluteWeight;

	    for(int iX = 0, nX = ithObjective.getTotalLits(); iX <nX; iX ++){
		int ithX = objectiveLits.get(iX);
		ithAbsoluteWeight = objectiveCoeffs.get(iX).asInt();
		sign = (ithAbsoluteWeight > 0? 1 : -1);
		ithAbsoluteWeight *= sign;
		if(ithAbsoluteWeight > getUpperKD(iObj))
		    if(this.coveredLiterals.get(-sign * ithX) == null)
			assumptions.push(-sign * ithX);
	    }

	}


	if(this.MSU3)
	    for(Integer x: this.coveredLiterals.keySet())
		assumptions.push(x);
	
	return assumptions;
    }
    /**
     *If necessary for the construction of the current assumptions,
     *initialize more of the domain of the goal delimeter
     */

    public boolean preAssumptionsExtend(IVecInt currentExplanation){
	// Log.comment(2, "explanation: ");
	// this.prettyPrintVecInt(currentExplanation);
	boolean change = false;
	// Log.comment(0, "covered x variables: " + this.coveredLiterals.size());
	IVecInt currentExplanationX = new VecInt(new int[] {});
	HashMap<Integer,Boolean> objectivesToChange = new HashMap<Integer, Boolean>(this.instance.nObjs());
	for(int lit: currentExplanation.toArray()){
	    int id = this.solver.idFromLiteral(lit);
	    if(this.isX(id)){
		currentExplanationX.push(lit);
		for(int iObj = 0; iObj < this.instance.nObjs(); ++iObj){
		    if(this.instance.getObj(iObj).getSubObj(0).weightFromLit(id) != null)
			objectivesToChange.put(iObj, null);
		}
	    }
	    else
		objectivesToChange.put(this.getIObjFromY(id), null);

	}
	change = this.uncoverXs(currentExplanationX);
	for(int iObj :objectivesToChange.keySet()){
	    // Log.comment(3, "changing upperlimit " + iObj);
	    int upperKDBefore = this.getUpperKD(iObj);
 	    if(this.getUpperKD(iObj) == this.getUpperBound(iObj))
		this.generateNext(iObj,this.getUpperKD(iObj), this.getMaxUncoveredKD(iObj));
	    this.setUpperKD(iObj, this.nextKDValue(iObj, this.getUpperKD(iObj)));
	    if(this.getUpperKD(iObj) >= this.getUpperBound(iObj))
		this.generateNext(iObj, this.getUpperKD(iObj), this.getMaxUncoveredKD(iObj));
	    this.setUpperBound(iObj, this.nextKDValue(iObj, this.getUpperKD(iObj)));
	    if(this.getUpperKD(iObj)!= upperKDBefore)
		change = true;
	}
	return change;

    }
    abstract public int nextKDValue(int iObj, int kD);

    abstract public int getMaxUncoveredKD(int iObj);
    abstract public int generateNext(int iObj, int kD, int inclusiveMax);



    /**
     *Sets the current upper bound of iObj to nowKD
     *@param newKD
     *@param iObj
     */
    private void setUpperBound(int iObj, int newKD){
	    this.UpperBound[iObj] = newKD;
    }

    /**
     *Log the current upperBound
     */

    public void logUpperBound()    {
	String logUpperLimit = "diff upper bound: ["+this.getUpperBound(0);
	for(int iObj = 1; iObj < this.instance.nObjs(); ++iObj)
	    logUpperLimit +=", "+this.getUpperBound(iObj) ;//+ this.instance.getObj(iObj).getMinValue())
	//..log
	
	logUpperLimit +="]";
	Log.comment(2, logUpperLimit );
    }

}
