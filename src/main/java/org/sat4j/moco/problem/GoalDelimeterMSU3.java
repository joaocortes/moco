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
import org.sat4j.moco.problem.GenTotalEncoder.SumTree;
import org.sat4j.moco.problem.GenTotalEncoderMSU3.SumTree.Node;
import org.sat4j.specs.IVecInt;


/**
 * Interface that contains MSU3 like abilities for GoalDelimeters. 
 * @author Joao O'Neill Cortes
 */


public abstract class GoalDelimeterMSU3<PIndex extends GoalDelimeter.Index> extends GoalDelimeter<PIndex>{
    private HashMap<Integer, Boolean> coveredLiterals = null;
    boolean change = false;

    //TODO: this is to be put in the goalManager

	/**
	 *Max uncovered KD value
	 */
    protected int maxUncoveredKD = 0;

    /**
     * Upper bound, exclusive
     */
    private int[] UpperBound = null;

    private int getUpperBound(int iObj){
	return this.UpperBound[iObj];
    }

    protected boolean uncoverXs(IVecInt explanationX) {
	// Log.comment(5, "{ UnsatSatMSU3.uncoverXs");
	int[] explanationXarray = explanationX.toArray();
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
	    this.setMaxUncoveredKD(a);
	}
    public void setMaxUncoveredKD(int a){this.maxUncoveredKD = a;}
    abstract void updateAllUncoveredMaxKD();

    public void logUncoveredMaxKD(){
	String logUpperLimit = "uncovered max: ["+this.getUncoveredMaxKD(0);
	for(int iObj = 1; iObj < this.instance.nObjs(); ++iObj)
	    logUpperLimit +=", "+this.getUncoveredMaxKD(iObj) ;//+ this.instance.getObj(iObj).getMinValue())
	//..log
	
	logUpperLimit +="]";
	Log.comment(2, logUpperLimit );
    }

    abstract public int getUncoveredMaxKD(int iObj);



}
