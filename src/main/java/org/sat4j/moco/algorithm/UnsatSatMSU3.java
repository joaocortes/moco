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
 *   Miguel Terra-Neves, Ines Lynce and Vasco Manquinho - MOCO solver
 *******************************************************************************/
package org.sat4j.moco.algorithm;

import java.util.Vector;
import java.nio.IntBuffer;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import org.sat4j.core.VecInt;
import org.sat4j.core.ReadOnlyVec;
import org.sat4j.core.ReadOnlyVecInt;
import org.sat4j.moco.analysis.Result;
import org.sat4j.moco.analysis.SubResult;
import org.sat4j.moco.util.Real;
import org.sat4j.moco.problem.Instance;
import org.sat4j.moco.problem.Objective;
import org.sat4j.moco.goal_delimeter.SelectionDelimeter;
import org.sat4j.moco.goal_delimeter.SelectionDelimeterMSU3;
import org.sat4j.moco.goal_delimeter.SeqEncoder;
import org.sat4j.moco.goal_delimeter.GenTotalEncoder;
import org.sat4j.moco.goal_delimeter.GenTotalEncoderMSU3;
import org.sat4j.moco.goal_delimeter.GoalDelimeter;
import org.sat4j.moco.goal_delimeter.GoalDelimeterI;
import org.sat4j.moco.goal_delimeter.GoalDelimeterMSU3;
import org.sat4j.moco.goal_delimeter.Index;
import org.sat4j.moco.util.Log;
import org.sat4j.specs.ContradictionException;
import org.sat4j.specs.IVecInt;

/**
 * Class that implements UnsatSat, MSU3 flavoured
 * @author João Cortes
 */

public class UnsatSatMSU3 extends  UnsatSat {

    
    public UnsatSatMSU3(Instance m) {
        // Log.comment(3, "in UnsatSat constructor");
	super(m);
	this.subResult = new SubResult(this.problem);
	
    }




    // private void analyzeDisjointCores(){
    // 	IVecInt currentAssumptions = this.GetGoalDelimeter().generateUpperBoundAssumptions();
    // 	IVecInt disjointCoresLiterals = new VecInt(new int[]{});
    // 	IVecInt currentExplanation = new VecInt(new int[]{});
    // 	int disjointCoresN = 0;
    // 	solver.check(currentAssumptions);
    // 	if(!solver.isSat()){
    // 	    currentExplanation = solver.unsatExplanation();
    // 	    disjointCoresN++;
    // 	    for(int x: currentExplanation.toArray())
    // 		if(this.GetGoalDelimeter().isX(x)){
    // 		    currentAssumptions.delete( currentAssumptions.indexOf(x));
    // 		    disjointCoresLiterals.push(x);
    // 		}
    // 	    solver.check(currentAssumptions);
    // 	}
    // 	Log.comment(2, "number of disjoint cores: "  + disjointCoresN);
    // 	Log.comment(2, "disjoint core union size: "  + disjointCoresLiterals.size());
    // }

    public void saveModel(){
	this.result.saveModel(this.solver);
    }

    public void finalizeHarvest(){}

}
