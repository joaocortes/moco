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
import org.sat4j.moco.problem.SelectionDelimeter;
import org.sat4j.moco.problem.SeqEncoder;
import org.sat4j.moco.problem.GenTotalEncoder;
import org.sat4j.moco.problem.GenTotalEncoderMSU3;
import org.sat4j.moco.problem.GoalDelimeter;
import org.sat4j.moco.problem.GoalDelimeterMSU3;
import org.sat4j.moco.util.Log;
import org.sat4j.specs.ContradictionException;
import org.sat4j.specs.IVecInt;

/**
 * Class that implements UnsatSat, MSU3 flavoured
 * @author João Cortes
 */

public class UnsatSatMSU3 extends algorithm {

    /**
     * IDs of the variables used int the sequential encoder. The first
     * index is the goal, the second is the first index of s from " On
     * using Incremental Encodings...".Remember that s(i,j) is an
     * indicator of the propositions of the form x_i>=j.
     */

    private GoalDelimeterMSU3<?> goalDelimeter = null;

    /**
     * Last explored differential k, for each objective function.
     */
    private int[] UpperKD = null;

    /**
     *  Last id of the real, non auxiliary,  variables
     */
    private int realVariablesN = 0;

    /**
     *signals that the MSU3 flavour is active
     */
    private boolean MSU3 = false;

    /**
     * Creates an instance of a MOCO solver, for a given instance,
     * that applies the Pareto-MCS algorithm.
     * @param m The MOCO instance.
     */

    public UnsatSatMSU3(Instance m, String encodingGD, boolean MSU3) {
        // Log.comment(3, "in UnsatSat constructor");
	this.MSU3 = MSU3;
	this.problem = m;
	this.result = new Result(m, true);
	try {
            this.solver = buildSolver();
        }
        catch (ContradictionException e) {
            // Log.comment(3, "Contradiction in ParetoMCS.buildSolver");
            return;
        }

	switch(encodingGD){
	case "SD":
	    this.goalDelimeter = new SelectionDelimeter(m, solver);
	    break;
	// case "GTE":	    
	//     this.goalDelimeter = new GenTotalEncoderMSU3(m, solver, MSU3);
	//     break;
	// case "SWC":
	//     this.goalDelimeter = new SeqEncoder(m, solver);
	// default:
	//     this.goalDelimeter = new SeqEncoder(m, solver);
	//     break;
	}
	
	this.realVariablesN = this.solver.nVars();
    }

    public UnsatSatMSU3(Instance m, String encodingGD) {
	this(m, encodingGD, true);
    }

    /**
     * Applies the UnsatSat algorithm to the MOCO instance provided
     */

    public void solve(){
	IVecInt currentExplanation = new VecInt(new int[] {});
	IVecInt currentAssumptions = new VecInt(new int[] {});

	if(!this.MSU3)
	    this.subResult = new SubResult(this.problem);
	else
	    this.subResult = this.result;

	boolean goOn = true;
	boolean goOn1 = true;
	this.generateUpperBoundAssumptions(currentExplanation);
	this.logUpperLimit();
	while(goOn){
	    if(currentAssumptions != null)
		solver.check(currentAssumptions);
	    if(goOn1 && solver.isSat()){
		this.subResult.saveModel(this.solver);
		Log.comment("model:");
		this.goalDelimeter.prettyPrintVecInt(this.getXModel());
		int[] diffAttainedValue = this.diffAttainedValue();
 		if(! this.blockDominatedRegion(diffAttainedValue)){
		    goOn1 = false;
		}

	    }else{
		if(!MSU3)
		    transferSubResult();
		this.solver.printStats();
		goOn = goOn1;
		if(goOn){
		    currentExplanation = solver.unsatExplanation();
		    if(currentExplanation.size() == 0){
			goOn = false;
		    }else{
			currentAssumptions = this.generateUpperBoundAssumptions(currentExplanation);
			this.logUpperLimit();
			// if currentAssumptions are null, then the attainable domain did was not expanded and there is no need to keep going 
			if(currentAssumptions == null){
			}else{
			    Log.comment(2, "There was no expansion");
			    goOn = false;
			}
		    }
		}
	    }
	}
	this.result.setParetoFrontFound();

	return;
    }



    /**
     *Log the value of the upperLimit
     */

    private void logUpperLimit()    {
	String logUpperLimit = "diff upper limit: ["+this.getUpperKD(0);
	for(int iObj = 1; iObj < this.problem.nObjs(); ++iObj)
	    logUpperLimit +=", "+this.getUpperKD(iObj) ;//+ this.problem.getObj(iObj).getMinValue())
	//..log
	
	logUpperLimit +="]";
	Log.comment(2, logUpperLimit );
    }

    
    
    
    /**
     * Generate the upper limit assumptions
     */
    public IVecInt generateUpperBoundAssumptions(IVecInt explanation ){
	IVecInt assumptions = new VecInt(new int[]{});
	assumptions = this.goalDelimeter.generateUpperBoundAssumptions(explanation);
	return assumptions;
    }


    private void analyzeDisjointCores(){
	IVecInt currentAssumptions = this.goalDelimeter.generateUpperBoundAssumptions();
	IVecInt disjointCoresLiterals = new VecInt(new int[]{});
	IVecInt currentExplanation = new VecInt(new int[]{});
	int disjointCoresN = 0;
	solver.check(currentAssumptions);
	if(!solver.isSat()){
	    currentExplanation = solver.unsatExplanation();
	    disjointCoresN++;
		    for(int x: currentExplanation.toArray())
			if(this.goalDelimeter.isX(x)){
			    currentAssumptions.delete( currentAssumptions.indexOf(x));
			    disjointCoresLiterals.push(x);
		}
	    solver.check(currentAssumptions);
	}
	Log.comment(2, "number of disjoint cores: "  + disjointCoresN);
	Log.comment(2, "disjoint core union size: "  + disjointCoresLiterals.size());
    }


    /**
     *gets the current upper limit of the explored value of the
     *differential k of the ithOjective
     *@param iObj
     */

    private int getUpperKD(int iObj){
	return this.goalDelimeter.getUpperKD(iObj);
    }


    /**
     *Checks if literal is an Y variable
     *@param literal
     */

    public boolean isY(int literal){
	if(this.goalDelimeter.isY(literal))
	    return true;
	return false;
    }
    /**
     *Checks if literal is an Y variable
     *@param literal
     */

    public boolean isX(int literal){
	return this.goalDelimeter.isX(literal);

    }

    /**
     *returns the model in DIMACS format, including only the real
     *variables and the Y variables of the sequential encoder
     *@return a filtered model
     */

    public IVecInt getYModel(){
	IVecInt model = new VecInt(new int[] {});
	for(int id = 1; id <= this.solver.nVars();++id){
	    int literal = (this.solver.modelValue(id))? id: -id;
	    if(this.isY(literal))
		model.push(literal);
	}
	return model;
    }


    /**
     *returns the model in DIMACS format, including only the real
     *variables and the Y variables of the sequential encoder
     *@return a filtered model
     */

    public boolean[] getXModelValues(){
	boolean[] modelValues = new boolean[this.problem.nVars()];
	for(int id = 1; id <= this.problem.nVars();++id){
	    modelValues[id - 1] = this.solver.modelValue(id);
	}
	return modelValues;
    }

    /**
     *returns the model in DIMACS format, including only the real
     *variables and the Y variables of the sequential encoder
     *@return a filtered model
     */

    public IVecInt getXModel(){
	IVecInt model = new VecInt(new int[] {});
	for(int id = 1; id <= this.solver.nVars();++id){
	    int literal = (this.solver.modelValue(id))? id: -id;
	    if(this.isX(literal))
		model.push(literal);
	}
	return model;
    }
    /**
     *returns the model in DIMACS format, including only the real
     *variables and the Y variables of the sequential encoder
     *@return a filtered model
     */

    public IVecInt getFullModel(){
	IVecInt model = new VecInt(new int[] {});
	for(int id = 1; id <= this.solver.nVars();++id){
	    int literal = (this.solver.modelValue(id))? id: -id;
	    model.push(literal);
	}
	return model;
    }


    /**
     * Print the models
     * @param models, the obtained models
     */
    public void printModels(Vector<IVecInt> models) {
	for(int i = 0; i <models.size(); ++i){
	    Log.comment(5, "Model " + i);
	    this.printModel(models.get(i));
	}
	return;
    }

    // /**
    //  * Print an modelY
    //  * @param model,
    //  */

    public void printModelY(IVecInt modelY) {
    	int[][] convertedModel = new int[(modelY.size())][];
    	for(int i=0, n = modelY.size();i<n;i++){
    	    int yId = this.solver.idFromLiteral( modelY.get(i));
    	    int iObj = this.goalDelimeter.getIObjFromY(yId);
    	    int kD = this.goalDelimeter.getKDFromY(yId);
    	    convertedModel[i] = new int[]{  iObj, kD,modelY.get(i),};
    	}


    	Arrays.sort(convertedModel, Comparator.comparing(IntBuffer::wrap));

	String logYModel = "";
	int  currentIObj = convertedModel[0][0];
    	for(int i=0, n = convertedModel.length;i<n;i++){
	    if(convertedModel[i][0] == currentIObj)	 {
		// if(convertedModel[i][2] > 0)
		logYModel += this.goalDelimeter.prettyFormatVariable(convertedModel[i][2]) + " ";
	    }
	    else{
		Log.comment(3, logYModel);
		logYModel = "";
		currentIObj = convertedModel[i][0];
		Log.comment(5, "");
		i--;
	    }
	}
		Log.comment(3, logYModel);
    }

    /**
     * Print a model
     * @param models, the obtained models
     */

    public void printModel(IVecInt model) {
	this.goalDelimeter.prettyPrintVecInt(model, 3);
	}


    /**
     * The attained value of objective in the interpretation of the
     * last found model
     @param objective
    */
    private int attainedValue(Objective objective){
	int result = 0;
	int objectiveNLit = objective.getTotalLits();
	ReadOnlyVecInt objectiveLits = objective.getSubObjLits(0);
	ReadOnlyVec<Real> objectiveCoeffs = objective.getSubObjCoeffs(0);
	for(int iLit = 0; iLit < objectiveNLit; ++iLit  ){
	    int coeff = objectiveCoeffs.get(iLit).asInt();
	    int literal = objectiveLits.get(iLit);
	    if(this.solver.modelValue(literal))
		result += coeff;
	}
	return result;
    }

    public int[] diffAttainedValue(){
	int[] diffAttainedValue = new int[this.problem.nObjs()];
	for(int i = 0; i < this.problem.nObjs(); ++i){
	    diffAttainedValue[i] = this.attainedValue(this.problem.getObj(i));
	    diffAttainedValue[i]-=this.problem.getObj(i).getMinValue();
	}
	return diffAttainedValue;
    }

    /**
     * Block the region dominated by the last found model.
     */

    public boolean blockDominatedRegion(int[] diffAttainedValue ){
    
	String logDiffAttainedValue = "diff attained value: ["+ diffAttainedValue[0];
	for(int iObj = 1; iObj < this.problem.nObjs(); ++iObj)
	    logDiffAttainedValue +=", "+ diffAttainedValue[iObj];
	//..log
	
	logDiffAttainedValue +="]";
	Log.comment(2, logDiffAttainedValue );
	IVecInt newHardClause = new VecInt();
	for (int iObj = 0; iObj < this.problem.nObjs(); ++iObj){
	    if(diffAttainedValue[iObj] != 0){
		int possibleClause =- this.goalDelimeter.getY(iObj, diffAttainedValue[iObj]);
		//this better always be true.
		if(possibleClause != 0)
		    newHardClause.push(possibleClause);
	    }	}	    
	// Log.comment(6, "Blocking clause:");
	return this.AddClause(newHardClause);

    }
    
    public boolean blockModelX(IVecInt modelX){
	IVecInt notPreviousModel = new VecInt(new int[] {});
	for(int iX = 0; iX < modelX.size(); ++iX)
	    notPreviousModel.push(-modelX.get(iX));
	return this.AddClause(notPreviousModel);
    }

    public void prettyPrintVecInt(IVecInt vecInt, boolean clausing){
	if(clausing)
	    Log.clausing(this.goalDelimeter.prettyFormatVecInt(vecInt));
	else
	    Log.comment(6, this.goalDelimeter.prettyFormatVecInt(vecInt));
	return;
    }
    public String prettyFormatVecInt(IVecInt literals){return this.goalDelimeter.prettyFormatVecInt(literals);}


    public void printFlightRecordParticular(){
	// Log.comment(2, "covered x variables: " + this.coveredLiterals.size());
	this.goalDelimeter.logUpperBound();
	this.goalDelimeter.logUncoveredMaxKD();

    }
}
