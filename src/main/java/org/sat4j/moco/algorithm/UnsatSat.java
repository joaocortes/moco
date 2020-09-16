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
import java.util.Arrays;
import java.util.Comparator;
import org.sat4j.core.VecInt;
import org.sat4j.core.ReadOnlyVec;
import org.sat4j.core.ReadOnlyVecInt;
import org.sat4j.moco.analysis.Result;
import org.sat4j.moco.analysis.SubResult;
import org.sat4j.moco.util.Real;
import org.sat4j.moco.pb.PBSolver;
import org.sat4j.moco.problem.Instance;
import org.sat4j.moco.problem.Objective;
import org.sat4j.moco.problem.GoalDelimeter;
import org.sat4j.moco.problem.SeqEncoder;
import org.sat4j.moco.problem.GenTotalEncoder;
import org.sat4j.moco.problem.SelectionDelimeter;
import org.sat4j.moco.util.Log;
import org.sat4j.specs.ContradictionException;
import org.sat4j.specs.IVecInt;

/**
 * Class that implements UnsatSat
 * @author João Cortes
 */

public class UnsatSat extends algorithm {

    /**
     * IDs of the variables used int the sequential encoder. The first
     * index is the goal, the second is the first index of s from " On
     * using Incremental Encodings...".Remember that s(i,j) is an
     * indicator of the propositions of the form x_i>=j.
     */

    private GoalDelimeter<?> goalDelimeter = null;

    /**
     * Last explored differential k, for each objective function.
     */
    private int[] UpperKD = null;

    /**
     * Exhausted upperKD. At any time, all that solutions that
     * dominate this point were found already.
     */
    private int[] exhaustedUpperKD = null;

    /**
     *  Last id of the real, non auxiliary,  variables
     */
    private int realVariablesN = 0;

    /**
     * Creates an instance of a MOCO solver, for a given instance,
     * that applies the Pareto-MCS algorithm.
     * @param m The MOCO instance.
     */

    public UnsatSat(Instance m) {
        // Log.comment(3, "in UnsatSat constructor");
	this.problem = m;
	this.result = new Result(m, true);
	try {
            this.solver = buildSolver();
        }
        catch (ContradictionException e) {
            Log.comment(3, "Contradiction in ParetoMCS.buildSolver");
            return;
        }
	this.realVariablesN = this.solver.nVars();
	this.UpperKD =  new int[(this.problem.nObjs())];
	this.exhaustedUpperKD =  new int[(this.problem.nObjs())];
    }


    public UnsatSat(Instance m, boolean encodingGD) {

	this(m);
	if(encodingGD)
	    this.goalDelimeter = new GenTotalEncoder(this.problem,this.solver);
	else
	    this.goalDelimeter = new SeqEncoder(this.problem,this.solver);
    }


    public UnsatSat(Instance m, String encoding ) {
	this(m);
	switch(encoding){
	case "SD":
	    this.goalDelimeter = new SelectionDelimeter(m, solver);
	    break;
	case "GTE":	    
	    this.goalDelimeter = new GenTotalEncoder(m, solver);
	    break;
	case "SWC":
	    this.goalDelimeter = new SeqEncoder(m, solver);
	default:
	    this.goalDelimeter = new SeqEncoder(m, solver);
	    break;
	}
    }

    /**
     * Applies the UnsatSat algorithm to the MOCO instance provided
     */

    public void solve() {
	IVecInt currentExplanation = new VecInt(new int[] {});
	IVecInt currentAssumptions = new VecInt(new int[] {});
	this.subResult = new SubResult(this.problem);


	boolean goOn = true;
	boolean goOn1 = true;
	this.logUpperLimit();
	this.preAssumptionsExtend();
	currentAssumptions = this.goalDelimeter.generateUpperBoundAssumptions(this.UpperKD);
	while(goOn){
	    solver.check(currentAssumptions);
	    if(goOn1 && solver.isSat()){
		this.subResult.saveModel(this.solver);
		int[] diffAttainedValue = this.diffAttainedValue();
 		if(! this.blockDominatedRegion(diffAttainedValue)){
		    goOn1 = false;
		}

		// if(! this.blockModelX(modelsX.lastElement()))
		//     goOn = false;
	    }else{
		this.transferSubResult();
		goOn = goOn1;
		if(goOn){
		    currentExplanation = solver.unsatExplanation();
		if(currentExplanation.size() == 0){
		    goOn = false;
		}else{
		    this.printFlightRecord();
		    this.exhaustedUpperKD = this.UpperKD;
		    this.logExhaustedUpperKD();
		    this.updateUpperBound(currentExplanation);
		    this.preAssumptionsExtend();
		    currentAssumptions = this.goalDelimeter.generateUpperBoundAssumptions(this.UpperKD);

		    }}
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
     *Log the value of the exhaustedUpperKD
     */

    private void logExhaustedUpperKD()    {
	String logExhaustedUpperKD = "exhausted upper limit: ["+this.exhaustedUpperKD[0];
	for(int iObj = 1; iObj < this.problem.nObjs(); ++iObj)
	    logExhaustedUpperKD +=", "+ (this.exhaustedUpperKD[iObj]);
	logExhaustedUpperKD +="]";
	Log.comment(0, logExhaustedUpperKD );
    }
    


    /**
     *If necessary for the construction of the current assumptions,
     *initialize more of the domain of the sequential encoder
     *differential k index
     */

    private void preAssumptionsExtend(){
	int objN = this.problem.nObjs();
	for(int iObj = 0; iObj < objN ; ++iObj){
	    int ithMax = this.problem.getObj(iObj).getWeightDiff();
	    if(this.getUpperKD(iObj) == ithMax){
		this.goalDelimeter.UpdateCurrentK(iObj, ithMax);
	    }
	    else{
		this.goalDelimeter.UpdateCurrentK(iObj, this.getUpperKD(iObj)+1);
	    }
	}
    }


    /**
     * Updates the current upperBound on the differential k, according
     * to the unsatExplanation, and updates the GoalDelimeter accordingly
     * @param currentExplanation
     * current explanation of unsatisfiability
     */
    private void updateUpperBound(IVecInt currentExplanation){
	int[] provUpperKD = new int[this.UpperKD.length];
	int objN = this.problem.nObjs();

	for(int i = 0; i < currentExplanation.size(); ++i){
	    int ithLiteral = currentExplanation.get(i);
	    int id = this.solver.idFromLiteral(ithLiteral);
	    if(this.goalDelimeter.isY(id)){
		int jObj = this.goalDelimeter.getIObjFromY(id);
		int kd = this.goalDelimeter.getKDFromY(id);
		this.setUpperKD(jObj, kd);
		// this.goalDelimeter.UpdateCurrentK(jObj, kd);

	    }else{
		if(this.goalDelimeter.isX(id)){

		    for(int iObj = 0; iObj < objN ; ++iObj){
			Objective ithObjective = this.problem.getObj(iObj);
			IVecInt ithObjectiveXs = ithObjective.getSubObjLits(0);
			int nX = ithObjective.getTotalLits(); 
			int iX = 0;
			for( iX = 0; iX < nX ;iX ++  )
			    if(ithObjectiveXs.get(iX) == id)
				break;
			if(iX < nX)
			    {
				int weight = ithObjective.getSubObjCoeffs(0).get(iX).asInt();
				weight = weight > 0? weight: -weight;
				if(provUpperKD[iObj] == 0)
				    if(weight >= this.getUpperKD(iObj))
					provUpperKD[iObj] = weight;
				    else
					if(weight >= this.getUpperKD(iObj))
					    if(provUpperKD[iObj] > weight)
						provUpperKD[iObj] = weight;
			    }
		    }
		}
	    }
	}
		    
	// provUpperKD is the array of the minimal weights above the upperKD value
	for(int iObj = 0; iObj < objN ; ++iObj)
	    if(provUpperKD[iObj] > this.getUpperKD(iObj))
		this.setUpperKD(iObj, provUpperKD[iObj]);
	// Log.comment(5, "{ done");
    }


    /**
     *gets the current upper limit of the explored value of the
     *differential k of the ithOjective
     *@param iObj
     */

    private int getUpperKD(int iObj){
	return this.UpperKD[iObj];
    }

    /**
     *Sets the current upper limit of the explored value of the
     *differential k of the ithOjective to newKD
     *@param newKD
     *@param iObj
     */
    private void setUpperKD(int iObj, int newKD){
	// if(this.goalDelimeter.getCurrentKD(iObj) < newKD)
	//     this.goalDelimeter.UpdateCurrentK(iObj, newKD);
	this.UpperKD[iObj] = newKD;
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
	int id = (literal>0)? literal: -literal;
	return id <= this.realVariablesN && id >= 1;

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
     *returns the model in DIMACS format
     */

    public IVecInt getModel(){
	IVecInt model = new VecInt(new int[] {});
	for(int id = 1; id <= this.solver.nVars();++id){
	    if(this.solver.modelValue(id))
		model.push(id);
	    else
		model.push(id);
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
    	    convertedModel[i] = new int[]{ modelY.get(i), iObj, kD};
    	}

    	Arrays.sort(convertedModel, new Comparator<int[]>() {
    		public int compare(int[] o1, int[] o2) {
    		    // Intentional: Reverse order for this demo
		    return o1[1]-o2[1];
    		}
    	    });

	String logYModel = "";
	int  currentIObj = convertedModel[0][1];
    	for(int i=0, n = convertedModel.length;i<n;i++){
	    if(convertedModel[i][1] == currentIObj)	 
		if(convertedModel[i][0] > 0)
		    logYModel += this.goalDelimeter.prettyFormatVariable(convertedModel[i][0]) + " ";
	    else{
		Log.comment(5, "\n");
		currentIObj = convertedModel[i][1];
	    }
	}
	Log.comment(2, logYModel);
    }

    /**
     * Print a model
     * @param models, the obtained models
     */

    public void printModel(IVecInt model) {
	this.goalDelimeter.prettyPrintVecInt(model, 2);
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
	    if(diffAttainedValue[iObj] != 0)
		newHardClause.push( - this.goalDelimeter.getY(iObj, diffAttainedValue[iObj]));
	}
	Log.comment(6, "Blocking clause:");
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

    public String prettyFormatVecInt(IVecInt vecInt){return this.goalDelimeter.prettyFormatVecInt(vecInt);}
    public void printFlightRecordParticular(){
	// String logExhaustedUpperKD = "exhausted upper limit: ["+this.exhaustedUpperKD[0];
	// for(int iObj = 1; iObj < this.problem.nObjs(); ++iObj)
	//     logExhaustedUpperKD +=", "+ (this.exhaustedUpperKD[iObj]);
	
	// logExhaustedUpperKD +="]";
	// Log.comment(2, logExhaustedUpperKD);

    }
}
