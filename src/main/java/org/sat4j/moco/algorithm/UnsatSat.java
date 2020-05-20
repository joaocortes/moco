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
import java.util.HashMap;
import org.sat4j.core.VecInt;
import org.sat4j.core.ReadOnlyVec;
import org.sat4j.core.ReadOnlyVecInt;
import org.sat4j.moco.analysis.Result;
import org.sat4j.moco.analysis.SubResult;
import org.sat4j.moco.util.Real;
import org.sat4j.moco.pb.PBSolver;
import org.sat4j.moco.problem.Instance;
import org.sat4j.moco.problem.Objective;
import org.sat4j.moco.problem.GenTotalEncoder;
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

    private GenTotalEncoder goalDelimeter = null;

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

    private HashMap<Integer, Boolean> coveredLiterals = null;
    /**
     * Creates an instance of a MOCO solver, for a given instance,
     * that applies the Pareto-MCS algorithm.
     * @param m The MOCO instance.
     */

    public UnsatSat(Instance m, boolean encodingGD) {
        Log.comment(3, "in UnsatSat constructor");
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
	this.coveredLiterals = new HashMap<Integer, Boolean>(this.realVariablesN);
	for(int iObj = 0, nObj = this.problem.nObjs();iObj < nObj; iObj++){
	    Objective ithObjective = this.problem.getObj(iObj);
	    ReadOnlyVecInt objectiveLits = ithObjective.getSubObjLits(0);
	    ReadOnlyVec<Real> objectiveCoeffs = ithObjective.getSubObjCoeffs(0);
	    int sign = 1;
	    int ithAbsoluteWeight;
	    for(int iX = 0, nX = ithObjective.getTotalLits(); iX <nX; iX ++){
		int ithX = objectiveLits.get(iX);
		ithAbsoluteWeight = objectiveCoeffs.get(iX).asInt();
		sign = (ithAbsoluteWeight > 0? 1 : -1);
		ithAbsoluteWeight *= sign;
		this.coveredLiterals.put(-sign * ithX, null);
	    }
	}
	this.goalDelimeter = new GenTotalEncoder(this.problem,this.solver);
	this.UpperKD =  new int[(this.problem.nObjs())];
    }



    /**
     * Applies the UnsatSat algorithm to the MOCO instance provided
     */

    public void solve() {
	IVecInt currentExplanation = new VecInt(new int[] {});
	IVecInt currentAssumptions = new VecInt(new int[] {});
	IVecInt currentYModel = new VecInt(new int[] {});
	// boolean[] currentXModelValues = new boolean[this.problem.nVars()];
	// Vector<IVecInt> modelsX = new Vector<IVecInt>();
	// Vector<IVecInt> modelsY = new Vector<IVecInt>();
	SubResult subResult = new SubResult(this.problem);


        // if (this.result.isParetoFront()) {
        //     Log.comment(1, "UnsatSat.solve called on already solved instance");
        //     return;
        // }
	Log.comment(3, "in UnsatSat.solve");
	boolean goOn = true;
	boolean goOn1 = true;
	this.logUpperLimit();
	// this.preAssumptionsExtend(currentExplanation);
	currentAssumptions = this.generateUpperBoundAssumptions();
	while(goOn){
	    this.logUpperLimit();
	    Log.comment(5, "Checking against assumptions:");
	    this.goalDelimeter.prettyPrintVecInt(currentAssumptions);
	    solver.check(currentAssumptions);

	    if(goOn1 && solver.isSat()){
		subResult.saveModel(this.solver);
		Log.comment(5, " current subResult size:" + subResult.nSolutions());
		currentYModel = this.getYModel();
		Log.comment(5, "ModelX :");
		this.printModel(this.getXModel());
		Log.comment(5, "ModelY :");
		this.printModelY(currentYModel);
		Log.comment(5, "Blocking dominated region");
		int[] diffAttainedValue = this.diffAttainedValue();
 		if(! this.blockDominatedRegion(diffAttainedValue)){
		    goOn1 = false;
		}

	    }else{
		for(int i = 0; i < subResult.nSolutions(); ++i)
		    this.result.addSolutionUnsafe(subResult.getSolution(i));
		subResult = new SubResult(this.problem);
		goOn = goOn1;
		if(goOn){
		    currentExplanation = solver.unsatExplanation();
		    //log..
		    Log.comment(5, "Explanation:");
		    this.goalDelimeter.prettyPrintVecInt(currentExplanation);
		    Log.comment(5, "//");
		
		    if(currentExplanation.size() == 0){
			goOn = false;
		    }else{
			IVecInt currentExplanationX = new VecInt(new int[] {});
			for(int lit: currentExplanation.toArray()){
			    int id = this.solver.idFromLiteral(lit);
			    if(this.goalDelimeter.isX(id))
				currentExplanationX.push(-lit);
			}
			this.uncoverXs(currentExplanationX);
			this.bindXs();
			this.exhaustedUpperKD = this.UpperKD;
			this.logExhaustedUpperKD();
			for(int iObj = 0; iObj < this.problem.nObjs(); ++iObj)
			    this.setUpperKD(iObj, this.goalDelimeter.getCurrentKD(iObj));
			this.preAssumptionsExtend();
			currentAssumptions = this.generateUpperBoundAssumptions();
		    }}
	    }
	}
	this.result.setParetoFrontFound();
	return;
    }


    /**
     *Uncover leafs
     */
    private boolean uncoverXs(IVecInt explanationX)
    {
	boolean change = false;
	for(int iObj = 0; iObj < this.problem.nObjs(); ++iObj)
	    change = this.goalDelimeter.addLeafs(iObj, explanationX) || change;
	int[] explanationXarray = explanationX.toArray();
	for(int x : explanationXarray)
	    this.coveredLiterals.remove(x);
	return change;
    }

    /**
     *bind leafs
     */
    private boolean bindXs()
    {	boolean change = false;
	for(int iObj = 0; iObj < this.problem.nObjs(); ++iObj)
	    change = this.goalDelimeter.bindFreshSubTree(iObj) || change;
	return change;
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
	Log.comment(1, logExhaustedUpperKD );
    }
    
    /**
     * Generate the upper limit assumptions
     */
    public IVecInt generateUpperBoundAssumptions( ){
	IVecInt assumptions = new VecInt(new int[]{});
	
	for(int iObj = 0; iObj < this.problem.nObjs(); ++iObj){
	    Objective ithObjective = this.problem.getObj(iObj);
	    if(this.getUpperKD(iObj)  < ithObjective.getWeightDiff()){
		int newY = -this.goalDelimeter.getY(iObj, this.getUpperKD(iObj) + 1);
		if(newY!=0)
		    assumptions.push(newY);
	    }
	    for(Integer x: this.coveredLiterals.keySet())
		assumptions.push(x);

	}

	return assumptions;
    }


    /**
     *If necessary for the construction of the current assumptions,
     *initialize more of the domain of the goal delimeter
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
	if(this.getUpperKD(iObj)< newKD)
	    this.UpperKD[iObj] = newKD;
    }

    /**
     * Creates a PB oracle initialized with the MOCO's constraints.
     * @return The oracle.
     * @throws ContradictionException if the oracle detects that the
     * MOCO's constraint set is unsatisfiable.
     */
    private PBSolver buildSolver() throws ContradictionException {
        Log.comment(5, "in UnsatSat.buildSolver");
        PBSolver solver = new PBSolver();
        solver.newVars(this.problem.nVars());
        for (int i = 0; i < this.problem.nConstrs(); ++i) {
            solver.addConstr(this.problem.getConstr(i));
        }
        Log.comment(5, "out UnsatSat.buildSolver");
        return solver;
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
	if(modelY.size()>0){
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


	int  currentIObj = convertedModel[0][1];
    	for(int i=0, n = convertedModel.length;i<n;i++){
	    if(convertedModel[i][1] == currentIObj)	 
    		this.goalDelimeter.prettyPrintVariable(convertedModel[i][0]);
	    else{
		Log.comment(5, "\n");
		currentIObj = convertedModel[i][1];
	    }
	}
	}

    }
    /**
     * Print a model
     * @param models, the obtained models
     */

    public void printModel(IVecInt model) {
	for(int j = 0; j <model.size(); ++j){
	    if(model.get(j)>0)
		this.goalDelimeter.prettyPrintVariable(model.get(j),2);
	}

	return;
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

    public void printFlightRecordParticular(){
	String logExhaustedUpperKD = "completed upper limit: ["+this.exhaustedUpperKD[0];
	for(int iObj = 1; iObj < this.problem.nObjs(); ++iObj)
	    logExhaustedUpperKD +=", "+ (this.exhaustedUpperKD[iObj] - this.problem.getObj(iObj).getMinValue());
	
	logExhaustedUpperKD +="]";
	System.out.print("f " + logExhaustedUpperKD);

    }
}
