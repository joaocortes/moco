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
import org.sat4j.moco.pb.PBSolver;
import org.sat4j.moco.problem.Instance;
import org.sat4j.moco.problem.Objective;
import org.sat4j.moco.problem.GenTotalEncoderMSU3;
import org.sat4j.moco.util.Log;
import org.sat4j.specs.ContradictionException;
import org.sat4j.specs.IVecInt;

/**
 * Class that implements UnsatSat
 * @author João Cortes
 */

public class UnsatSatMSU3 extends algorithm {

    /**
     * IDs of the variables used int the sequential encoder. The first
     * index is the goal, the second is the first index of s from " On
     * using Incremental Encodings...".Remember that s(i,j) is an
     * indicator of the propositions of the form x_i>=j.
     */

    private GenTotalEncoderMSU3 goalDelimeter = null;

    /**
     * Last explored differential k, for each objective function.
     */
    private int[] UpperKD = null;

    /**
     * Upper bound, exclusive
     */
    private int[] UpperBound = null;

    /**
     * maxValues, given the current coveredLiterals
     */
    private int[] maxValues = null;

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

    public UnsatSatMSU3(Instance m, boolean encodingGD) {
        // Log.comment(3, "in UnsatSat constructor");
	this.problem = m;
	this.result = new Result(m, true);
	try {
            this.solver = buildSolver();
        }
        catch (ContradictionException e) {
            // Log.comment(3, "Contradiction in ParetoMCS.buildSolver");
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
		this.coveredLiterals.putIfAbsent(-sign * ithX, true);
	    }
	}
	this.goalDelimeter = new GenTotalEncoderMSU3(this.problem,this.solver);
	this.UpperKD =  new int[(this.problem.nObjs())];
	this.UpperBound =  new int[(this.problem.nObjs())];
	this.maxValues =  new int[(this.problem.nObjs())];
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
	this.subResult = new SubResult(this.problem);


        // if (this.result.isParetoFront()) {
        //     Log.comment(1, "UnsatSat.solve called on already solved instance");
        //     return;
        // }
	// Log.comment(3, "in UnsatSat.solve");
	boolean goOn = true;
	boolean goOn1 = true;
	currentAssumptions = this.generateUpperBoundAssumptions();
	this.logUpperLimit();
	this.logUpperBound();
	while(goOn){

	    // Log.comment(3, "Checking against assumptions:");
	    // Log.comment(3, this.goalDelimeter.prettyFormatVecInt(currentAssumptions));
	    solver.check(currentAssumptions);

	    if(goOn1 && solver.isSat()){
		this.subResult.saveModel(this.solver);
		Log.comment(3, " current subResult size:" + this.subResult.nSolutions());
		currentYModel = this.getYModel();
		// Log.comment(3, "ModelX :");
		// this.printModel(this.getXModel());
		// Log.comment(3, "ModelY :");
		// this.printModelY(currentYModel);
		// Log.comment(3, "Blocking dominated region");
		int[] diffAttainedValue = this.diffAttainedValue();
 		if(! this.blockDominatedRegion(diffAttainedValue)){
		    goOn1 = false;
		}

	    }else{
		this.moveSubResult();
		this.solver.printStats();
		
		this.subResult = new SubResult(this.problem);
		goOn = goOn1;
		if(goOn){
		    this.exhaustedUpperKD = this.UpperKD;
		    this.logExhaustedUpperKD();
		    currentExplanation = solver.unsatExplanation();
		    //log..
		    // Log.comment(3, "Explanation:");
		    // Log.comment(3, this.goalDelimeter.prettyFormatVecInt(currentExplanation));
		    // Log.comment(3, "//");
		
		    if(currentExplanation.size() == 0){
			goOn = false;
		    }else{
			this.analyzeDisjointCores();
			boolean change = this.preAssumptionsExtend(currentExplanation);
			this.logUpperLimit();
			this.logUpperBound();
			this.logMaxValues();

			if(change)
			    currentAssumptions = this.generateUpperBoundAssumptions();

			else {
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
     *Uncover leafs
     */
    private boolean uncoverXs(IVecInt explanationX) {
	// Log.comment(5, "{ UnsatSatMSU3.uncoverXs");

	boolean change = false;
	for(int iObj = 0; iObj < this.problem.nObjs(); ++iObj){
	    change = this.goalDelimeter.addLeafs(iObj, explanationX) || change;
	    change = this.goalDelimeter.bindFreshSubTree(iObj, this.getUpperBound(iObj)) || change;
	}
	int[] explanationXarray = explanationX.toArray();
	for(int x : explanationXarray)
	    this.coveredLiterals.remove(x);
	// Log.comment(5, "}");
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
     *Log the current upperBound
     */

    private void logUpperBound()    {
	String logUpperLimit = "diff upper bound: ["+this.getUpperBound(0);
	for(int iObj = 1; iObj < this.problem.nObjs(); ++iObj)
	    logUpperLimit +=", "+this.getUpperBound(iObj) ;//+ this.problem.getObj(iObj).getMinValue())
	//..log
	
	logUpperLimit +="]";
	Log.comment(2, logUpperLimit );
    }
    
    /**
     *Log the current maxValues
     */

    private void logMaxValues()    {
	String logMaxValues = " uncovered max values: ["+this.maxValues[0];
	for(int iObj = 1; iObj < this.problem.nObjs(); ++iObj)
	    logMaxValues +=", "+this.maxValues[iObj] ;//+ this.problem.getObj(iObj).getMinValue())
	//..log
	
	logMaxValues +="]";
	Log.comment(2, logMaxValues );
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
     * Generate the upper limit assumptions
     */
    public IVecInt generateUpperBoundAssumptions( ){
	IVecInt assumptions = new VecInt(new int[]{});
	
	for(int iObj = 0; iObj < this.problem.nObjs(); ++iObj){
	    Objective ithObjective = this.problem.getObj(iObj);
	    if(this.getUpperKD(iObj)  != this.getUpperBound(iObj)){
		int newY = -this.goalDelimeter.getY(iObj, this.getUpperBound(iObj));
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
		if(ithAbsoluteWeight > this.getUpperKD(iObj))
		    assumptions.push(-sign * ithX);
	    }

	}
	for(Integer x: this.coveredLiterals.keySet())
	    assumptions.push(x);

	return assumptions;
    }


    private void analyzeDisjointCores(){
	IVecInt currentAssumptions = this.generateUpperBoundAssumptions();
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
     *If necessary for the construction of the current assumptions,
     *initialize more of the domain of the goal delimeter
     */

    private boolean preAssumptionsExtend(IVecInt currentExplanation){

	boolean change = false;
	// Log.comment(0, "covered x variables: " + this.coveredLiterals.size());
	IVecInt currentExplanationX = new VecInt(new int[] {});
	HashMap<Integer,Boolean> objectivesToChange = new HashMap<Integer, Boolean>(this.problem.nObjs());
	for(int lit: currentExplanation.toArray()){
	    int id = this.solver.idFromLiteral(lit);
	    if(this.goalDelimeter.isX(id)){
		currentExplanationX.push(lit);
		for(int iObj = 0; iObj < this.problem.nObjs(); ++iObj){
		    if(this.problem.getObj(iObj).getSubObj(0).weightFromId(id) != null)
			objectivesToChange.put(iObj, null);
		}
	    }
	    else
		objectivesToChange.put(this.goalDelimeter.getIObjFromY(id), null);

	}
	change = this.uncoverXs(currentExplanationX);
	this.updateCurrentMaxValues();
	for(int iObj :objectivesToChange.keySet()){
	    // Log.comment(3, "changing upperlimit " + iObj);
	    int upperKDBefore = this.getUpperKD(iObj);
	    if(this.getUpperKD(iObj) == this.getUpperBound(iObj))
		this.goalDelimeter.generateNext(iObj, this.getUpperKD(iObj), maxValues[iObj]);
	    this.setUpperKD(iObj, this.goalDelimeter.nextKDValue(iObj, this.getUpperKD(iObj)));
	    if(this.getUpperKD(iObj) >= this.getUpperBound(iObj))
		this.goalDelimeter.generateNext(iObj, this.getUpperKD(iObj), maxValues[iObj]);
	    this.setUpperBound(iObj, this.goalDelimeter.nextKDValue(iObj, this.getUpperKD(iObj)));
	    if(this.getUpperKD(iObj)!= upperKDBefore)
		change = true;
	}
	return change;

    }




    /**
     *Updates the current maxValues for each objective
     */

    private void updateCurrentMaxValues(){

	for(int iObj = 0, nObj = this.problem.nObjs();iObj < nObj; iObj++){
	    Objective ithObjective = this.problem.getObj(iObj);
	    ReadOnlyVecInt objectiveLits = ithObjective.getSubObjLits(0);
	    ReadOnlyVec<Real> objectiveCoeffs = ithObjective.getSubObjCoeffs(0);
	    int sign = 1;
	    int ithAbsoluteWeight;
	    maxValues[iObj] = 0; // 
	    for(int iX = 0, nX = ithObjective.getTotalLits(); iX < nX; iX ++){
		int ithX = objectiveLits.get(iX);
		ithAbsoluteWeight = objectiveCoeffs.get(iX).asInt();
		sign = (ithAbsoluteWeight > 0? 1 : -1);
		ithAbsoluteWeight *= sign;
		if(this.coveredLiterals.get(-sign * ithX) == null)
		    maxValues[iObj] += ithAbsoluteWeight;
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
     *gets the current upper bound
     *@param iObj
     */

    private int getUpperBound(int iObj){
	return this.UpperBound[iObj];
    }

    /**
     *Sets the current upper bound of iObj to nowKD
     *@param newKD
     *@param iObj
     */
    private void setUpperBound(int iObj, int newKD){
	    this.UpperBound[iObj] = newKD;
    }

    /**
     * Creates a PB oracle initialized with the MOCO's constraints.
     * @return The oracle.
     * @throws ContradictionException if the oracle detects that the
     * MOCO's constraint set is unsatisfiable.
     */
    private PBSolver buildSolver() throws ContradictionException {
        // Log.comment(5, "in UnsatSat.buildSolver");
        PBSolver solver = new PBSolver();
        solver.newVars(this.problem.nVars());
        for (int i = 0; i < this.problem.nConstrs(); ++i) {
            solver.addConstr(this.problem.getConstr(i));
        }
        // Log.comment(5, "out UnsatSat.buildSolver");
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
	// Log.comment(2, logDiffAttainedValue );
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


    public void printFlightRecordParticular(){
	this.logExhaustedUpperKD();
	Log.comment(2, "covered x variables: " + this.coveredLiterals.size());
	this.logUpperLimit();
	this.logUpperBound();
	this.logMaxValues();

    }
}
