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
import org.sat4j.core.VecInt;
import org.sat4j.moco.pb.ConstrID;
import org.sat4j.core.ReadOnlyVec;
import org.sat4j.core.ReadOnlyVecInt;
import org.sat4j.moco.analysis.Result;
import org.sat4j.moco.util.Real;
import org.sat4j.moco.pb.PBFactory;
import org.sat4j.moco.pb.PBSolver;
import org.sat4j.moco.problem.Instance;
import org.sat4j.moco.problem.Objective;
import org.sat4j.moco.problem.SeqEncoder;
import org.sat4j.moco.util.Log;
import org.sat4j.specs.ContradictionException;
import org.sat4j.specs.IVecInt;

/**
 * Class that implements UnsatSat
 * @author João Cortes
 */

public class UnsatSat {

    /**
     * An instance of a MOCO problem to be solved.
     */
    private Instance problem = null;
    
    /**
     * Stores the result (e.g. nondominated solutions) of the execution of the Pareto-MCS algorithm.
     */
    private Result result = null;
    
    /**
     * Stores the PB solver to be used by the Pareto-MCS algorithm.
     */
    private PBSolver solver = null;
    

    /**
     * IDs of the variables used int the sequential encoder. The first
     * index is the goal, the second is the first index of s from " On
     * using Incremental Encodings...".Remember that s(i,j) is an
     * indicator of the propositions of the form x_i>=j.
     */

    private SeqEncoder seqEncoder = null;

    /**
     * Last explored differential k, for each objective function.
     */
    private int[] UpperKD = null;
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
	this.problem = m;
	this.result = new Result(m);
	try {
            this.solver = buildSolver();
        }
        catch (ContradictionException e) {
            Log.comment(3, "Contradiction in ParetoMCS.buildSolver");
            return;
        }
	this.realVariablesN = this.solver.nVars();
	this.seqEncoder = new SeqEncoder(this.problem,this.solver);
	this.UpperKD =  new int[(this.problem.nObjs())];
    }

    
    
    /**
     * Applies the UnsatSat algorithm to the MOCO instance provided
     */

    public void solve() {
	IVecInt currentExplanation = new VecInt(new int[] {});
	IVecInt currentAssumptions = new VecInt(new int[] {});
	IVecInt currentYModel = new VecInt(new int[] {});
	boolean[] currentXModelValues = new boolean[this.problem.nVars()];
	Result subResult = new Result(this.problem);	    


        // if (this.result.isParetoFront()) {
        //     Log.comment(1, "UnsatSat.solve called on already solved instance");
        //     return;
        // }
        Log.comment(3, "in UnsatSat.solve");
	boolean goOn = true;
	//for testing purposes
	//	this.seqEncoder.UpdateCurrentK(0, 2);
	while(goOn){
	    ///log..
	    Log.comment(5, "upper limit:");
	    Log.comment(5, "["+this.getUpperKD(0));
	    for(int iObj = 1; iObj < this.problem.nObjs(); ++iObj)
		Log.comment(5, ", "+this.getUpperKD(iObj));
	    Log.comment(5, "]");
	    //..log
	    this.preAssumptionsExtend();
	    currentAssumptions = this.generateUpperBoundAssumptions();

	    //log..
	    Log.comment(5, "Checking against assumptions:");
	    this.seqEncoder.prettyPrintVecInt(currentAssumptions);
	    //..log

	    solver.check(currentAssumptions);

	    if(solver.isSat()){
		subResult.saveModel(this.solver);
		//log
		Log.comment(5, " current subResult size:" + subResult.nSolutions());

		currentYModel = this.getYModel();
		currentXModelValues = this.getXModelValues();

		//log..
		Log.comment(5, "ModelX :");
		this.printModel(modelsX.lastElement());
		Log.comment(5, "j o ");
		for(int iObj = 0; iObj < this.problem.nObjs(); ++iObj){
		    Objective ithObj = this.problem.getObj(iObj);
		    Log.comment(5, this.attainedValue(ithObj)+ " " );
		}
		Log.comment(5, "ModelY :");
		this.printModel(modelsY.lastElement());
		//..log

		//log
		Log.comment(5, "Blocking dominated region");

		if(! this.blockDominatedRegion(modelsY.lastElement()))
		    goOn = false;
		// if(! this.blockModelX(modelsX.lastElement()))
		//     goOn = false;
	    }else{
		subResult = new Result(this.problem);	    
		currentExplanation  = solver.unsatExplanation();
		//log..
		Log.comment(5, "Explanation:");
		this.seqEncoder.prettyPrintVecInt(currentExplanation);
		//..log

		if(currentExplanation.size() == 0){
		    goOn = false;
		}else{
		    this.updateUpperBound(currentExplanation);
		}
	    }
	}
	this.result.setParetoFrontFound();
	return;
    }
    

    /**
     * Generate the upper limit assumptions
     */
    public IVecInt generateUpperBoundAssumptions( ){
	IVecInt assumptions = new VecInt(new int[]{});
	for(int iObj = 0; iObj < this.problem.nObjs(); ++iObj){
	    assumptions.push(-this.seqEncoder.getSTop(iObj, this.getUpperKD(iObj) + 1));
	}
	return assumptions;
    }


    /**
     *If necessary for the construction of the current assumptions,
     *initialize more of the domain of the sequential encoder
     *differential k index
     */

    private void preAssumptionsExtend(){
	int objN = this.problem.nObjs();
	for(int iObj = 0; iObj < objN ; ++iObj){
	    this.seqEncoder.UpdateCurrentK(iObj, this.getUpperKD(iObj) + 1);
	}
    }


    /**
     * Updates the current upperBound on the differential k, according
     * to the unsatExplanation, and updates the sequentialEncoder accordingly
     * @param current explanation of unsatisfiability
     */
    private void updateUpperBound(IVecInt currentExplanation){
	for(int i = 0; i < currentExplanation.size(); ++i){
	    int ithLiteral = currentExplanation.get(i);
	    int jObj = this.seqEncoder.getObjFromSTopVariable(ithLiteral);
	    int kd = this.seqEncoder.getKDFromSTopVariable(ithLiteral);
	    //TODO This is strange, and should not be always true
	    assert kd ==this.getUpperKD(jObj);

		this.setUpperKD(jObj, kd);
		this.seqEncoder.UpdateCurrentK(jObj, kd);
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
	if(this.seqEncoder.getCurrentKD(iObj) < newKD)
	    this.seqEncoder.UpdateCurrentK(iObj, newKD);
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
     *Checks if literal is an STop variable
     *@param literal
     */

    public boolean isY(int literal){
	if(this.seqEncoder.isSTop(literal))
	    return true;
	return false;
    }
    /**
     *Checks if literal is an STop variable
     *@param literal
     */

    public boolean isX(int literal){
	int id = (literal>0)? literal: -literal;
	return id <= this.realVariablesN && id >= 1;

    }

    /**
     *returns the model in DIMACS format, including only the real
     *variables and the STop variables of the sequential encoder
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
     *variables and the STop variables of the sequential encoder
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
     *variables and the STop variables of the sequential encoder
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

    /** 
     * Print a model 
     * @param models, the obtained models
     */
    public void printModel(IVecInt model) {
	for(int j = 0; j <model.size(); ++j)
	    this.seqEncoder.prettyPrintVariable(model.get(j));


	return;
    }

    /**
     * The attained value of objective  in the interpretation of model 
     @param model
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

    /**
     * Block the region dominated by the known models.
     */

    public int[] findUpperLimits(IVecInt newSolution){
	int[] upperLimits = new int[this.problem.nObjs()];
	for(int i = 0; i < this.problem.nObjs(); ++i){
	    upperLimits[i] = this.attainedValue(this.problem.getObj(i));
	    upperLimits[i]-=this.problem.getObj(i).getMinValue();
	}
	return upperLimits;
    }

    public boolean blockDominatedRegion(IVecInt newSolution){
	int[] upperLimits = this.findUpperLimits(newSolution);
	int[] literals = new int[this.problem.nObjs()];
	for (int iObj = 0; iObj < this.problem.nObjs(); ++iObj)
	    literals[iObj] = -this.seqEncoder.getSTop(iObj, upperLimits[iObj]);
	IVecInt newHardClause = new VecInt(literals);
	return this.AddClause(newHardClause);
    }

    public boolean blockModelX(IVecInt modelX){
	IVecInt notPreviousModel = new VecInt(new int[] {});
	for(int iX = 0; iX < modelX.size(); ++iX)
	    notPreviousModel.push(-modelX.get(iX));
	return this.AddClause(notPreviousModel);
    }
    
    private boolean AddClause(IVecInt setOfLiterals){
	for(int i = 0; i < setOfLiterals.size(); ++i)
	    this.seqEncoder.prettyPrintVariable(setOfLiterals.get(i));
	try{
	    this.solver.addConstr(PBFactory.instance().mkClause(setOfLiterals));
	}
	catch (ContradictionException e) {
	    Log.comment(5, "contradiction when adding clause: ");
	    for(int j = 0; j < setOfLiterals.size(); ++j)
		this.seqEncoder.prettyPrintVariable(setOfLiterals.get(j));
	    return false;
	}
	return true;
    }

    /**
     * Retrieves the result of the last call to {@link #solve()}.
     * @return The result.
     */
    public Result getResult() { return this.result; }
    
}

