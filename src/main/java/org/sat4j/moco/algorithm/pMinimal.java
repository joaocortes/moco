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
 * Class that implements the p-minimal inspired algorithm
 * @author João Cortes
 */

public class pMinimal {

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
    
    public pMinimal(Instance m) {
	Log.comment(5, "In pMinimal.pMinimal");
	this.problem = m;
	this.result = new Result(m);
	try {
            this.solver = buildSolver();
        }
        catch (ContradictionException e) {
            Log.comment(3, "Contradiction in ParetoMCS.buildSolver");
	    Log.comment(5, "done");
            return;
        }
	this.realVariablesN = this.solver.nVars();
	this.seqEncoder = new SeqEncoder(this.problem,this.solver);
	this.UpperKD =  new int[(this.problem.nObjs())];
	for(int iObj = 0, nObj = this.problem.nObjs(); iObj < nObj; ++iObj)
	    this.setUpperKD(iObj);
	    Log.comment(5, "done");
    }

    
    
    /**
     * Applies the p-minimal algorithm to the MOCO instance provided
     */

    public void solve() {
	IVecInt currentYModel = new VecInt(new int[] {});
	// IVecInt currentXModel = new VecInt(new int[] {});
	boolean[] currentXModelValues = new boolean[this.problem.nVars()];
	IVecInt assumptions = new VecInt(new int[] {});
	boolean sat = true;
        Log.comment(3, "in pMinimal.solve");
	this.solver.check(assumptions);
	sat = this.solver.isSat();
	while(sat){
	    while(sat){		
		currentYModel = this.getYModel();
		currentXModelValues = this.getXModelValues();
		this.setAssumptions(assumptions, currentYModel, currentXModelValues);
		// this.blockModelX(currentXModel);
		this.blockDominatedRegion(currentXModelValues);
		this.solver.check(assumptions);
		sat = this.solver.isSat();
		Log.comment(3, "ali");
	    }
	    Log.comment(3, "aqui");
	    assumptions = new VecInt(new int[] {});
	    this.result.saveThisModel(currentXModelValues);
	    this.solver.check();
	    sat = this.solver.isSat();
	    if(sat)
		Log.comment(3, "Is sat after check with no assumptions");
	    if(sat)
	    	sat = this.blockDominatedRegion(currentXModelValues);
	    // this.blockModelX(currentXModel);
	}
	this.result.setParetoFrontFound();
    }




    private void setAssumptions(IVecInt assumptions, IVecInt yModel,boolean[] XModelValues){
	int[] upperLimits = this.findUpperLimits(XModelValues);
	int literal, id;
	int currentKD, currentIObj;
	for(int i = 0, n = yModel.size(); i < n ; ++i){
	    literal = yModel.get(i);
	    boolean literalValue = this.solver.isLiteralPositive(literal);
	    id =  this.solver.idFromLiteral(literal);
	    if(literalValue){
		currentKD = this.seqEncoder.getKDFromSTopVariable(literal);
		currentIObj = this.seqEncoder.getObjFromSTopVariable(literal);
		if( currentKD < upperLimits[currentIObj])
		    assumptions.push(-id);
	    }
	}
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
     *@param iObj
     */
    private void setUpperKD(int iObj){
	int newKD = this.problem.getObj(iObj).getWeightDiff();
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
        Log.comment(3, "in UnsatSat.buildSolver");
        PBSolver solver = new PBSolver();
        solver.newVars(this.problem.nVars());
        for (int i = 0; i < this.problem.nConstrs(); ++i) {
            solver.addConstr(this.problem.getConstr(i));
        }
        Log.comment(3, "out UnsatSat.buildSolver");
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

    public boolean[] getXModelValues(){
	boolean[] modelValues = new boolean[this.problem.nVars()];
	for(int id = 1; id <= this.problem.nVars();++id){
	    modelValues[id - 1] = this.solver.modelValue(id);
	}
	return modelValues;
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
	    System.out.println("Model " + i);
	    this.printModel(models.get(i));
	    System.out.println();
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
	System.out.println();


	return;
    }

    /**
     * The attained value of objective  in the interpretation of model 
     @param model
    */
    private int attainedValue(Objective objective, boolean[] XModelValues){
	Log.comment(5, "In pMinimal.attainedValue");
	int result = 0;
	int objectiveNLit = objective.getTotalLits();
	ReadOnlyVecInt objectiveLits = objective.getSubObjLits(0);
	ReadOnlyVec<Real> objectiveCoeffs = objective.getSubObjCoeffs(0);
	for(int iLit = 0; iLit < objectiveNLit; ++iLit  ){
	    int coeff = objectiveCoeffs.get(iLit).asInt();
	    int literal = objectiveLits.get(iLit);
	    int id = this.solver.idFromLiteral(literal);
	    if(XModelValues[id - 1])
		result += coeff;
	}
	Log.comment(3, "attained value:" + result);
	Log.comment(5, "done");
	return result;
    }
    /**
     * Block the region dominated by the known models.
     */

    public int[] findUpperLimits(boolean[] XModelValues){
	Log.comment(5, "In pMinimal.findUpperLimits");
	int[] upperLimits = new int[this.problem.nObjs()];
	for(int i = 0; i < this.problem.nObjs(); ++i){
	    upperLimits[i] = this.attainedValue(this.problem.getObj(i), XModelValues);
	    upperLimits[i]-=this.problem.getObj(i).getMinValue();
	}
	Log.comment(5, "done");
	return upperLimits;
    }

    public boolean blockDominatedRegion(boolean[] XModelValues){
	Log.comment(5, "in pMinimal.blockDominatedregion");
	int[] upperLimits = this.findUpperLimits(XModelValues);
	int[] literals = new int[this.problem.nObjs()];
	for (int iObj = 0; iObj < this.problem.nObjs(); ++iObj)
	    literals[iObj] = -this.seqEncoder.getSTop(iObj, upperLimits[iObj]);
	IVecInt newHardClause = new VecInt(literals);
	Log.comment(5, "done");
	return this.AddClause(newHardClause);
    }


    public boolean blockModelX(IVecInt modelX){
	IVecInt notPreviousModel = new VecInt(new int[] {});
	for(int iX = 0; iX < modelX.size(); ++iX)
	    notPreviousModel.push(-modelX.get(iX));
	return this.AddClause(notPreviousModel);
    }
    
    private boolean AddClause(IVecInt setOfLiterals){
	Log.comment(5, "In pMinimal.AddClause");
	for(int i = 0; i < setOfLiterals.size(); ++i)
	    this.seqEncoder.prettyPrintVariable(setOfLiterals.get(i));
	System.out.println();
	try{
	    this.solver.addConstr(PBFactory.instance().mkClause(setOfLiterals));
	}
	catch (ContradictionException e) {
	    System.out.println("contradiction when adding clause: ");
	    for(int j = 0; j < setOfLiterals.size(); ++j)
		this.seqEncoder.prettyPrintVariable(setOfLiterals.get(j));
	    System.out.println();
	    Log.comment(5, "done");
	    return false;
	}
	    Log.comment(5, "done");
	return true;
    }

    /**
     * Retrieves the result of the last call to {@link #solve()}.
     * @return The result.
     */
    public Result getResult() { return this.result; }
    
}

