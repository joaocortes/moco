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
import java.util.Hashtable;
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
    // private Result result = null;
    
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
	try {
            this.solver = buildSolver();
        }
        catch (ContradictionException e) {
            Log.comment(3, "Contradiction in ParetoMCS.buildSolver");
            return;
        }
	this.realVariablesN = this.solver.nVars();
	this.seqEncoder = new SeqEncoder(this.problem,this.solver);
    }

    
    
    /**
     * Applies the UnsatSat algorithm to the MOCO instance provided
     */

    public void solve() {
	IVecInt currentExplanation = new VecInt(new int[] {});
	IVecInt currentAssumptions = new VecInt(new int[] {});
	Vector<IVecInt> models = new Vector<IVecInt>();
	ConstrID lastLessThan1 = null;
	this.UpperKD =  new int[(this.problem.nObjs())];
        // if (this.result.isParetoFront()) {
        //     Log.comment(1, "UnsatSat.solve called on already solved instance");
        //     return;
        // }
        Log.comment(3, "in UnsatSat.solve");
	boolean goOn = true;
	while(goOn){
	    System.out.print("upper limit:");
	    System.out.print("["+this.getUpperKD(0));
	    for(int iObj = 1; iObj < this.problem.nObjs(); ++iObj)
		System.out.print(", "+this.getUpperKD(iObj));
	    System.out.println("]");

	    this.preAssumptionsExtend();
	    currentAssumptions = this.generateUpperBoundAssumptions();

	    System.out.println("Checking against assumptions:");
	    this.seqEncoder.prettyPrintVecInt(currentAssumptions);

	    solver.check(currentAssumptions);

	    if(solver.isSat()){
		models.add(this.getSemiFilteredModel());

		System.out.println("Model :");
		this.printModel(models.lastElement());
		System.out.println("Blocking dominated region");

		if(! this.blockDominatedRegion(models.lastElement()))
		    goOn = false;
	    }else{
		currentExplanation  = solver.unsatExplanation();

		System.out.println("Explanation:");
		this.seqEncoder.prettyPrintVecInt(currentExplanation);

		if(currentExplanation.size() == 0){
		    goOn = false;
		}else{
		    // System.out.println("UpperBound extend");
		    this.updateUpperBound(currentExplanation);
		    lastLessThan1 = this.swapLessThan1Clause(lastLessThan1);
		}


	    }


	}
	//	this.printModels(models);


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
	    //TODO only if kd is not initialized already
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
        Log.comment(3, "in UnsatSat.buildSolver");
        PBSolver solver = new PBSolver();
        solver.newVars(this.problem.nVars());
        for (int i = 0; i < this.problem.nConstrs(); ++i) {
            solver.addConstr(this.problem.getConstr(i));
        }
        Log.comment(3, "out UnsatSat.buildSolver");
        return solver;
    }

    private ConstrID swapLessThan1Clause(ConstrID lastLessThan1){
	IVecInt literals = new VecInt(new int[]{});
	for(int iObj = 0; iObj < this.problem.nObjs(); ++iObj){
	    literals.push(this.seqEncoder.getSTop(iObj,
						  this.getUpperKD(iObj)));
	}
	System.out.println("Swaping lessThan1");
	this.seqEncoder.prettyPrintVecInt(literals);
	if(lastLessThan1 != null) this.solver.removeConstr(lastLessThan1);
	try{	return this.solver.addRemovableConstr(PBFactory.instance().mkLE(literals, 1));
	} catch(ContradictionException e){
	    System.out.print("Contradiction when adding the less than 1 restriction");}
	return null;
    }


    
    // /**
    //  * Representation of weighted literals.  Used to store and sort
    //  * literals based on their coefficients in some objective
    //  * function.
    //  * @author Miguel Terra-Neves
    //  */
    // private class WeightedLit implements Comparable<WeightedLit> {

    //     /**
    //      * Stores the literal.
    //      */
    //     private int lit = 0;
        
    //     /**
    //      * Stores the weight.
    //      */
    //     private Real weight = Real.ZERO;
        
    //     /**
    //      * Creates an instance of a weighted literal with a given weight.
    //      * @param l The literal.
    //      * @param w The weight.
    //      */
    //     WeightedLit(int l, Real w) {
    //         this.lit = l;
    //         this.weight = w;
    //     }
        
    //     /**
    //      * Retrieves the literal part of the weighted literal.
    //      * @return The literal.
    //      */
    //     int getLit() { return this.lit; }
        
    //     /**
    //      * Retrieves the weight part of the weighted literal.
    //      * @return The weight.
    //      */
    //     Real getWeight() { return this.weight; }
        
    //     /**
    //      * Compares the weighted literal to another weighted literal.
    //      * The weighted literal order is entailed by their weights.
    //      * @param other The other weighted literal.
    //      * @return An integer smaller than 0 if this literal's weight
    //      * is smaller than {@code other}'s, 0 if the weight are equal,
    //      * an integer greater than 0 if this literal's weight is
    //      * larger than {@code other}'s.
    //      */
    //     public int compareTo(WeightedLit other) {
    //         return getWeight().compareTo(other.getWeight());
    //     }
        
    // }
    
    
    
    // /**
    //  * Retrieves the literals and respective coefficients in an
    //  * objective function as a vector of weighted literals.
    //  * @param o The objective.
    //  * @return The objective's literals and coefficients as weighted
    //  * literals.
    //  */
    // private IVec<WeightedLit> getWeightedLits(Objective o) {
    //     IVec<WeightedLit> w_lits = new Vec<WeightedLit>();
    //     for (int i = 0; i < o.nSubObj(); ++i) {
    //         ReadOnlyVecInt lits = o.getSubObjLits(i);
    //         ReadOnlyVec<Real> coeffs = o.getSubObjCoeffs(i);
    //         for (int j = 0; j < lits.size(); ++j) {
    //             int lit = lits.get(j);
    //             Real coeff = coeffs.get(j);
    //             if (coeff.isPositive()) {
    //                 w_lits.push(new WeightedLit(lit, coeff));
    //             }
    //             else if (coeff.isNegative()) {
    //                 w_lits.push(new WeightedLit(-lit, coeff.negate()));
    //             }
    //             else {
    //                 Log.comment(2, "0 coefficient ignored");
    //             }
    //         }
    //     }
    //     return w_lits;
    // }

    /**
     *returns the model in DIMACS format, including only the real
     *variables and the STop variables of the sequential encoder
     *@return a filtered model
     */

    public IVecInt getSemiFilteredModel(){
	IVecInt model = new VecInt(new int[] {});
	for(int id = 1; id <= this.solver.nVars();++id){
	    int literal = (this.solver.modelValue(id))? id: -id;
	    if(id <= this.realVariablesN && id >= 1)
		model.push(literal);
	    else
		if(this.seqEncoder.isSTop(id))
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
	System.out.println("Model ");
	System.out.print("j o ");
	for(int iObj = 0; iObj < this.problem.nObjs(); ++iObj){
	    Objective ithObj = this.problem.getObj(iObj);
	    System.out.print(this.attainedValue(ithObj, model)+ " " );
	}
	System.out.println();
	for(int j = 0; j <model.size(); ++j)
	    this.seqEncoder.prettyPrintVariable(model.get(j));
	System.out.println();


	return;
    }
    
 public boolean isYFrontier(int literal){
     int iObj = this.seqEncoder.getObjFromSTopVariable(literal);
     int kD = this.seqEncoder.getKDFromSTopVariable(literal);
     if(kD == this.getUpperKD(iObj))
	 return true;
     return false;
    }

    public boolean isX(int literal){
	int sign = (literal > 0)? 1: -1;
	if(sign * literal <= this.problem.nVars())
	    if(1 <= sign * literal)
		return true;
	return false;
    }

    /**
     * The attained value of objective  in the interpretation of model 
     @param model
     @param iObj
    */
    private int attainedValue(Objective objective , IVecInt model){
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

    public boolean blockDominatedRegion(IVecInt newSolution){
	int[] upperLimits = new int[this.problem.nObjs()];
	for(int i = 0; i < newSolution.size(); ++i){
	    int literal = newSolution.get(i);
	    if(this.seqEncoder.isSTop(literal))
		if(literal > 0){
		    int iObj = this.seqEncoder.getObjFromSTopVariable(literal);
		    int kDPotencial = this.seqEncoder.getKDFromSTopVariable(literal);
		    int kDCurrent = upperLimits[iObj];
		    if(kDCurrent < kDPotencial)
			upperLimits[iObj] = kDPotencial;
		}
	}
	int[] literals = new int[this.problem.nObjs()];
	for (int iObj = 0; iObj < this.problem.nObjs(); ++iObj)
	    literals[iObj] = -this.seqEncoder.getSTop(iObj, upperLimits[iObj]);
	IVecInt newHardClause = new VecInt(literals);
	return this.AddClause(newHardClause);
    }
    
    private boolean AddClause(IVecInt setOfLiterals){
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
	    return false;
	}
	return true;
    }
    public Result getResult(){
	return null;
    }    
}

