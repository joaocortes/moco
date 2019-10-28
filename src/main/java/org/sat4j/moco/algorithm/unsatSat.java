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

import java.util.Arrays;
import java.util.Vector;

import org.moeaframework.core.PRNG;
import org.sat4j.core.ReadOnlyVec;
import org.sat4j.core.ReadOnlyVecInt;
import org.sat4j.core.Vec;
import org.sat4j.core.VecInt;
import org.sat4j.moco.Params;
import org.sat4j.moco.pb.ConstrID;
import org.sat4j.moco.analysis.Result;
import org.sat4j.moco.mcs.IModelListener;
import org.sat4j.moco.mcs.MCSExtractor;
import org.sat4j.moco.pb.PBExpr;
import org.sat4j.moco.pb.PBFactory;
import org.sat4j.moco.pb.PBSolver;
import org.sat4j.moco.problem.Instance;
import org.sat4j.moco.problem.SeqEncoder;
import org.sat4j.moco.problem.Objective;
import org.sat4j.moco.util.Log;
import org.sat4j.moco.util.Real;
import org.sat4j.specs.ContradictionException;
import org.sat4j.specs.IVec;
import org.sat4j.specs.IVecInt;

/**
 * Class that implements unsatSat
 * @author João Cortes
 */

public class unsatSat {

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
     *  Last id of real, non auxiliary,  variables 
     */  
    private int realVariablesN = 0;


    /**
     * Creates an instance of a MOCO solver, for a given instance,
     * that applies the Pareto-MCS algorithm.
     * @param m The MOCO instance.
     */
    public unsatSat(Instance m) {
	this.problem = m;
        this.result = new Result(m);
	try {
            this.solver = buildSolver();
	    System.out.println(this.solver.nVars());
        }
        catch (ContradictionException e) {
            Log.comment(3, "Contradiction in ParetoMCS.buildSolver");
            this.result.setParetoFrontFound();
            return;
        }
	this.realVariablesN = this.solver.nVars();
	this.seqEncoder = new SeqEncoder(this.problem,this.solver);


    }

    
    
    /**
     * Applies the unsatSat algorithm to the MOCO instance provided
      * in {@link #ParetoMCS(Instance)}.  If the instance has already
      * been solved, nothing happens.
     */

    public void solve() {
	IVecInt currentExplanation = new VecInt(new int[] {});
	IVecInt currentAssumptions = new VecInt(new int[] {});
	Vector<IVecInt> models = new Vector<IVecInt>();
	ConstrID lastLessThan1 = null;

        if (this.result.isParetoFront()) {
            Log.comment(1, "unsatSat.solve called on already solved instance");
            return;
        }
        Log.comment(3, "in unsatSat.solve");

	this.initialExtend();
	while(true){
	    currentAssumptions = this.seqEncoder.generateUpperBoundAssumptions();
	    solver.check(currentAssumptions);
	    if(solver.isSat()){
		models.add(this.getSemiFilteredModel());
		this.seqEncoder.blockDominatedRegion(models.lastElement());
	    }else{
		currentExplanation  = solver.unsatExplanation();
		if(currentExplanation.size() == 0){
		    this.printModels(models);
		    return;
		}
		this.updateUpperBound(currentExplanation);
		lastLessThan1 = this.swapLessThan1Clause(lastLessThan1);
	    }
	}
		
    }

    /**
     *Initial extend. Extends the S (and Y) variables of the
     *sequential encoder, in order to allow the construction of the
     *first set of assumptions
     */

    private void initialExtend(){
	int objN = this.problem.nObjs();
	for(int iObj = 0; iObj < objN ; ++iObj){
	this.seqEncoder.extendUpperIdsSInK(iObj, 1);
	}
    }


    /**
     * Updates the current upperBound on the differential k, according
     * to the unsatExplanation
     * @param current explanation of unsatisfiability
     */
    private void updateUpperBound(IVecInt currentExplanation){
	for(int i = 0; i < currentExplanation.size(); ++i){
	    int ithLiteral = currentExplanation.get(i);
	    assert this.seqEncoder.isY(ithLiteral);
	    if(currentExplanation.get(ithLiteral) > 0){
		int jObj = this.seqEncoder.getObjFromYVariable(ithLiteral);
		int dK = this.seqEncoder.getDKfromYVariable(ithLiteral);
		this.seqEncoder.setCurrentUpperDK(jObj, dK);
		// necessary for the construction of the next set of assumptions
		this.seqEncoder.extendUpperIdsSInK(jObj, dK + 1);
	    }}
    }
    
    /**
     * Creates a PB oracle initialized with the MOCO's constraints.
     * @return The oracle.
     * @throws ContradictionException if the oracle detects that the
     * MOCO's constraint set is unsatisfiable.
     */
    private PBSolver buildSolver() throws ContradictionException {
        Log.comment(3, "in ParetoMCS.buildSolver");
        PBSolver solver = new PBSolver();
        solver.newVars(this.problem.nVars());
        for (int i = 0; i < this.problem.nConstrs(); ++i) {
            solver.addConstr(this.problem.getConstr(i));
        }
        Log.comment(3, "out ParetoMCS.buildSolver");
        return solver;
    }

    private ConstrID swapLessThan1Clause(ConstrID lastLessThan1){
	IVecInt literals = new VecInt(new int[]{});
	for(int iObj = 0; iObj < this.problem.nObjs(); ++iObj){
	    int nLitsIthObj =   this.problem.getObj(iObj).getTotalLits();
	    literals.push(this.seqEncoder.getY(iObj,
					      this.seqEncoder.getDK(iObj)));
	}
	if(lastLessThan1 != null) this.solver.removeConstr(lastLessThan1);
	return this.solver.addRemovableConstr(PBFactory.instance().mkLE(literals, 1));
    }



    /**
     * Builds a partition sequence of the literals in the objective functions to be used for stratified
     * MCS extraction.
     * If stratification is disabled, a single partition is returned.
     * @return The objective literals partition sequence.
     */
    private IVec<IVecInt> buildUndefFmls() {
        Log.comment(3, "in ParetoMCS.buildUndefFmls");
        IVec<IVecInt> fmls = new Vec<IVecInt>();
        IVec<IVec<IVecInt>> p_stacks = new Vec<IVec<IVecInt>>(this.undef_parts.size());
        for (int i = 0; i < this.undef_parts.size(); ++i) {
            IVec<IVecInt> parts = this.undef_parts.get(i);
            IVec<IVecInt> p_stack = new Vec<IVecInt>(parts.size());
            for (int j = parts.size()-1; j >= 0; --j) {
                p_stack.unsafePush(parts.get(j));
            }
            p_stacks.unsafePush(p_stack);
        }
        while (!p_stacks.isEmpty()) {
            int rand_i = PRNG.nextInt(p_stacks.size());
            IVec<IVecInt> rand_stack = p_stacks.get(rand_i);
            fmls.push(new ReadOnlyVecInt(rand_stack.last()));
            rand_stack.pop();
            if (rand_stack.isEmpty()) {
                p_stacks.set(rand_i, p_stacks.last());
                p_stacks.pop();
            }
        }
        Log.comment(3, "out ParetoMCS.buildUndefFmls");
        return fmls;
    }
    
    /**
     * Representation of weighted literals.  Used to store and sort
     * literals based on their coefficients in some objective
     * function.
     * @author Miguel Terra-Neves
     */
    private class WeightedLit implements Comparable<WeightedLit> {

        /**
         * Stores the literal.
         */
        private int lit = 0;
        
        /**
         * Stores the weight.
         */
        private Real weight = Real.ZERO;
        
        /**
         * Creates an instance of a weighted literal with a given weight.
         * @param l The literal.
         * @param w The weight.
         */
        WeightedLit(int l, Real w) {
            this.lit = l;
            this.weight = w;
        }
        
        /**
         * Retrieves the literal part of the weighted literal.
         * @return The literal.
         */
        int getLit() { return this.lit; }
        
        /**
         * Retrieves the weight part of the weighted literal.
         * @return The weight.
         */
        Real getWeight() { return this.weight; }
        
        /**
         * Compares the weighted literal to another weighted literal.
         * The weighted literal order is entailed by their weights.
         * @param other The other weighted literal.
         * @return An integer smaller than 0 if this literal's weight
         * is smaller than {@code other}'s, 0 if the weight are equal,
         * an integer greater than 0 if this literal's weight is
         * larger than {@code other}'s.
         */
        public int compareTo(WeightedLit other) {
            return getWeight().compareTo(other.getWeight());
        }
        
    }
    
    
    
    /**
     * Retrieves the literals and respective coefficients in an
     * objective function as a vector of weighted literals.
     * @param o The objective.
     * @return The objective's literals and coefficients as weighted
     * literals.
     */
    private IVec<WeightedLit> getWeightedLits(Objective o) {
        IVec<WeightedLit> w_lits = new Vec<WeightedLit>();
        for (int i = 0; i < o.nSubObj(); ++i) {
            ReadOnlyVecInt lits = o.getSubObjLits(i);
            ReadOnlyVec<Real> coeffs = o.getSubObjCoeffs(i);
            for (int j = 0; j < lits.size(); ++j) {
                int lit = lits.get(j);
                Real coeff = coeffs.get(j);
                if (coeff.isPositive()) {
                    w_lits.push(new WeightedLit(lit, coeff));
                }
                else if (coeff.isNegative()) {
                    w_lits.push(new WeightedLit(-lit, coeff.negate()));
                }
                else {
                    Log.comment(2, "0 coefficient ignored");
                }
            }
        }
        return w_lits;
    }

    /**
     *returns the model in DIMACS format, including only the real
     *variables and the Y variables of the sequential encoder
     *@return a filtered model
     */

    public IVecInt getSemiFilteredModel(){
	IVecInt model = new VecInt(new int[] {});
	for(int id = 1; id <= this.solver.nVars();++id){
	    int literal = (this.solver.modelValue(id))? id: -id;
	    if(id <= this.realVariablesN)
		model.push(literal);
	    else
		if(this.seqEncoder.isY(id))
		    model.push(literal);
	}
	return model;
    }
    /** 
     * Sets the algorithm configuration to the one stored in a given set of parameters.
     * @param p The parameters object.
     */
    public void printModels(Vector<IVecInt> models) {
	for(int i = 0; i <models.size(); ++i)
	    for(int j = 0; j <models.get(i).size(); ++j)
		System.out.print(models.get(i).get(j) + " ");
	return;
    }
    
}
    /**
     * Retrieves the result of the last call to {@link #solve()}.
     * @return The result.
     */
    public Result getResult() { return this.result; }
