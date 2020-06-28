package org.sat4j.moco.algorithm;

import org.sat4j.moco.analysis.Result;
import org.sat4j.moco.pb.PBFactory;
import org.sat4j.moco.pb.PBSolver;
import org.sat4j.moco.problem.Instance;
import org.sat4j.moco.util.Log;
import org.sat4j.specs.ContradictionException;
import org.sat4j.specs.IVecInt;


abstract public class  algorithm{

    /**
     * An instance of a MOCO problem to be solved.
     */
    protected Instance problem = null;

    /**
     * Stores the result (e.g. nondominated solutions) of the execution of the Pareto-MCS algorithm.
     */
    protected Result result = null;

    /**
     * Stores the partial result (e.g. nondominated solutions) of the execution of the Pareto-MCS algorithm.
     */
    protected Result subResult = null;

    /**
     * Stores the PB solver to be used by the Pareto-MCS algorithm.
     */
    protected PBSolver solver = null;

    /**
     * Retrieves the result of the last call to {@link #solve()}.
     * @return The result.
     */
    public Result getResult() { return this.result; }

    /**
     *Stores any solutions that are in subResult.
     */

    public void moveSubResult() {
	if(this.subResult != null)
	for(int i = 0; i < this.subResult.nSolutions(); ++i)
	    this.result.addSolutionUnsafe(this.subResult.getSolution(i));
    }

    abstract public void prettyPrintVecInt(IVecInt vecInt, boolean clausing);




    /**
     *Adds the disjunction of setOfLiterals, and logs
     *@param setOfliterals
     */

    public boolean AddClause(IVecInt setOfLiterals){
	Log.comment(6, "{ algorithm.AddClause");
	this.prettyPrintVecInt(setOfLiterals,true);
	try{
	    this.solver.AddClause(setOfLiterals);
	} catch (ContradictionException e) {
	    Log.comment(2, "contradiction when adding clause: ");
	    for(int j = 0; j < setOfLiterals.size(); ++j)
		Log.comment(2, " " + setOfLiterals.get(j) + " " );
	    return false;
	}
	Log.comment(6, "}");
	return true;
    }


    public void printFlightRecord(){
	Log.comment("constraints: " + this.solver.getClausesN());
	Log.comment("Variables: " + this.solver.nVars());
    }
    abstract public void printFlightRecordParticular();
}
