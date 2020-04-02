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
     * Stores the PB solver to be used by the Pareto-MCS algorithm.
     */
    protected PBSolver solver = null;

    /**
     * the number of auxiliar constraints added
     */
    protected int auxiliarConstraintsN = 0;

    /**
     * the number of auxiliar variables added
     */
    protected int auxiliarVariablesN = 0;


    /**
     * Retrieves the result of the last call to {@link #solve()}.
     * @return The result.
     */
    public Result getResult() { return this.result; }

    abstract public void prettyPrintVecInt(IVecInt vecInt, boolean clausing);

    /**
     *Adds the disjunction of setOfLiterals
     *@param setOfliterals
     */
    public boolean AddClause(IVecInt setOfLiterals){
	this.prettyPrintVecInt(setOfLiterals,true);
	for(int i = 0; i < setOfLiterals.size(); ++i)
	try{
	    this.solver.addConstr(PBFactory.instance().mkClause(setOfLiterals));
	} catch (ContradictionException e) {
	    Log.comment(6, "contradiction when adding clause: ");
	    for(int j = 0; j < setOfLiterals.size(); ++j)
		Log.comment(3, " " + setOfLiterals.get(j) + " " );
	    return false;
	}
	return true;
    }

    public void printFlightRecord(){
	Log.comment(1, "aux constraints: " + this.auxiliarConstraintsN);
	Log.comment(1, "aux Variables: " + this.auxiliarVariablesN);
    };

}
