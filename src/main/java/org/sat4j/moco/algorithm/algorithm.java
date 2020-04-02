package org.sat4j.moco.algorithm;

import org.sat4j.moco.analysis.Result;
import org.sat4j.moco.pb.PBSolver;
import org.sat4j.moco.problem.Instance;
import org.sat4j.moco.util.Log;


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

    public void printFlightRecord(){
	Log.comment(1, "aux constraints: " + this.auxiliarConstraintsN);
	Log.comment(1, "aux Variables: " + this.auxiliarVariablesN);
    };

}
