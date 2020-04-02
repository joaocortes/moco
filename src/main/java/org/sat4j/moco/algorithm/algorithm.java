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
     * Retrieves the result of the last call to {@link #solve()}.
     * @return The result.
     */
    public Result getResult() { return this.result; }
}
