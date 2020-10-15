package org.sat4j.moco.goal_delimeter;

import org.sat4j.moco.util.Log;
import org.sat4j.specs.IVecInt;


import org.sat4j.moco.pb.PBSolver;
import org.sat4j.moco.problem.Instance;


import org.sat4j.specs.ContradictionException;


public abstract class GoalDelimeter<PIndex extends Index> implements GoalDelimeterI{

    /**
     * the instance to be solved
     */
    protected Instance instance = null;
    /**
     * the solver being used
     */
    protected PBSolver solver = null;

    protected Librarian<PIndex> librarian = null;

    /**
     *First solver variable that pertains to the goal delimeter
     *encoding
     */

    protected int firstVariable = 0;

    public GoalDelimeter(){
	this.librarian = new Librarian<PIndex>();
    };


    public GoalDelimeter(Instance instance, PBSolver solver) {
	this();
	this.instance = instance;	
	this.solver = solver;
	this.setFirstVariable();
}

    public void setSolver(PBSolver solver){this.solver = solver;}
    public PBSolver getSolver(){return this.solver;}

    public void setInstance(Instance instance){this.instance = instance;}
    public Instance getInstance(){return this.instance;}

    public int getFirstVariable(){return this.firstVariable;}
    public void setFirstVariable(){this.firstVariable = this.getSolver().nVars() + 1;}
    
    /**
     * Generate the upper limit assumptions
     */
    public IVecInt generateUpperBoundAssumptions(){return null;};
    public IVecInt generateUpperBoundAssumptions(IVecInt explanation){return null;};
    public IVecInt generateUpperBoundAssumptions(int[] upperKD){return null;};

;

    /**
     *Checks if a variable is an X(original) variable.
     */
    public boolean isX(int literal){
	int id = this.solver.idFromLiteral(literal);
	//TODO: the 1 is the constant literal, that the solver creates.
	if(id < this.firstVariable - 1)
	    return true;
	return false;
    }


    /**
     *Adds the disjunction of setOfLiterals
     *@param setOfliterals
     */

    public boolean AddClause(IVecInt setOfLiterals){
	// this.prettyPrintVecInt(setOfLiterals);
	try{
	    this.solver.AddClause(setOfLiterals);
	} catch (ContradictionException e) {
	    Log.comment(6, "contradiction when adding clause: ");
	    for(int j = 0; j < setOfLiterals.size(); ++j)
		Log.comment(3, " " + setOfLiterals.get(j) + " " );
	    return false;
	}
	return true;
    }
}
