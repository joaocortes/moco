package org.sat4j.moco.goal_delimeter;

import org.sat4j.moco.util.Log;
import org.sat4j.specs.IVecInt;

import java.util.HashMap;
import java.util.Map;

import org.sat4j.moco.pb.PBSolver;
import org.sat4j.moco.problem.Instance;


import org.sat4j.specs.ContradictionException;


public abstract class GoalDelimeter<PIndex extends GoalDelimeter.Index>{

    /**
     * the instance to be solved
     */
    protected Instance instance = null;
    /**
     * the solver being used
     */
    protected PBSolver solver = null;

    protected Librarian librarian = null;

    /**
     *First solver variable that pertains to the goal delimeter
     *encoding
     */

    protected int firstVariable = 0;

    public GoalDelimeter(){
	this.librarian = new Librarian();
    };


    public GoalDelimeter(Instance instance, PBSolver solver) {
	this();
	this.instance = instance;	
	this.solver = solver;
	this.firstVariable = solver.nVars() + 1;
}

    public void setSolver(PBSolver solver){this.solver = solver;}
    public PBSolver getSolver(){return this.solver;}

    public void setInstance(Instance instance){this.instance = instance;}
    public Instance getInstance(){return this.instance;}

    public int getFirstVariable(){return this.firstVariable;}
    public void setFirstVariable(int firstVariable){ this.firstVariable = firstVariable;}
    


    protected class Librarian{
	
	private Map<Integer, PIndex> library = null;

	 public Librarian(){
	    this.library = new HashMap<Integer, PIndex>();
	};

	public  void putIndex(int varId, PIndex index) {
	    this.library.put(varId, index);
	};

	public PIndex getIndex(int varId){
	    return this.library.get(varId);
	};
	
    }

    static protected abstract class Index{
	int iObj = 0;
	int kD = 0;
	public Index(){};
	public Index(int iObj, int kD){
	    this.iObj = iObj;
	    this.kD = kD;
	};
	public int getIObj(){
	    return this.iObj;
}
	public int getKD(){
	    return this.kD;
}
    };

    public boolean UpdateCurrentK(int iObj, int upperKD){return true;};
    abstract public boolean isY(int id);

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

    abstract public int getCurrentKD(int iObj);

    abstract public int getIObjFromY(int id);
    abstract public int getKDFromY(int id);

    abstract public int getY(int iObj, int iKD);

    abstract public String prettyFormatVariable(int literal);    

    public String prettyFormatVecInt(IVecInt vecInt){
	 String result = "";
	 for(int j = 0; j < vecInt.size(); ++j)
	     result += this.prettyFormatVariable(vecInt.get(j));
	 return result;
     }
    public String prettyFormatVecIntWithValues(IVecInt vecInt){
	 String result = "\n";
	 for(int j = 0; j < vecInt.size(); j++)
	     result += this.prettyFormatVariable(vecInt.get(j)) + " " +this.solver.modelValue(vecInt.get(j)) + "|\n";
	 return result;
     }
     
    public String prettyFormatArrayWithValues(Integer[] literals){
	String result = "";
	for(int j = 0, n = literals.length; j < n ; ++j)
	    result += prettyFormatVariable(literals[j]) + " " +this.solver.modelValue(literals[j]) + "|\n";
	return result;
    }


    public void prettyPrintVecInt(IVecInt vecInt, boolean clausing){
	if(clausing)
	    Log.clausing(this.prettyFormatVecInt(vecInt));
	else
	    Log.comment(6, this.prettyFormatVecInt(vecInt));
	return;
    }
    public void prettyPrintVecInt(IVecInt vecInt){
	Log.comment(2, prettyFormatVecInt(vecInt));
	return;
     }
    public void prettyPrintVecInt(IVecInt vecInt, int level){
	Log.comment(level, prettyFormatVecInt(vecInt));
	return;
     }
    public void prettyPrintVariable(int literal){
	Log.comment(6,prettyFormatVariable(literal));
    }
    public void prettyPrintVariable(int literal, int level){
	Log.comment(level,prettyFormatVariable(literal));
    }


    /**
     *Return the ID of a freshly created auxiliar variable
     */

    protected int getFreshVar(){
	solver.newVar();
	return solver.nVars();
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

    public boolean preAssumptionsExtend(IVecInt currentExplanation){return true;};
    public int getUpperKD(int iObj){return -1;};
}
