package org.sat4j.moco.problem;

import org.sat4j.moco.util.Log;
import org.sat4j.specs.IVecInt;
import org.sat4j.moco.pb.PBSolver;

import java.util.HashMap;
import java.util.Map;

import org.sat4j.specs.ContradictionException;


public abstract class GoalDelimeter{

    /**
     *Acess to the instance to be solved
     */
    protected Instance instance = null;
    /**
     *Access to the solver being used
     */
    protected PBSolver solver = null;

    protected Librarian<? extends Index> librarian = null;

    /**
     *First solver variable that pertains to the goal delimeter
     *encoding
     */
    protected int firstVariable = 0;

    public GoalDelimeter(){};


    public GoalDelimeter(Instance instance, PBSolver solver) {
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
    


    static protected class Librarian<SIndex extends Index>{
	
	private Map<Integer, SIndex> library = null;

	 public Librarian(){
	    this.library = new HashMap<Integer, SIndex>();
	};
	public void putIndex(int varId,SIndex index) {
	    this.library.put(varId, index);
	};

	public SIndex getIndex(int varId){
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

    abstract public boolean UpdateCurrentK(int iObj, int upperKD);
    abstract public boolean isY(int id);

    /**
     * Generate the upper limit assumptions
     */
    abstract public IVecInt generateUpperBoundAssumptions(int[] UpperKD);


;

    /**
     *Checks if a variable is an X(original) variable.
     */
    public boolean isX(int literal){
	int id = this.solver.idFromLiteral(literal);
	if(id < this.firstVariable)
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
	// this.prettyPrintVecInt(setOfLiterals,true);
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
