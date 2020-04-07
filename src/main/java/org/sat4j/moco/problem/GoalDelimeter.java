package org.sat4j.moco.problem;

import org.sat4j.moco.util.Log;
import org.sat4j.specs.IVecInt;
import org.sat4j.moco.pb.PBSolver;
import org.sat4j.moco.pb.PBFactory;
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

    /**
     *First solver variable that pertains to the goal delimeter
     *encoding
     */
    private int firstVariable = 0;

    abstract public boolean UpdateCurrentK(int iObj, int upperKD);
    abstract public boolean isY(int id);

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

    abstract String prettyFormatVariable(int literal);    


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
	Log.comment(5, prettyFormatVecInt(vecInt));
	return;
     }

   

    public void prettyPrintVariable(int literal){
	Log.comment(6,prettyFormatVariable(literal));
    }



    /**
     *Adds the disjunction of setOfLiterals
     *@param setOfliterals
     */

    public boolean AddClause(IVecInt setOfLiterals){
	this.prettyPrintVecInt(setOfLiterals,true);
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
