package org.sat4j.moco.problem;

import org.sat4j.moco.util.Log;
import org.sat4j.specs.IVecInt;
import java.util.Hashtable;
import org.sat4j.moco.pb.PBSolver;
import org.sat4j.moco.pb.PBFactory;
import org.sat4j.specs.ContradictionException;

public abstract class GoalDelimeter{

    /**
     *Acess to the instance to be solved
     */
    private Instance instance = null;
    /**
     *Access to the solver being used
     */
    private PBSolver solver = null;

    /**
     *The inverse index map for the S(um) variables. For each ID, a
     *value that is an array vector with the value of the goal and the
     *value of the sum
     */
    private Hashtable<Integer,int[]> auxVariablesInverseIndex  = new Hashtable<Integer, int[]>();

    abstract public void UpdateCurrentK(int iObj, int upperKD);
    abstract public boolean isY(int id);

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

     public void prettyPrintVecInt(IVecInt vecInt){
	Log.comment(6,prettyFormatVecInt(vecInt));
	 return;
     }

    public void prettyPrintVariable(int literal){
	Log.comment(6,prettyFormatVariable(literal));
    }

     /**
      *Return the ID of a freshly created auxiliar variable
      */
     protected int newAuxiliarVar(int sum, int iObj){
	 this.solver.newVar();
	 int id = this.solver.nVars();
	 this.auxVariablesInverseIndex.put(id, new int[]{sum, iObj});
	 return id;
     }


    /**
     *Adds the disjunction of setOfLiterals
     *@param setOfliterals
     */

    protected void AddClause(IVecInt setOfLiterals){
	this.prettyPrintVecInt(setOfLiterals);
	for(int i = 0; i < setOfLiterals.size(); ++i)
	try{
	    this.solver.addConstr(PBFactory.instance().mkClause(setOfLiterals));
	} catch (ContradictionException e) {
	    Log.comment(6, "contradiction when adding clause: ");
	    for(int j = 0; j < setOfLiterals.size(); ++j)
		Log.comment(6, " " + setOfLiterals.get(j) + " " );
	    return;
	}
    }


}
