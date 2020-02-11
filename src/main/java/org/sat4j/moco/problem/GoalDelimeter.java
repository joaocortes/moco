package org.sat4j.moco.problem;

import org.sat4j.moco.util.Log;
import org.sat4j.specs.IVecInt;

public abstract class GoalDelimeter{

    abstract void UpdateCurrentK(int iObj, int upperKD);
    abstract boolean isY(int id);

    abstract int getIObjFromY(int id);
    abstract int getKDFromY(int id);
    abstract int getY(int iObj, int iKD);

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
}
