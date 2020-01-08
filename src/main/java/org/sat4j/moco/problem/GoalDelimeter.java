package org.sat4j.moco.problem;

import org.sat4j.specs.IVecInt;

public interface GoalDelimeter{

    public void prettyPrintVecInt(IVecInt vecInt);
    public void UpdateCurrentK(int iObj, int upperKD);
    public void prettyPrintLiteral(int literal);
    
}
