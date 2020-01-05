package org.sat4j.moco.problem;

import org.sat4j.specs.IVecInt;

public interface GoalDelimeter{

    public void prettyPrintVecInt(IVecInt vecInt);
    public int getY(int iObj, int kD);
    public void UpdateCurrentK(int iObj, int upperKD);
    public int getIObjFromY(int id);
    public int getKDFromY(int id);
    public boolean isY(int literal);
    public void prettyPrintLiteral(int literal);
    
}
