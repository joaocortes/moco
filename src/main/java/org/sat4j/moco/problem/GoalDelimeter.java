package org.sat4j.moco.problem;

import org.sat4j.specs.IVecInt;

public interface GoalDelimeter{

    public void prettyPrintVecInt(IVecInt vecInt);
    public void getY(int iObj, int kD);
    public void updateCurrentK(int iObj, int upperKD);
    public int getIObjFromY(int id);
    public int getUpperKFromY(int id);
    public void updateCurrentKD(int iObj, int newkD);
    public boolean isY(int literal);
    public void prettyPrintLiteral(int literal);
    
}
