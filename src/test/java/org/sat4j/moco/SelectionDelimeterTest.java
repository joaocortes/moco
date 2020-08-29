/*******************************************************************************
 * SAT4J: a SATisfiability library for Java Copyright (C) 2004, 2012 Artois University and CNRS
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 *  http://www.eclipse.org/legal/epl-v10.html
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU Lesser General Public License Version 2.1 or later (the
 * "LGPL"), in which case the provisions of the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of the LGPL, and not to allow others to use your version of
 * this file under the terms of the EPL, indicate your decision by deleting
 * the provisions above and replace them with the notice and other provisions
 * required by the LGPL. If you do not delete the provisions above, a recipient
 * may use your version of this file under the terms of the EPL or the LGPL.
 *
 * Contributors:
 *   CRIL - initial API and implementation
 *   Miguel Terra-Neves, Ines Lynce and Vasco Manquinho - MOCO solver
 *******************************************************************************/
package org.sat4j.moco;

import static org.junit.Assert.assertTrue;

import java.util.Iterator;

import org.junit.Before;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.sat4j.core.Vec;
import org.sat4j.core.VecInt;
import org.sat4j.moco.algorithm.UnsatSat;
import org.sat4j.moco.pb.PBFactory;
import org.sat4j.moco.pb.PBSolver;
import org.sat4j.moco.problem.DigitalEnv;
import org.sat4j.moco.problem.Instance;
import org.sat4j.moco.problem.LinearObj;
import org.sat4j.moco.problem.Objective;
import org.sat4j.moco.problem.SelectionDelimeter;
import org.sat4j.moco.problem.DigitalEnv.DigitalNumber;
import org.sat4j.moco.problem.SelectionDelimeter.ObjManager;
import org.sat4j.moco.util.Log;
import org.sat4j.moco.util.Real;
import org.sat4j.specs.ContradictionException;
import org.sat4j.specs.ISolver;
import org.sat4j.specs.IVecInt;
import org.sat4j.specs.TimeoutException;
import org.sat4j.tools.ModelIterator;

public class SelectionDelimeterTest {
    protected SelectionDelimeter sd = null;
    protected PBSolver pbSolver;
    protected Instance moco;
    protected LinearObj main_obj;
    protected IVecInt range;
    static{Log.setVerbosity(6);}

    public SelectionDelimeterTest(){};
    @BeforeEach
    public void partialSetUp() {
	    this.moco = new Instance();
	    this.main_obj = new LinearObj(new VecInt(new int[] { 1, 2 }),
					  new Vec<Real>(new Real[] { new Real(2), new Real(5) }));
	    this.moco.addObj(this.main_obj);
	    try {
		this.pbSolver = buildSolver();

	    }
	    catch (ContradictionException e) {
		Log.comment("Could not build the solver");
            return;
        }
	    this.sd  = new SelectionDelimeter(moco, this.pbSolver,true){
		    /**
		     *Adds the disjunction of setOfLiterals, and logs
		     *@param setOfliterals
		     */

		    public boolean AddClause(IVecInt setOfLiterals){
			Log.comment(6,"AddClause:");
			this.prettyPrintVecInt(setOfLiterals,true);
			try{
			    this.solver.AddClause(setOfLiterals);
			} catch (ContradictionException e) {
			    Log.comment(2, "contradiction when adding clause: ");
			    for(int j = 0; j < setOfLiterals.size(); ++j)
				Log.comment(2, " " + setOfLiterals.get(j) + " " );
			    return false;
			}
			return true;
		    }

};
	    this.range = new VecInt(this.pbSolver.nVars());
	    for(int i = 0, n = this.pbSolver.nVars(); i < n; i++)
		this.range.push(i + 1);
    }
    @Test
    public void testDigits(){
	int[] ratios = new int[]{2};
	DigitalEnv digitalEnv = new DigitalEnv(ratios);
	DigitalEnv.DigitalNumber digitalNumber = digitalEnv.toDigital(9);
	int digit0 = digitalNumber.getDigit(1);
	assertTrue("digit0 fails", digit0 == 1);
	int digit1 = digitalNumber.getDigit(2);
	assertTrue("digit1 fails", digit1 == 0);
	int digit2 = digitalNumber.getDigit(4);
	assertTrue("digit2 fails", digit2 == 0);
	int digit3 = digitalNumber.getDigit(8);
	assertTrue("digit3 fails", digit3 == 1);
    }

    @Test
    public void testDigitsMulti(){
	int[] ratios = new int[]{2, 3};
	DigitalEnv digitalEnv = new DigitalEnv(ratios);
	DigitalEnv.DigitalNumber digitalNumber = digitalEnv.toDigital(8);
	int digit0 = digitalNumber.getDigitI(0);
	assertTrue("digit0 fails", digit0 == 0);
	int digit1 = digitalNumber.getDigitI(1);
	assertTrue("digit1 fails: " + digit1, digit1 == 1);
	int digit2 = digitalNumber.getDigitI(2);
	assertTrue("digit2 fails", digit2 == 1);
	int digit3 = digitalNumber.getDigitI(4);
	assertTrue("digit3 fails", digit3 == 0);
    }

    @Test
    public void testWeightExpansion(){
	int[] ratios = new int[]{2, 3};
	DigitalEnv digitalEnv = new DigitalEnv(ratios);
	DigitalEnv.DigitalNumber digitalNumber = digitalEnv.toDigital(8);
	int digit0 = digitalNumber.getDigitI(0);
	assertTrue("digit0 fails", digit0 == 0);
	int digit1 = digitalNumber.getDigitI(1);
	assertTrue("digit1 fails: " + digit1, digit1 == 1);
	int digit2 = digitalNumber.getDigitI(2);
	assertTrue("digit2 fails", digit2 == 1);
	int digit3 = digitalNumber.getDigitI(4);
	assertTrue("digit3 fails", digit3 == 0);
    }



@Test
    public void delimitationTest(){
	int[] upperBound = new int[]{};
	assertTrue(this.moco.nObjs() + " objectives and " + upperBound.length + "upper bounds", this.moco.nObjs() == upperBound.length);
	IVecInt assumptions = this.sd.generateUpperBoundAssumptions(upperBound);
	Iterator<boolean[]> iterator = new MyModelIterator(this.pbSolver, assumptions);
	boolean[] model;
	int modelN = 0;
	while(iterator.hasNext()){
	    modelN++;
	    model = iterator.next();
	    assertTrue("model " + model + "failed the test", this.testUpperBound(model, upperBound));}
	    assertTrue("0 models", modelN > 0);

    }

    private boolean testUpperBound(boolean[] model, int[] upperBound){
	for(int i = 0, n = this.moco.nObjs(); i < n; i++)
	    if(this.moco.getObj(i).evaluate(model).asIntExact() <= upperBound[i])
		continue;
	    else
		return false;
	return true;	

}
    @Test
    public void digitalCounterTest(){

	boolean[] inputValues = new boolean[this.moco.nVars()];
	inputValues[0] = false;
	inputValues[1] = true;
	IVecInt assumptions = new VecInt();
	for(int i = 1, n = this.moco.nVars(); i <= n;i++)
	    if(inputValues[i - 1])
		assumptions.push(i);
	    else
		assumptions.push(-i);

	Iterator<boolean[]> iterator = new MyModelIterator(this.pbSolver, assumptions);
	boolean[] model;
	while(iterator.hasNext()){
	    model = iterator.next();
	    this.testDigitalValues(model);
	}
    }
    private void testDigitalValues(boolean[] model){
	for(int i = 0, n = this.moco.nObjs(); i < n; i++){
	    ObjManager objManager  = this.sd.getIthObjManager(i);
	    Objective objective = this.moco.getObj(i);
	    int obtained = objective.evaluate(model).asIntExact();
	    DigitalNumber digitalNumber = objManager.getDigitalEnv().toDigital(obtained);
	    DigitalNumber.IteratorJumps iterator = digitalNumber.iterator();
	    int digit0 = 0;
	    int base = 0;
	    int lit = 0;
	    while(iterator.hasNext()){
		digit0 = iterator.next();
		base = iterator.currentBase();
		for(int digit = 0; digit <= digit0; digit++){
		    lit = objManager.digitalLiteral(base, digit );
		    if(lit != 0)
			assertTrue("failing at base " + base + ", value" + digit,
				   this.pbSolver.modelValue(lit));
		}
	    }

	}

    }

        /**
     * Creates a PB oracle initialized with the MOCO's constraints.
     * @return The oracle.
     * @throws ContradictionException if the oracle detects that the
     * MOCO's constraint set is unsatisfiable.
     */
    private PBSolver buildSolver() throws ContradictionException {
        // Log.comment(5, "in UnsatSat.buildSolver");
        PBSolver solver = new PBSolver();
        solver.newVars(this.moco.nVars());
        for (int i = 0; i < this.moco.nConstrs(); ++i) {
            solver.addConstr(this.moco.getConstr(i));
        }
        // Log.comment(5, "out UnsatSat.buildSolver");
        return solver;
    }


/**
 * Enumerate all models of a DIMACS formula. Implement the enumeration
 *
 */


    static public class MyModelIterator implements Iterator<boolean[]>{
	private PBSolver pbSolver;
	boolean contradiction = false;
	final IVecInt assumptions;

	public MyModelIterator(PBSolver solver, IVecInt assumptions){
	    this.pbSolver =  solver;
	    this.assumptions = assumptions;
	}


	public boolean hasNext(){
	    this.pbSolver.check(assumptions);
	    if(contradiction)
		return false;
	    return this.pbSolver.isSat();
	}
	public boolean[] next(){
	    boolean[] currentAssignment = new boolean[this.pbSolver.nVars()];
	    IVecInt notCurrent = new VecInt();
	    int litId;
	    for(int i = 0, n = currentAssignment.length; i < n ; i++){
		litId = i + 1;
		currentAssignment[i] = this.pbSolver.modelValue(litId);
		if(currentAssignment[i])
		    notCurrent.push(-litId);
		else
		    notCurrent.push(litId);

	    }
	    if(notCurrent.size() > 0)
		try {
		this.pbSolver.AddClause(notCurrent);
	    }
	    catch (ContradictionException e) {
		Log.comment(3, "Contradiction detected!");
		this.contradiction = true;
	    }
	    

	    return currentAssignment;
	}

    }
}
