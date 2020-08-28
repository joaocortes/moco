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

import org.junit.Before;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import org.sat4j.core.Vec;
import org.sat4j.core.VecInt;
import org.sat4j.moco.algorithm.UnsatSat;
import org.sat4j.moco.pb.PBFactory;
import org.sat4j.moco.pb.PBSolver;
import org.sat4j.moco.problem.DigitalEnv;
import org.sat4j.moco.problem.Instance;
import org.sat4j.moco.problem.LinearObj;
import org.sat4j.moco.problem.SelectionDelimeter;
import org.sat4j.moco.util.Real;
import org.sat4j.specs.ContradictionException;

public class SelectionDelimeterTest {
    protected SelectionDelimeter sd = null;
    protected Instance moco;
    protected LinearObj main_obj;
    protected UnsatSat solver;

    public SelectionDelimeterTest(){};
    @BeforeEach
    public void partialSetUp() {
	    this.moco = new Instance();
	    this.moco.addConstr(PBFactory.instance().mkGE(new VecInt(new int[] { 1, 2, 3 }), 2));
	    this.main_obj = new LinearObj(new VecInt(new int[] { 1, 2 }),
                                      new Vec<Real>(new Real[] { new Real(9), Real.ONE }));
	    this.moco.addObj(this.main_obj);
	    PBSolver pbSolver;

	    
	    try {
		pbSolver = buildSolver();

	    }
	    catch (ContradictionException e) {
            return;
        }
	    this.solver = new UnsatSat(moco);
	    this.sd  = new SelectionDelimeter(moco, pbSolver,true);

	    
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

}
