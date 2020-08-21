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
import static org.junit.jupiter.api.Assertions.assertFalse;

import java.util.List;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.Random;

import org.junit.Before;
import org.junit.jupiter.api.Test;
import org.sat4j.core.Vec;
import org.sat4j.core.VecInt;
import org.sat4j.moco.algorithm.UnsatSat;
import org.sat4j.moco.pb.PBFactory;
import org.sat4j.moco.pb.PBSolver;
import org.sat4j.moco.problem.DigitalEnv;
import org.sat4j.moco.problem.Instance;
import org.sat4j.moco.problem.LinearObj;
import org.sat4j.moco.problem.SelectionDelimeter;
import org.sat4j.moco.problem.SelectionDelimeter.Circuit;
import org.sat4j.moco.problem.SelectionDelimeter.Circuit.ModComponent;
import org.sat4j.moco.util.Real;
import org.sat4j.specs.ContradictionException;
import org.sat4j.specs.IVecInt;

public class CircuitTest {
    protected SelectionDelimeter sd = null;
    protected Instance moco;
    protected LinearObj main_obj;
    protected UnsatSat solver;
    protected PBSolver pbSolver = new PBSolver();
    @Before
    public void partialSetUp() {

    }
    
    @Test
    public void ModuleComponentTest(){
	int modN = 2;
	Integer[] inputs = new Integer[2 * modN];
	for(int i = 0; i < inputs.length; i++){
	    this.pbSolver.newVar();
	    inputs[i] =  this.pbSolver.nVars();
	}
	Circuit circuit = new Circuit(){
		public void buildCircuit(){
		    ModComponent modComponent = new ModComponent(inputs, modN);
		    modComponent.constitutiveClause();
		}
		public int getFreshVar1(){pbSolver.newVar();return pbSolver.nVars();}

		public boolean AddClause1(IVecInt setOfLiterals){
		    // this.prettyPrintVecInt(setOfLiterals,true);
		    try{
			pbSolver.AddClause(setOfLiterals);
		    } catch (ContradictionException e) {return false;} return true;
		}
	    };
	circuit.buildCircuit();
	IVecInt assumptions = new VecInt();
	    int value = modN + 1;
	{
	    int i = 0;
	    for(;i < value; i++)
		assumptions.push(inputs[i]);
	    for(;i < inputs.length; i++)
		assumptions.push(-inputs[i]);
}
	pbSolver.check(assumptions);
	int result = 0;
	for(int lit: inputs)
	    if(pbSolver.modelValue(lit))
		result++;
	assertTrue("result:" + result + " value:" + value,result == value);
}

    @Test
    public void SelectionComponentTest(){
	int sortedPortionN = 4;
	Random rand = new Random();
	int inputsLength = 4;
	Integer[] inputs = new Integer[inputsLength];
	Integer[] inputValues = new Integer[inputsLength];
	for(int i = 0; i < inputs.length; i++){
	    this.pbSolver.newVar();
	    inputs[i] =  this.pbSolver.nVars();
	}

	IVecInt assumptions = new VecInt();
	// for( int i = 0; i < inputs.length; i++){
	//     int random = rand.nextInt(2);
	//     System.out.println("random is " + random);
	//     inputValues[i] = random;
	//     if(random == 1)
	// 	assumptions.push(inputs[i]);
	//     else
	// 	assumptions.push(-inputs[i]);
	// }
	assumptions.push(inputs[0]);
	inputValues[0]=1;
	assumptions.push(inputs[3]);
	inputValues[3]=1;
	assumptions.push(-inputs[2]);
	inputValues[2]=0;
	assumptions.push(-inputs[1]);
	inputValues[1]=0;
	Circuit circuit = new Circuit(){
		public void buildCircuit(){
		    SelectionComponent comp = new SelectionComponent(inputs, sortedPortionN);
		    comp.constitutiveClause();
		}
		public int getFreshVar1(){pbSolver.newVar();return pbSolver.nVars();}

		public boolean AddClause1(IVecInt setOfLiterals){
		    // this.prettyPrintVecInt(setOfLiterals,true);
		    try{
			pbSolver.AddClause(setOfLiterals);
		    } catch (ContradictionException e) {return false;} return true;
		}
	    };


	circuit.buildCircuit();
	List<Integer> sorted = Arrays.asList(inputValues);
	Collections.sort(sorted);
	Collections.reverse(sorted);
	sorted.subList(0, sortedPortionN - 1);
	pbSolver.check(assumptions);
	for(int value: sorted)
	    System.out.println(value);

	for(int i = 0, n = sortedPortionN ; i < n; i++)
	    if(sorted.get(i) == 1)
		assertTrue(pbSolver.modelValue(inputs[i]));
	    else 
		assertFalse(pbSolver.modelValue(inputs[i]));

	return;
    }


}
