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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.Random;
import java.util.logging.Logger;

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
import org.sat4j.moco.problem.SelectionDelimeter.Circuit.BaseComponent;
import org.sat4j.moco.problem.SelectionDelimeter.Circuit.ControlledComponent;
import org.sat4j.moco.problem.SelectionDelimeter.Circuit.ModComponent;
import org.sat4j.moco.problem.SelectionDelimeter.Circuit.optimumComponent;
import org.sat4j.moco.util.Log;
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

    @Test void MergeComponentTest(){
	int sortedPortionN = 16;
	
	List<ArrayList<Integer>> inputsList = new ArrayList<ArrayList<Integer>>(4);		    
	Integer[] inputs = new Integer[16];
	this.fillInputWithVars(inputs);
	for(int i = 0; i < 4; i++)
	    inputsList.add(new ArrayList<Integer>());

	{
	    int n = inputs.length;
	    int i = 0;
	    for(; i < n / 4; i++)
		inputsList.get(0).add(inputs[i]);
	    for(; i < 2 * n / 4; i++)
		inputsList.get(1).add(inputs[i]);
	    for(; i < 3 * n / 4; i++)
		inputsList.get(2).add(inputs[i]);
	    for(; i < n ; i++)
		inputsList.get(3).add(inputs[i]);
	}

		Circuit circuit = new Circuit(){
		public void buildCircuit(){
		    MergeComponent comp = new MergeComponent(sortedPortionN);
		    comp.constitutiveClause(inputsList);
		    new ControlledComponent(0, comp);
		}


		public int getFreshVar1(){pbSolver.newVar();return pbSolver.nVars();}

		public boolean AddClause1(IVecInt setOfLiterals){
		    try{
			pbSolver.AddClause(setOfLiterals);
		    } catch (ContradictionException e) {return false;} return true;
		}
	    };
	circuit.buildCircuit();
	Integer[] inputValues = new Integer[inputs.length];
	inputValues[0] = 0;
	inputValues[1] = 0; 
	inputValues[2] = 0; 
	inputValues[3] = 1; 
	inputValues[4] = 1; 
	inputValues[5] = 1; 
	inputValues[6] = 1; 
	inputValues[7] = 1; 
	inputValues[8] = 1; 
	inputValues[9] = 1; 
	inputValues[10] = 1; 
	inputValues[11] = 1; 
	inputValues[12] = 1; 
	inputValues[13] = 1; 
	inputValues[14] = 1; 
	inputValues[15] = 1; 

	circuit.buildCircuit();
	ControlledComponent comp1 = circuit.getControlledComponentBase(0);
	comp1.getOutputs();
	List<Integer> sorted = new ArrayList<Integer>(Arrays.asList(inputValues));
	sorted.addAll(Arrays.asList(inputValues[1]));
	Collections.sort(sorted);
	Collections.reverse(sorted);
	// sorted.subList(0, sortedPortionN - 1);
	for(int value: sorted)
	    System.out.println(value);

	IVecInt assumptions = this.buildAssumption(inputValues, inputs);
	pbSolver.check(assumptions);

	List<Integer> obtainedSorted = new ArrayList<Integer>();
	Iterator<Integer> iterator = comp1.iteratorOutputs();
	while(iterator.hasNext())
	    if(pbSolver.modelValue(iterator.next()))
		obtainedSorted.add(1);
	    else
		obtainedSorted.add(0);
	for(int value: obtainedSorted)
	    System.out.println(value);

	{int n = obtainedSorted.size();
	    assertTrue("lengths are different!", n == sorted.size());
	    for(int i = 0; i < n; i++  )
		assertTrue("failing " + i +"'th comparison", obtainedSorted.get(i) == sorted.get(i));
		    
	    return;
	}

    }

    @Test void OptimumComponentTest(){
	Integer[] inputs = new Integer[4];
	this.fillInputWithVars(inputs);
	Circuit circuit = new Circuit(){
		public void buildCircuit(){
		    optimumComponent comp = new optimumComponent(inputs);
		    comp.constitutiveClause();
		    new ControlledComponent(0, comp);
		}


		public int getFreshVar1(){pbSolver.newVar();return pbSolver.nVars();}

		public boolean AddClause1(IVecInt setOfLiterals){
		    try{
			pbSolver.AddClause(setOfLiterals);
		    } catch (ContradictionException e) {return false;} return true;
		}
	    };
	circuit.buildCircuit();
	Integer[] inputValues = new Integer[inputs.length];
	inputValues[0] = 0; inputValues[1] = 0; inputValues[2] = 0; inputValues[3] = 1; 
	IVecInt assumptions = this.buildAssumption(inputValues, inputs);
	int expected = 0;
	for(int value: inputValues)
	    if(value == 1){
		expected = 1;
		break;
	    }
	ControlledComponent comp = circuit.getControlledComponentBase(0);
	Integer[] output = comp.getOutputs();
	this.pbSolver.check(assumptions);
	Log.comment("expected is " + expected);
	assertTrue("length is not 1", output.length == 1);
	if(this.pbSolver.modelValue(output[0]))
	    assertTrue("value is not correct", expected == 1);
	else
	    assertTrue("value is not correct", expected == 0);
    }


    @Test
    public void CombineComponentTest(){
	int sortedPortionN = 8;
	Random rand = new Random();
	Integer[] inputsSize = new Integer[]{4,4};
	Integer[][] inputs = new Integer[2][];
	Integer[][] inputValues = new Integer[2][];
	for(int k = 0; k < 2; k++){
	    inputs[k] = new Integer[inputsSize[k]];
	    inputValues[k] = new Integer[inputsSize[k]];
	    for(int i = 0; i < inputs[k].length; i++){
		this.pbSolver.newVar();
		inputs[k][i] =  this.pbSolver.nVars();
	    }}
	//Each input shall be a sorted sequence
	inputValues[0][0] = 1; inputValues[0][1] = 1; inputValues[0][2] = 0; inputValues[0][3] = 0; 

	inputValues[1][0] = 1; inputValues[1][1] = 1; inputValues[1][2] = 1; inputValues[1][3] = 1; 
	int expectedTrueN = 0;
	IVecInt assumptions = new VecInt();
	for( int i = 0; i < 2; i++)
	    for(int k = 0,n = inputValues[i].length; k < n; k++){
		if(inputValues[i][k] == 1){
		    assumptions.push(inputs[i][k]);
		    expectedTrueN++;
		}
		else
		    assumptions.push(-inputs[i][k]);
	    }
	
	// for( int i = 0; i < 2; i++)
	//     for(int k = 0,n = inputValues[i].length; k < n; k++){
	// 	int random = rand.nextInt(2);
	// 	System.out.println("random is " + random);
	// 	inputValues[i][k] = random;
	// 	if(random == 1)
	// 	    assumptions.push(inputs[i][k]);
	// 	else
	// 	    assumptions.push(-inputs[i][k]);
	//     }
	Circuit circuit = new Circuit(){
		public void buildCircuit(){
		    SelectionDelimeter.Circuit.CombineComponent comp = new SelectionDelimeter.Circuit.CombineComponent(sortedPortionN);
		    comp.constitutiveClause(inputs[0], inputs[1]);
		    new ControlledComponent(0, comp);
		}


		public int getFreshVar1(){pbSolver.newVar();return pbSolver.nVars();}

		public boolean AddClause1(IVecInt setOfLiterals){
		    try{
			pbSolver.AddClause(setOfLiterals);
		    } catch (ContradictionException e) {return false;} return true;
		}
	    };

	circuit.buildCircuit();
	ControlledComponent comp1 = circuit.getControlledComponentBase(0);
	comp1.getOutputs();
	List<Integer> sorted = new ArrayList<Integer>(Arrays.asList(inputValues[0]));
	sorted.addAll(Arrays.asList(inputValues[1]));
	Collections.sort(sorted);
	Collections.reverse(sorted);
	sorted.subList(0, sortedPortionN - 1);
	pbSolver.check(assumptions);
	for(int value: sorted)
	    System.out.println(value);

	List<Integer> obtainedSorted = new ArrayList<Integer>();
	Iterator<Integer> iterator = comp1.iteratorOutputs();
	while(iterator.hasNext())
	    if(pbSolver.modelValue(iterator.next()))
		obtainedSorted.add(1);
	    else
		obtainedSorted.add(0);
	for(int value: obtainedSorted)
	    System.out.println(value);
	{int n = obtainedSorted.size();
	    assertTrue("lengths are different!",n == sorted.size());
	    for(int i = 0; i < n; i++  )
		assertTrue("failing " + i +"'th comparison", obtainedSorted.get(i) == sorted.get(i));
		    
	    return;
	}

    }
    public IVecInt buildAssumption(Integer[] inputValues, Integer[] inputs){
	IVecInt assumptions = new VecInt();
	for(int i = 0, n = inputValues.length; i < n; i++){
	    if(inputValues[i] == 1)
	        assumptions.push(inputs[i]);
	    else
	        assumptions.push(-inputs[i]);
	}
	return assumptions;
    }

    public boolean AddClause(IVecInt setOfLiterals, boolean print){
	if(print)
	    {
		Log.comment("clause:");
		for(int lit: setOfLiterals.toArray())
		    Log.comment("lit:"+ lit);
		System.out.println();
	    }
	try{
	    pbSolver.AddClause(setOfLiterals);
	} catch (ContradictionException e) {return false;} return true;
    }
    public void fillInputWithVars(Integer[] inputs){
	for(int i = 0; i < inputs.length; i++){
	    this.pbSolver.newVar();
	    inputs[i] =  this.pbSolver.nVars();
	}

    }
}
