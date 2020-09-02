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
import org.sat4j.moco.util.MyModelIterator;
import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.Random;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.sat4j.core.VecInt;
import org.sat4j.moco.algorithm.UnsatSat;
import org.sat4j.moco.pb.PBSolver;
import org.sat4j.moco.problem.SelectionDelimeter;
import org.sat4j.moco.problem.SelectionDelimeter.Circuit;
import org.sat4j.moco.problem.SelectionDelimeter.Circuit.ControlledComponent;
import org.sat4j.moco.util.General;
import org.sat4j.moco.util.Log;
import org.sat4j.specs.ContradictionException;
import org.sat4j.specs.IVecInt;

public class CircuitTest {
    protected SelectionDelimeter sd = null;
    protected UnsatSat solver;
    protected PBSolver pbSolver = new PBSolver();
    static {Log.setVerbosity(6);}

    @BeforeEach
    public void partialSetUp() {
	pbSolver = new PBSolver();
	this.pbSolver.setConstantID();


}
    
    @Test
    public void ModuleComponentTest(){
	int modN = 4;
	Integer[] inputs = new Integer[2 * modN];
	for(int i = 0; i < inputs.length; i++){
	    this.pbSolver.newVar();
	    inputs[i] =  this.pbSolver.nVars();
	}
	Circuit circuit = new Circuit(pbSolver){
		public void buildCircuit(){
		    ModComponent modComponent = new ModComponent(inputs, modN);
		    modComponent.constitutiveClause();
		    new ControlledComponent(0, modComponent);
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
	int value = 2;
	{
	    int i = 0;
	    for(;i < value + modN; i++)
		assumptions.push(inputs[i]);
	    for(;i < inputs.length; i++)
		assumptions.push(-inputs[i]);
	}
	pbSolver.check(assumptions);
	ControlledComponent comp = circuit.getControlledComponentBase(0);
	Iterator<boolean[]> iterator = new MyModelIterator(this.pbSolver, assumptions);
	boolean[] model;
	while(iterator.hasNext()){
	    Log.comment("inputs of ModComponent:");
	    General.FormatArrayWithValues(comp.getInputs(), pbSolver, true);
	    Log.comment("outputs of ModComponent:");
	    General.FormatArrayWithValues(comp.getOutputs(), pbSolver, true);
	    model = iterator.next();
	    this.testModComponentModel(comp.getOutputs(), modN, value);
	}
    }

    private void testModComponentModel(Integer[] lits, int modN, int value){
	for(int i = 0; i < value - 1; i++)
	    assertTrue("Failing at " + i +"'th comparison",this.pbSolver.modelValue( lits[i]));
	
    }
    @Test
    public void SelectionComponentTest(){
	int inputsLength = 4;
	int sortedPortionN = inputsLength;
	Random rand = new Random();
	Integer[] inputs = new Integer[inputsLength];
	Integer[] inputValues = new Integer[inputsLength];
	Arrays.fill(inputValues, 0);
	for(int i = 0; i < inputs.length; i++){
	    this.pbSolver.newVar();
	    inputs[i] =  this.pbSolver.nVars();
	}

	IVecInt assumptions = new VecInt();
	for( int i = 0; i < inputs.length; i++){
	    int random = rand.nextInt(2);
	    inputValues[i] = random;
	}
	    for(int k = 0,n = inputValues.length; k < n; k++){
		if(inputValues[k] == 1){
		    assumptions.push(inputs[k]);
		}
		else
		    assumptions.push(-inputs[k]);
	    }

	Circuit circuit = new Circuit(this.pbSolver){
		public void buildCircuit(){
		    SelectionComponent comp = new SelectionComponent(inputs, sortedPortionN);
		    comp.constitutiveClause();
		    new ControlledComponent(0, comp);

		}
		public int getFreshVar1(){pbSolver.newVar();return pbSolver.nVars();}

		public boolean AddClause1(IVecInt setOfLiterals){
		    return AddClause(setOfLiterals, true);
		}
	    };

	circuit.buildCircuit();
	List<Integer> sorted = new ArrayList<Integer>(Arrays.asList(inputValues));
	Collections.sort(sorted);
	Collections.reverse(sorted);
	sorted.subList(sortedPortionN,sorted.size()).clear();
	ControlledComponent controlledComp =  circuit.getControlledComponentBase(0);
	Integer[] outputs = controlledComp.getOutputs();

	// assumptions.push(-controlledComp.getIthOutput(2));

	pbSolver.check(assumptions);
	Integer[] obtainedSorted = new Integer[sorted.size()];
	for(int i = 0, n = sortedPortionN ; i < n; i++)
	    if(pbSolver.modelValue(outputs[i]))
		obtainedSorted[i] = 1;
	    else 
		obtainedSorted[i] = 0;
	
	Log.comment("obtained sorted:");
	for(int value: obtainedSorted)
	    Log.comment("" + value);

	Log.comment("sorted:");
	for(int value: sorted)
	    Log.comment("" + value);

	for(int i = 0, n = sortedPortionN ; i < n; i++)
	    if(sorted.get(i) == 1)
		assertTrue(pbSolver.modelValue(outputs[i]));
	    else 
		assertFalse(pbSolver.modelValue(outputs[i]));

	return;
    }

    @Test void MergeComponentTest(){
	int sortedPortionN = 16;
	Integer[] inputs = new Integer[16];
	this.fillInputWithVars(inputs);
	Integer[][] inputsArray = new Integer[4][];
	int n = inputs.length;
	{
	    int i = 0;
	    int m = 0;
	    int i0 = 0;
	    for(; m < 4 - 1; m++){
		inputsArray[m] = new Integer[n / 4];
		i0 = i;
		for(; i < (m + 1) * n / 4; i++)
		    inputsArray[m][i - i0 ] = inputs[i];
	    }
	    inputsArray[m] = new Integer[n - 3 * n / 4];
	    i0 = i;
	    for(; i < n; i++)
		inputsArray[m][i - i0] = inputs[i];
	}
	Circuit circuit = new Circuit(this.pbSolver){
		public void buildCircuit(){
		    MergeComponent comp = new MergeComponent(sortedPortionN);
		    comp.constitutiveClause(inputsArray);
		    new ControlledComponent(0, comp);
		    }


		public int getFreshVar1(){pbSolver.newVar();return pbSolver.nVars();}

		public boolean AddClause1(IVecInt setOfLiterals){
		    return AddClause(setOfLiterals, false);
		}
	    };

	Integer[] inputValues = new Integer[inputs.length];
	inputValues[0] = 0;
	inputValues[1] = 0; 
	inputValues[2] = 0; 
	inputValues[3] = 0; 

	inputValues[4] = 1; 
	inputValues[5] = 1; 
	inputValues[6] = 1; 
	inputValues[7] = 0; 

	inputValues[8] = 1; 
	inputValues[9] = 1; 
	inputValues[10] = 0; 
	inputValues[11] = 0; 

	inputValues[12] = 1; 
	inputValues[13] = 1; 
	inputValues[14] = 1; 
	inputValues[15] = 1; 

	circuit.buildCircuit();
	ControlledComponent comp1 = circuit.getControlledComponentBase(0);
	List<Integer> sorted = new ArrayList<Integer>(Arrays.asList(inputValues));
	Collections.sort(sorted);
	Collections.reverse(sorted);
	sorted.subList(sortedPortionN, sorted.size()).clear();

	IVecInt assumptions = this.buildAssumption(inputValues, inputs);

	pbSolver.check(assumptions);

	List<Integer> obtainedSorted = new ArrayList<Integer>();
	Iterator<Integer> iterator = comp1.iteratorOutputs();
	while(iterator.hasNext())
	    if(pbSolver.modelValue(iterator.next()))
		obtainedSorted.add(1);
	    else
		obtainedSorted.add(0);
	Iterator<ControlledComponent> iteratorControlledComp = circuit.controlledCompIterator();
	ControlledComponent next;
	while(iteratorControlledComp.hasNext()) {
	    next = iteratorControlledComp.next();
	    auxGetValues(next);
}


	{n = obtainedSorted.size();
	    assertTrue("lengths are different!" + n + " and " + sorted.size() , n == sorted.size());
	    for(int i = 0; i < n; i++  )
		assertTrue("failing " + i +"'th comparison", obtainedSorted.get(i) == sorted.get(i));
		    
	    return;
	}

    }

    private Integer[][] auxGetValues(ControlledComponent controlledComp){
	Integer[][] results = new Integer[2][];
	Integer[] outputs = controlledComp.getOutputs();
	Integer[] inputs = controlledComp.getInputs();
	Integer[] inputsValues = new Integer[inputs.length];
	Log.comment("Looking at component " + controlledComp.getBase());
	for(int i = 0, n = inputs.length; i < n; i++)
	    if(pbSolver.modelValue(inputs[i]))
		inputsValues[i] = 1;
	    else
		inputsValues[i] = 0;
	
	Log.comment("unsorted inputs:");
	for(int value: inputsValues)
	    System.out.print(value + " ");
	System.out.println();

	List<Integer> sorted = new ArrayList<Integer>(Arrays.asList(inputsValues));
	Collections.sort(sorted);
	Collections.reverse(sorted);
	Log.comment("sorted inputs:");
	for(int value: sorted)
	    System.out.print(value + " ");
	System.out.println();

	Integer[] obtainedSorted = new Integer[outputs.length];
	for(int i = 0, n = outputs.length; i < n; i++)
	    if(pbSolver.modelValue(outputs[i]))
		obtainedSorted[i] = 1;
	    else
		obtainedSorted[i] = 0;
	Log.comment("outputs:");
	for(int value: obtainedSorted)
	    System.out.print(value + " ");
	System.out.println();

	results[0] = sorted.toArray(new Integer[0]);
	results[1] = obtainedSorted;
	return results;
    }

    @Test void OptimumComponentTest(){
	Integer[] inputs = new Integer[4];
	boolean polarity = false;
	this.fillInputWithVars(inputs);
	Circuit circuit = new Circuit(pbSolver){
		public void buildCircuit(){
		    optimumComponent comp = new optimumComponent(inputs, polarity);
		    comp.constitutiveClause();
		    new ControlledComponent(0, comp);
		}


		public int getFreshVar1(){pbSolver.newVar();return pbSolver.nVars();}

		public boolean AddClause1(IVecInt setOfLiterals){
		    return AddClause(setOfLiterals, true);
		}
	    };
	circuit.buildCircuit();
	Integer[] inputValues = new Integer[inputs.length];
	inputValues[0] = 0; inputValues[1] = 1; inputValues[2] = 0; inputValues[3] = 1; 
	IVecInt assumptions = this.buildAssumption(inputValues, inputs);
	int compareTo; if(polarity) compareTo = 1; else compareTo = 0;
	int expected = (compareTo + 1) % 2;
	for(int value: inputValues)
	    if(value == compareTo){
		expected = compareTo;
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
	Random rand = new Random();
	Integer[] inputsSize = new Integer[]{8,8};
	int sortedPortionN = inputsSize[0] + inputsSize[1];
	Integer[][] inputs = new Integer[2][];
	Integer[][] inputValues = new Integer[2][];
	for(int k = 0; k < 2; k++){
	    inputs[k] = new Integer[inputsSize[k]];
	    inputValues[k] = new Integer[inputsSize[k]];
	    for(int i = 0; i < inputs[k].length; i++){
		this.pbSolver.newVar();
		inputs[k][i] =  this.pbSolver.nVars();
	    }}
	//Each input shall be a sorted sequence. The first input shall be heavier
	inputValues[0] = new Integer[]{1, 1, 1, 1, 1, 1, 0, 0};
	inputValues[1] = new Integer[]{1, 1, 1, 1, 0, 0, 0, 0};
	IVecInt assumptions = new VecInt();
	for( int i = 0; i < 2; i++)
	    for(int k = 0,n = inputValues[i].length; k < n; k++){
		if(inputValues[i][k] == 1){
		    assumptions.push(inputs[i][k]);
		}
		else
		    assumptions.push(-inputs[i][k]);
	    }
	
	Circuit circuit = new Circuit(pbSolver){
		public void buildCircuit(){
		    SelectionDelimeter.Circuit.CombineComponent comp = new SelectionDelimeter.Circuit.CombineComponent(sortedPortionN);
		    comp.constitutiveClause(inputs[0], inputs[1]);
		    new ControlledComponent(0, comp);
		}


		public int getFreshVar1(){pbSolver.newVar();return pbSolver.nVars();}

		public boolean AddClause1(IVecInt setOfLiterals){
		    return AddClause(setOfLiterals, true);
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

	List<Integer> obtainedSorted = new ArrayList<Integer>();
	Iterator<Integer> iterator = comp1.iteratorOutputs();
	while(iterator.hasNext())
	    if(pbSolver.modelValue(iterator.next()))
		obtainedSorted.add(1);
	    else
		obtainedSorted.add(0);

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
	int lit;
	if(print)
	    {
		Log.comment("clause:");
		for(int i = 0, n = setOfLiterals.size(); i < n; i++){
		    lit = setOfLiterals.get(i);
		Log.comment("lit:"+ lit);
		}
		
		Log.comment("");
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

