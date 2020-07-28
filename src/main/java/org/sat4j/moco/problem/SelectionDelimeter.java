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
 *   João O'Neill Cortes, INESC
 *******************************************************************************/
package org.sat4j.moco.problem;


import org.sat4j.moco.util.Log;
import java.util.List;
import java.util.Map;
import java.util.ArrayList;

import java.util.PriorityQueue;
    
import java.util.Arrays;
import java.util.TreeMap;
import java.util.Map.Entry;
import java.util.stream.Collector;
import java.util.stream.Collectors;



import java.util.Collection;
import java.util.HashMap;


import org.sat4j.core.ReadOnlyVec;
import org.sat4j.core.VecInt;
import org.sat4j.core.Vec;
import org.sat4j.moco.util.Real;
import org.sat4j.moco.pb.PBSolver;
import org.sat4j.moco.problem.SelectionDelimeter.Circuit.SelectionComponent;
import org.sat4j.specs.IVecInt;


/**
 * Class with the implementation of the generalized totalizor encoder.
 * Notice that the differential is both a value and an index starting at 0
 * @author Joao O'Neill Cortes
 */

public class SelectionDelimeter extends GoalDelimeter {

    private Circuit[] circuits;
    

    public SelectionDelimeter(Instance instance, PBSolver solver) {
	// Log.comment(5, "{ GenTotalEncoder");
	
	this.circuits = new Circuit[this.instance.nObjs()];
	for(int iObj = 0, nObj = instance.nObjs() ;iObj< nObj; ++iObj){
	    this.circuits[iObj] = new Circuit(iObj);
	    this.buildCircuit(iObj);
	}

	// Log.comment(5, "}");
    }


    class Circuit{
	int iObj;
	int[] bases = null;
	int basesN = 0;
	Map<Integer, ControlledSelectionComponent> controlledComponents = null;
	
	abstract class BaseComponent {

	    protected Integer[] inputs;
	    protected  Integer[] outputs;
	    int nOutputs = 0;

	    public BaseComponent(){
	    }
	    public BaseComponent(int nOutputs){
		this.outputs = new Integer[nOutputs];
	    }
	    public BaseComponent(Integer[] inputs, int nOutputs){
		super();
		this.inputs = inputs;
		this.outputs = new Integer[nOutputs];
	    }

	    abstract void constitutiveClause();

	}



	class SelectionComponent extends BaseComponent{

	    public SelectionComponent(Integer[] inputs){
		super(inputs, inputs.length);
	    }

	    public SelectionComponent(Integer[] inputs, int nOutput){
		super(inputs, nOutput);
	    }

	    @Override
	    void constitutiveClause() {
	
		int n = this.inputs.length;
		int k = this.outputs.length;
		int[] ns = new int[4];
	    
		if(n == 0 || k <=1){
		    optimumComponent sonoc = new optimumComponent(this.inputs);
		    sonoc.constitutiveClause();
		    this.outputs = sonoc.outputs;
		    return;
		}
		if(n < 8 || k == n){
		    for(int i = 0; i<4; i++){
			ns[i] = (n + i)/ 4;
		    }
		
		}
		else{
		    // notice (k + 3) /4 is equivalent to floor((double)k / 4), if k is int.
		    int tailValue;
		    int largestPower  = (int) Math.pow(2 , Math.ceil(Math.log((k + 3 )/4)));
		    if(largestPower <= n / 4)
			tailValue = largestPower;
		    else
			tailValue = k / 4;
		    for(int i = 1; i< 4; i++)
			ns[i] =  tailValue;
		    ns[0] = n - 3 * tailValue;
		    
		}
		int offset = 1;
		int ki;

		List<ArrayList<Integer>> preffixes = new ArrayList<ArrayList<Integer>>();
		List<Integer> concatenatedSuffixes = new ArrayList<Integer>();
		for(int ni: ns){
		    ki = k < ni? k : ni;
		    List<Integer> slice = this.inputs.subList(offset, offset + ni - 1);
		    SelectionComponent selcomp = new SelectionComponent(slice, ki);
		    preffixes.add(preffix(selcomp.outputs, ki));
		    concatenatedSuffixes.addAll(suffix(selcomp.outputs, ki + 1));
		}
		MergeComponent mergecomp = new MergeComponent(preffixes, k);
		mergecomp.constitutiveClause();
		this.outputs = mergecomp.outputs;
		this.outputs.addAll(concatenatedSuffixes);
		return;
	    }
	}

	/**
	 *Component with 2 inputs that selects either the max or min
	 *entry, according to its polarity
	 */

	class optimumComponent extends BaseComponent{
	    boolean polarity = true;
	    public optimumComponent(Integer[] inputs, boolean polarity){
		super(inputs, 1);
		this.polarity = polarity;
		this.outputs =  new Integer[1];
	    }

	    public optimumComponent(Integer[] inputs){
		this(inputs, true);
	    }

	    @Override
	    void constitutiveClause() {
		this.outputs[0]=getFreshVar();
		int[] intArray = null;
		// polarity, then use a max component, else use min
		    if(this.polarity)
			intArray = Arrays.stream(this.inputs).mapToInt(Integer::intValue).toArray();
		    else
			intArray = Arrays.stream(this.inputs).mapToInt(i -> -i).toArray();
		    for(int lit: intArray){
			IVecInt clause = new VecInt(new int[]{-lit, this.outputs[0]});
			AddClause(clause);
		    }

		return;
	    }

	}

	
	class CombineComponent extends BaseComponent{

	    public CombineComponent(int nOutput){
		this.outputs = new Integer[nOutput];
	    }

	    // This is required by my stupidity.
	    void constitutiveClause(){};

	    void constitutiveClause(List<Integer> input1, List<Integer> input2) {
		int jMax = input1.size() + input2.size();
		for(int j = 1; j < jMax; j++){
		    // i is the index of the pair associated to j.
		    int i = (j + 1) / 2 - 1;
		    List<Integer> pair = new ArrayList<Integer>(); 
		    if(j % 2 == 0) {
			pair = this.normalizedPair(input1, i + 2, input2, i);
			optimumComponent max1 = new optimumComponent(pair.toArray(new Integer[0]), true);
			max1.constitutiveClause();
			pair = this.normalizedPair(input1, i + 1, input2, i - 1);
			optimumComponent min = new optimumComponent(pair.toArray(new Integer[0]), false);
			min.constitutiveClause();
			pair.clear();
			pair.add(max1.outputs[0]);
			pair.add(min.outputs[0]);
			optimumComponent max2 = new optimumComponent(pair.toArray(new Integer[0]), true);
			max2.constitutiveClause();
			this.outputs[j] = max2.outputs[0];
		    }
		    else {
			pair = this.normalizedPair(input1, i + 1, input2, i - 1);
			optimumComponent max = new optimumComponent(pair.toArray(new Integer[0]), true);
			max.constitutiveClause();
			pair = this.normalizedPair(input1, i, input2, i - 2);
			optimumComponent min1 = new optimumComponent(pair.toArray(new Integer[0]), false);
			min1.constitutiveClause();
			pair.clear();
			pair.add(max.outputs[0]);
			pair.add(min1.outputs[0]);
			optimumComponent min2 = new optimumComponent(pair.toArray(new Integer[0]), false);
			min2.constitutiveClause();
			this.outputs[j] = min2.outputs[0];

		    }
		}
		return;
	    }


	    private ArrayList<Integer> normalizedPair(List<Integer> list1, int index1,List<Integer> list2, int index2){
		ArrayList<Integer> pair = new  ArrayList<Integer>();
		if(index1 > 0 && index1 < list1.size())
		    pair.add(list1.get(index1));
		if(index2 > 0 && index2 < list2.size())
		    pair.add(list2.get(index2));
		return pair;
	    }

	}

	class MergeComponent extends BaseComponent{
	    public MergeComponent(int nOutput){
		super();
	    }
	    
	    void constitutiveClause(){};
	    void constitutiveClause(List<ArrayList<Integer>> inputsList) {
		assert inputsList.size() == 4;
		int k = this.outputs.length;
		if(inputsList.get(1).size() == 0){
		    this.outputs = inputsList.get(0).toArray(new Integer[0]);
		    return;
		}
		if(inputsList.get(0).size() == 1){
		    SelectionComponent selcomp = new SelectionComponent(concatenate(inputsList), k);
		    selcomp.constitutiveClause();
		    return;
		}
		List<ArrayList<Integer>> inputsListOdd = new ArrayList<ArrayList<Integer>>(4);		    
		List<ArrayList<Integer>> inputsListEven = new ArrayList<ArrayList<Integer>>(4);		    
		int parity = 1;
		int sizeOdd = 0, sizeEven = 0;
		for(int i = 0, n = inputsList.size(); i < n; i++){
		    for(int entry: inputsList.get(i))
			{
			    parity++; parity %= 2;
			    if(parity == 0)
				inputsListEven.get(i).add(entry);
			    else
				inputsListOdd.get(i).add(entry);
				
			}			
		sizeOdd += inputsListOdd.get(i).size();
		sizeEven += inputsListEven.get(i).size();
		}
		
		int kOdd = sizeOdd < k/2 + 2? sizeOdd: k/2 + 2;
		int kEven = sizeEven < k/2? sizeEven: k/2;
		MergeComponent mergeOdd = new MergeComponent(kOdd);
		mergeOdd.constitutiveClause(inputsListOdd);
		
		MergeComponent mergeEven = new MergeComponent(kEven);
		mergeEven.constitutiveClause(inputsListEven);
		CombineComponent combComp = new CombineComponent(k);
		combComp.constitutiveClause(preffix(mergeOdd.outputs,kOdd).toArray(new Integer[0]), preffix(mergeEven.outputs,kEven).toArray(new Integer[0]));
		this.outputs.addAll(combComp.outputs);
		this.outputs.addAll(suffix(mergeOdd.outputs, kOdd + 1));
		this.outputs.addAll(suffix(mergeEven.outputs, kEven + 1));

		
	    }
	}


	public Circuit(int iObj){
	    this.iObj = iObj;
	    this.setBases();
	    
	}



	/**
	 *Method that ooses the base to be used
	 */

	private void setBases(){
	    this.bases = new int[]{2};
	    return;
	}

	/**
	 *get Base element i.
	 */

	private int getBase(int i){
	    if(this.base.length < i)
		return this.base[this.base.length -1];
	    else return this.base[i];
	}

	public  IVecInt expandValue(int value){
	    IVecInt result = new VecInt(new int[]{});
	    int i = 0;	
	    while(value != 0){
		int base = getBase(i);
		result.push(value % base);
		value = (value - result.get(i)*base)/base;
	    }
	    return result;
	}

	public void setPrimordialComponents( Map<Integer,ArrayList<Integer>> baseInputs) {
	    for(Entry<Integer, ArrayList<Integer>> entry : baseInputs.entrySet()) {
		int base = entry.getKey();
		ArrayList<Integer> inputs = entry.getValue();
		SelectionComponent selComp = new SelectionComponent(inputs, inputs.size(), base);
		this.primordialComponents.add(selComp);
	    }
	}
    }

    public ArrayList<Integer> suffix(List<Integer> seq, int window){
	return 	new ArrayList<Integer>(seq.subList(seq.size() - window , seq.size() - 1 ));
    }

    public ArrayList<Integer> preffix(List<Integer> seq, int window){
	return 	new ArrayList<Integer>(seq.subList(0 ,window - 1 ));
    }

    public List<Integer> concatenate(List<ArrayList<Integer>> seq){
	List<Integer> result = new ArrayList<Integer>();
	for(ArrayList<Integer> el: seq)
	    result.addAll(el);
	return result;
    }    



    /**
     *setter of each circuit in this.circuits
     */

    private void buildCircuit(int iObj){
	Circuit circuit = this.circuits[iObj];
	Objective ithObjective = this.instance.getObj(iObj);
	HashMap<Integer, Integer> weights = this.getWeights(iObj);
	List<IVecInt> digitsList = new ArrayList<IVecInt>();
	int maxNDigits = 0;
	IVecInt digits = new VecInt(new int[]{});
	Map<Integer,Integer[]> baseInputs= new HashMap<Integer, ArrayList<Integer>>();
	for(Entry<Integer, Integer> entry: weights.entrySet()){
	    int weight = entry.getValue();
	    int lit = entry.getKey();
	    digits = circuit.expandValue(weight);
	    digitsList.add(digits);
	    int nDigits = digits.size();
	    if(maxNDigits < nDigits) maxNDigits = nDigits;
	    for(int digitI = 0; digitI < maxNDigits; digitI++){
		int ithBase = circuit.getBase(digitI);
		int ithDigit = digits.get(digitI);
		while( ithDigit > 0){
		    baseInputs.get(ithBase).add(lit);
		    ithDigit--;
		}

	    }
	}
	circuit.setPrimordialComponents(baseInputs);
	    
    }
    
    private HashMap<Integer, Integer> getWeights(int iObj){
	Objective ithObjective = this.instance.getObj(iObj);
	ReadOnlyVec<Real> objectiveCoeffs = ithObjective.getSubObjCoeffs(0);
	IVecInt literals = ithObjective.getSubObjLits(0);
	IVecInt weights = new VecInt(Arrays
				     .stream(objectiveCoeffs.toArray())
				     .mapToInt(s -> s.asIntExact())
				     .toArray());
	Map<Integer, Integer> result = new HashMap<Integer, Integer>();
	while(weights.size() > 0)
	    {
		int weight = weights.last();
		weights.pop();
		int lit = literals.last();
		literals.pop();
		result.put(lit, weight);
	    }
	return result;

    }
    public boolean UpdateCurrentK(int iObj, int upperKD){return true;}
    public boolean isY(int id){return true;};
    public int getCurrentKD(int iObj){return 0;};
    public int getIObjFromY(int id){return 0;};
    public int getKDFromY(int id){return 0;};
    public int getY(int iObj, int iKD){return 0;};
    public String prettyFormatVariable(int literal)   {return "";} ;


}
