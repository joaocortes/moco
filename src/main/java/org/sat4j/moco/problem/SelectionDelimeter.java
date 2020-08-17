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

import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.ArrayList;

import java.util.Arrays;
import java.util.Map.Entry;
import java.util.HashMap;
import java.util.Iterator;


import org.sat4j.core.ReadOnlyVec;
import org.sat4j.core.VecInt;
import org.sat4j.moco.util.Real;
import org.sat4j.moco.pb.PBSolver;
import org.sat4j.moco.problem.DigitalEnv.DigitalNumber;
import org.sat4j.moco.problem.DigitalEnv.DigitalNumber.IteratorContiguous;
import org.sat4j.specs.IVecInt;

/**
 * Class with the implementation of the Selection network based encoder.
 * @author Joao O'Neill Cortes
 */

public class SelectionDelimeter extends GoalDelimeter<SelectionDelimeter.SDIndex> {

    private Circuit[] circuits;
    
    public SelectionDelimeter(Instance instance, PBSolver solver, boolean buildCircuit) {
	super(instance, solver);
	this.instance = instance;
	this.circuits = new Circuit[this.instance.nObjs()];
	for(int iObj = 0, nObj = instance.nObjs() ;iObj< nObj; ++iObj){
	    Circuit circuit  = new Circuit(iObj);
	    this.circuits[iObj] = circuit;
	    if(buildCircuit)
		circuit.buildCircuit();
	}

	// Log.comment(5, "}");
    }

    public SelectionDelimeter(Instance instance, PBSolver solver) {
	this(instance, solver, true);

    }

    public Circuit getIthCircuit(int i){return this.circuits[i];}
    static class SDIndex extends GoalDelimeter.Index{

	private int type = 0;
	private int base = 0;

	SDIndex(int iObj, int kD, int type, int base){
	    super(iObj, kD);
	    this.type = type;
	    this.base = base;
	}
	int getType(){return this.type;}
	int getBase(){return this.base;}
    };

    class Circuit{
	
	
	int iObj;
	DigitalEnv digitalEnv;

	SortedMap<Integer, ControlledSelectionComponent> controlledComponents = null;
	
	public Circuit(int iObj){
	    this.controlledComponents = new TreeMap<Integer, ControlledSelectionComponent>();
	    this.iObj = iObj;
	    digitalEnv = new DigitalEnv();

	}
	/**
	 *setter of each circuit in this.circuits
	 */

	public void buildCircuit(){
	    this.setControlledComponents(getInputsFromWeights(this.iObj));
	    
	}
    
	public int getNextValidRoof(int upperLimit){
	    DigitalNumber digits = this.getDigitalEnv().toDigital(upperLimit);
	    int basesN = this.getDigitalEnv().getBasesN();
	    for(int digit: digits)
	    	System.out.println(digit);
	    int MSDigit = this.getDigitalEnv().getMSB(digits);
	    int MSRange = this.digitalEnv.getRatio(this.getDigitalEnv().getBasesN());
	    assert(MSDigit <= MSRange - 1);
	    int MSBase = this.getDigitalEnv().getBase(basesN - 1);
	    return MSBase * (MSDigit + 1);
	}

	public DigitalEnv getDigitalEnv(){return this.digitalEnv;}
	abstract class BaseComponent {

	    protected Integer[] inputs;
	    protected  Integer[] outputs;
	    int sortedPortionN = 0;

	    public BaseComponent(){
	    }
	    public BaseComponent(int sortedPortionN){
		this.sortedPortionN = sortedPortionN;
		    
	    }
	    public BaseComponent(Integer[] inputs, int sortedPortionN){
		super();
		this.inputs = inputs;
		this.outputs = new Integer[inputs.length];
		if(this.outputs.length > sortedPortionN) 
		    this.sortedPortionN = sortedPortionN;
		else
		    this.sortedPortionN = this.outputs.length;
	    }

	    abstract void constitutiveClause();

	}
	class SelectionComponent extends BaseComponent{
	    public SelectionComponent(Integer[] inputs){
		super(inputs, inputs.length);
	    }

	    public SelectionComponent(Integer[] inputs, int sortedPortionN){
		super(inputs, sortedPortionN);
	    }

	    @Override
	    void constitutiveClause() {
	
		int n = this.inputs.length;
		int k = this.sortedPortionN;
		int[] ns = new int[4];
	    

		if(k == 0 || n <=1){
		    for(int i = 0; i < k && i < n; i++)
			this.outputs[i] = this.inputs[i];
		    return;
		}
		if(k==1){
		    optimumComponent sonoc = new optimumComponent(this.inputs);
		    sonoc.constitutiveClause();
		    this.outputs = sonoc.outputs;
		    return;
		}
		if(n < 8 || k == n){
		    for(int i = 0; i<4; i++){
			ns[i] = (n + 3 - i )/ 4;
		    }
		
		}
		else{
		    ns[0] = n;
		    for(int i = 1; i<4; i++){
			ns[i] = n / 4;
			ns[0]-=ns[i];
		    }

		}
		// else{
		//     // notice (k + 3) /4 is equivalent to floor((double)k / 4), if k is int.
		//     int tailValue;
		//     int largestPower  = (int) Math.pow(2 , Math.ceil(Math.log((k + 3 )/4)));
		//     if(largestPower <= n / 4)
		// 	tailValue = largestPower;
		//     else
		// 	tailValue = k / 4;
		//     for(int i = 1; i< 4; i++)
		// 	ns[i] =  tailValue;
		//     ns[0] = n - 3 * tailValue;
		    
		// }
		int offset = 0;
		int ki;

		Integer[][] preffixes = new Integer[4][];
		
		// List<Integer> concatenatedSuffixes = new ArrayList<Integer>();
		int i = 0;
		for(int ithN: ns){
		    if(k < ithN) ki = k; else ki = ithN;
		    Integer[] slice = new Integer[ithN];
		    for(int j = 0; j < ithN; j++)
			slice[j] = this.inputs[offset + j];
		    offset+=ithN;
		    SelectionComponent selcomp = new SelectionComponent(slice, ki);
		    selcomp.constitutiveClause();
		    preffixes[i++] = selcomp.outputs;

		    // concatenatedSuffixes.addAll(suffix(selcomp.outputs, ki));
		}
		MergeComponent mergecomp = new MergeComponent(k);
		ArrayList<ArrayList<Integer>> preffixesArray = new ArrayList<ArrayList<Integer>>();
		for(Integer[] preffix: preffixes)
		    preffixesArray.add(new ArrayList<Integer>(Arrays.asList(preffix)));
		mergecomp.constitutiveClause(preffixesArray);
		ArrayList<Integer> outputs = new ArrayList<Integer>();
		outputs.addAll(Arrays.asList(mergecomp.outputs));
		// outputs.addAll(concatenatedSuffixes);
		return;
	    }
	}
	class ControlledSelectionComponent{
	    SelectionComponent selecComp = null;
	    int base = 0;
	    Integer[] realInputs = null;
	    Integer[] outputs = null;
	    Integer[] auxiliaryInputs = null;

	    public ControlledSelectionComponent(Integer[] inputs, int base){
		this.base = base;
		this.setInputs(inputs);
		
		Integer[] completeInputs = new Integer[this.realInputs.length + this.auxiliaryInputs.length];
		int i = 0;
		int n = this.realInputs.length;
		int m = this.auxiliaryInputs.length;
		for(; i < n; i++)
		    completeInputs[i] = this.realInputs[i];
		for(; i < m + n; i++)
		    completeInputs[i] = this.auxiliaryInputs[i - n];

		this.selecComp = new SelectionComponent(completeInputs);
		this.selecComp.constitutiveClause();
		this.setOutputs(this.selecComp.outputs);
		controlledComponents.put(base, this);
	    }
	    public void setInputs(Integer[] realInputs){
		this.realInputs = realInputs;
		int ratio = digitalEnv.getRatio(digitalEnv.getBaseI(base));
		this.auxiliaryInputs = new Integer[ratio - 1];
		for(int i = 0, n = this.auxiliaryInputs.length; i < n ; i++)
		    this.auxiliaryInputs[i] = getFreshVar();
		enforceOrder(this.auxiliaryInputs);

	    }

	    
	    public void setOutputs(Integer[] outputs) {
		this.outputs = outputs;
		enforceOrder(this.outputs);
		for(int outputI = 0, n = this.outputs.length;outputI < n; outputI++ ){
		    int lit = this.outputs[outputI];
		    librarian.putIndex(lit, new SDIndex(iObj, outputI + 1, 1, base));
		}
	    }
	    public Integer getOutput(Integer kD){
		int index = kD;
		return this.outputs[index];
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
		if(this.inputs.length == 0)
		    return;
		int sign;
		if(this.polarity) sign = 1; else sign = -1;
		for(int lit: this.inputs){
		    IVecInt clause = new VecInt(new int[]{-sign * lit, this.outputs[0]});
		    AddClause(clause);
		}

		return;
	    }

	}
	class CombineComponent extends BaseComponent{

	    public CombineComponent(int sortedPortionN){
		this.sortedPortionN = sortedPortionN;
	    }

	    // This is required by my stupidity.
	    void constitutiveClause(){};

	    void constitutiveClause(Integer[] input1, Integer[] input2) {
		int jMax = input1.length + input2.length;
		this.outputs = new Integer[jMax < this.sortedPortionN? jMax : this.sortedPortionN];
		for(int j = 0; j < jMax; j++){
		    // i is the index of the pair associated to j.
		    int i = (j + 1) / 2 ;
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


	    private ArrayList<Integer> normalizedPair(Integer[] list1, int index1,Integer[] list2, int index2){
		ArrayList<Integer> pair = new  ArrayList<Integer>();
		if(index1 > 0 && index1 < list1.length)
		    pair.add(list1[index1]);
		if(index2 > 0 && index2 < list2.length)
		    pair.add(list2[index2]);
		return pair;
	    }

	}
	class MergeComponent extends BaseComponent{
	    int sortedPortionN;
	    public MergeComponent(int sortedPortionN){
		this.sortedPortionN = sortedPortionN;
	    }
	    
	    void constitutiveClause(){};
	    void constitutiveClause(List<ArrayList<Integer>> inputsList) {

		assert inputsList.size() == 4;
		
		ArrayList<Integer> outputs = new ArrayList<Integer>();
		ArrayList<Integer> suffix = new ArrayList<Integer>();
		Integer[] toCombine1;
		Integer[] toCombine2;
		if(this.sortedPortionN == 0)
		    return;
		Integer[][] inputsArray = new Integer[4][];
		int iInput = 0;
		int inputN = 0;
		for(ArrayList<Integer> input: inputsList){
		    inputN+=input.size();
		    inputsArray[iInput++] = input.toArray(new Integer[0]);

		}
		this.outputs = new Integer[inputN];
		int k = this.sortedPortionN;
		if(inputsArray[1].length == 0){
		    this.outputs = inputsArray[0];
		    return;
		}

		if(inputsArray[0].length == 1){
		    
		    ArrayList<Integer> smallerInput1 = new ArrayList<Integer>();
		    ArrayList<Integer> smallerInput2 = new ArrayList<Integer>();

		    for(int i = 0; i < 1; i++)
			smallerInput1.addAll(inputsList.get(i));
		    for(int i = 3; i < 4; i++)
			smallerInput2.addAll(inputsList.get(i));
		    SelectionComponent selcomp1 = new SelectionComponent(smallerInput1.toArray(new Integer[0]), 1);
		    selcomp1.constitutiveClause();
		    SelectionComponent selcomp2 = new SelectionComponent(smallerInput2.toArray(new Integer[0]), k);
		    selcomp1.constitutiveClause();
		    toCombine1 = selcomp1.outputs;
		    toCombine2 = selcomp2.outputs;
		}else{
		    List<ArrayList<Integer>> inputsListOdd = new ArrayList<ArrayList<Integer>>(4);		    
		    List<ArrayList<Integer>> inputsListEven = new ArrayList<ArrayList<Integer>>(4);		    
		    for(int i = 0; i < 4; i++){
			inputsListOdd.add(new ArrayList<Integer>());
			inputsListEven.add(new ArrayList<Integer>());
		    }
		    int parity = 1;
		    int sizeOdd = 0, sizeEven = 0;
		    for(int i = 0, n = inputsArray.length; i < n; i++){
			parity = 1;
			for(Integer entry: inputsArray[i])
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
		    toCombine1 = preffix(mergeOdd.outputs,kOdd);
		    toCombine2 = preffix(mergeEven.outputs,kEven);
		    suffix.addAll(suffix(mergeOdd.outputs, mergeOdd.outputs.length - kOdd));
		    suffix.addAll(suffix(mergeEven.outputs, mergeEven.outputs.length - kEven));
		}
		CombineComponent combComp = new CombineComponent(k);
		combComp.constitutiveClause(toCombine1, toCombine2);
		outputs.addAll(Arrays.asList(combComp.outputs));
		this.outputs = outputs.toArray(new Integer[0]);
	    }
	}







	public void setControlledComponents( SortedMap<Integer, ArrayList<Integer>> baseInputs) {
	    // ControlledSelectionComponent lastContComp = null;
	    // ArrayList<Integer> inputs = new ArrayList<Integer>();
	    // // last base needed to expand the weights
	    // int ratioI = 0;
	    // int base = 1;
	    // int ratio = 1;

	    // do{
	    // 	base *=ratio;
	    // 	inputs.clear();
	    // 	ArrayList<Integer> inputsWeights = baseInputs.get(base);
	    // 	if(lastContComp != null)
	    // 	    inputs.addAll(getCarryBits(lastContComp.outputs, ratio));		    
	    // 	if(inputsWeights!=null)
	    // 	    inputs.addAll(inputsWeights);
	    // 	if(base <= maxBase || inputs.size() > 0){
	    // 	    ControlledSelectionComponent contComp =
	    // 		new ControlledSelectionComponent(inputs.toArray(new Integer[0]), base);
	    // 	    lastContComp = contComp;
	    // 	} else
	    // 	    break;
	    // 	ratio = this.getRatio(ratioI++);
	    // }while(true);

	}
	



	/**
	 *Enforces a sequential relation between the variables.
	 */
	public void enforceOrder(Integer[] variables){
	    int pastVariable = 0;
	    if(variables.length <=1)
		return;
	    
	    for(int variable: variables)
		if(pastVariable != 0){
		    IVecInt clause = new VecInt(new int[] {-variable, pastVariable});
		    AddClause(clause);
		    pastVariable = variable;
		}

	}

    } 

    public ArrayList<Integer> suffix(Integer[] seq, int window){
	ArrayList<Integer> result = new ArrayList<Integer>(window);
	for(int i = 0; i< window; i++)
	    result.add(seq[seq.length - window + i ]);
	return result;
    }

    public Integer[] preffix(Integer[] seq, int window){
	if(window > seq.length)
	    return new Integer[0];
	Integer[] result = new Integer[window];
	for(int i = 0; i< window; i++)
	    result[i] = seq[i];

	return result;
    }

    public Integer[] concatenate(Integer[][] seq){
	List<Integer> result = new ArrayList<Integer>();
	for(Integer[] array: seq)
	    for(Integer value: array)
		result.add(value);
	return result.toArray(new Integer[0]);
    }    

    public ArrayList<Integer> getCarryBits(Integer[] outputs, int ratio) {
	ArrayList<Integer> carryBits = new ArrayList<Integer>();
	for(int i = 0, n = outputs.length; i<n; i++)
	    if((i + 1) % ratio == 0)
		carryBits.add(outputs[i]);
	return carryBits;
    }


    /**
     *Generates the inputs created by the weights of the objective
     *function iObj
     */
    protected SortedMap<Integer,ArrayList<Integer>> getInputsFromWeights(int iObj){
	Circuit circuit = this.circuits[iObj];
	DigitalEnv digitalEnv = circuit.getDigitalEnv();
	SortedMap<Integer,ArrayList<Integer>> baseInputs= new TreeMap<Integer, ArrayList<Integer>>();
	HashMap<Integer, Integer> weights = this.getWeights(iObj);
	List<DigitalNumber> digitsList = new ArrayList<DigitalNumber>();
	int maxNDigits = 0;
	// IVecInt digits = new VecInt(new int[]{});
	for(Entry<Integer, Integer> entry: weights.entrySet()){
	    int weight = entry.getValue();
	    boolean weightSign = weight > 0;
	    int lit = entry.getKey();
	    DigitalNumber digits = digitalEnv.toDigital(weightSign? weight: -weight);
	    digitsList.add(digits);
	    // if(maxNDigits < nDigits) maxNDigits = nDigits;
	    DigitalNumber.IteratorContiguous iterator = digits.iterator2();

	    int ithDigit = 0;
	    int ithBase = 1;
	    while(iterator.hasNext())
		{
		    ithBase = iterator.currentBase();
		    ithDigit = iterator.next();
		    while( ithDigit > 0){
			if(baseInputs.containsKey(ithBase))
			    baseInputs.get(ithBase).add(weightSign? lit: -lit);
			else
			    baseInputs.put(ithBase, new ArrayList<Integer>(Arrays.asList(new Integer[]{weightSign? lit: -lit})));
			ithDigit--;
		    }

		}
	}
	return baseInputs;

    }
    private HashMap<Integer, Integer> getWeights(int iObj){
	HashMap<Integer, Integer> result = new HashMap<Integer, Integer>();
	Objective ithObjective = this.instance.getObj(iObj);
	ReadOnlyVec<Real> objectiveCoeffs = ithObjective.getSubObjCoeffs(0);
	IVecInt literals = ithObjective.getSubObjLits(0);
	for(int i = 0, n = objectiveCoeffs.size(); i < n; i++)
	    {
		int weight = objectiveCoeffs.get(i).asIntExact();
		int lit = literals.get(i);
		result.put(lit, weight);
	    }
	return result;

    }

    /**
     * This delimeter is not incremental. Therefore, this is a trivial
     * operation.
     */
    public boolean UpdateCurrentK(int iObj, int upperKD){return true;}

    public boolean isY(int id){
	SDIndex index = librarian.getIndex(id);
	if(index == null) return false;
	int iObj = index.getIObj();
	return index.base == this.circuits[iObj].controlledComponents.lastKey();
    }
    public int getCurrentKD(int iObj){return 0;};
    public int getIObjFromY(int id){
	SDIndex index = this.librarian.getIndex(id);
	if(index!=null)
	    return index.getIObj();
	return 0;};

    public int getKDFromY(int id){
	SDIndex index = this.librarian.getIndex(id);
	if(index!=null)
	    return index.getKD();
	return 0;};

    public int getY(int iObj, int iKD){

	int index  = this.kDToIndex(iKD);
	Circuit circuit = this.circuits[iObj];
	Circuit.ControlledSelectionComponent controlledComp =  circuit.controlledComponents.get(circuit.controlledComponents.lastKey());
	return controlledComp.outputs[index];
    };

    public String prettyFormatVariable(int literal)   {return "";} ;


    /**
     *UpperKD contains the inclusive upper limits of the truncated
     *domain. Shall return the assumptions necessary to enforce the
     *truncation.
     */

    public IVecInt generateUpperBoundAssumptions(int[] UpperKD){
	IVecInt assumptions = new VecInt(new int[]{});
	for(int iObj = 0, n = this.instance.nObjs(); iObj < n; iObj++) {
	    int upperLimit = UpperKD[iObj];
	    Circuit circuit = this.circuits[iObj];
	    DigitalEnv ithDigitalEnv = circuit.getDigitalEnv();
	    int nextValidRoof = circuit.getNextValidRoof(upperLimit);
	    int diff = nextValidRoof - upperLimit;
	    DigitalNumber digits = ithDigitalEnv.toDigital(diff);
	    // add the desired constant, through the assumptions
	    int base = 1;
	    DigitalNumber.IteratorJumps iterator = digits.iterator();
	    while(iterator.hasNext()){
		base = iterator.currentBase();
		int digit = iterator.next();
		if(digit != 0){
	    	Circuit.ControlledSelectionComponent contComp = circuit.controlledComponents.get(base);
		int index = this.kDToIndex(digit);
	    	assumptions.push(contComp.auxiliaryInputs[index]);} 
	    }
	    Circuit.ControlledSelectionComponent MSBContComp = circuit.controlledComponents.get(base);
	    int MSDigit = digits.getMSB();
	    assert MSDigit != 0;
	    int index = this.kDToIndex(MSDigit);
	    assumptions.push(-MSBContComp.getOutput(index));

	}
	return assumptions;
	
	
 
	
    }

    public int kDToIndex(int kD){
	return kD  - 1;

    }

    public IVecInt uglyUpperBoundClause(int iObj, int upperLimit){
	Circuit circuit = this.getIthCircuit(iObj);
	DigitalNumber digits = circuit.getDigitalEnv().toDigital(upperLimit);
	IteratorContiguous iterator = digits.iterator2();
	IVecInt clause = new VecInt(new int[]{});
	int range = 0;
	int i = 0;
	int currentBase = 1;
	int currentDigit = 0;
	int equalVar = 0;
	int upperVar = 0;
	while(iterator.hasNext() && i <= range)
	    {
		if(i < range){
		    currentBase = iterator.currentBase();
		    currentDigit = iterator.next();
		    clause.push(i);

		    upperVar = circuit.getControlledComponentBase(currentBase).getOutput(this.kDToIndex(currentDigit + 1))
			}
		if(i == range && iterator.hasNext()){
		    iterator = digits.iterator2();		    
		    range++;
		    i = 0;
		    clause.clear();
		}
	    }
	    }

    }
}
