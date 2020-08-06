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
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.ArrayList;

import java.util.Arrays;
import java.util.Map.Entry;
import java.util.HashMap;


import org.sat4j.core.ReadOnlyVec;
import org.sat4j.core.VecInt;
import org.sat4j.moco.util.Real;
import org.sat4j.moco.pb.PBSolver;
import org.sat4j.moco.problem.SelectionDelimeter.Circuit.ControlledSelectionComponent;
import org.sat4j.specs.IVecInt;


/**
 * Class with the implementation of the Selection network based encoder.
 * @author Joao O'Neill Cortes
 */

public class SelectionDelimeter extends GoalDelimeter {

    private Circuit[] circuits;

    /**
     *The inverse index map for the partial sum variables. For each
     *ID, an index is associated. I would like to 
     */

    protected InverseIndex<SDIndex>  auxVariablesInverseIndex  = null;


    public SelectionDelimeter(Instance instance, PBSolver solver) {
	// Log.comment(5, "{ GenTotalEncoder");
	super(instance, solver);
	this.auxVariablesInverseIndex = new InverseIndex<SelectionDelimeter.SDIndex>() {};
	this.instance = instance;
	this.circuits = new Circuit[this.instance.nObjs()];
	for(int iObj = 0, nObj = instance.nObjs() ;iObj< nObj; ++iObj){
	    this.circuits[iObj] = new Circuit(iObj);
	    this.buildCircuit(iObj);
	}

	// Log.comment(5, "}");
    }

    class SDIndex extends Index{
	int type = 0;
	int base = 0;
	SDIndex(int iObj, int kD, int type, int base){
	    super(iObj, kD);
	}
    };

    class Circuit{
	int iObj;
	private int[] ratios = null;
	int basesN = 0;
	SortedMap<Integer, ControlledSelectionComponent> controlledComponents = null;
	
	public Circuit(int iObj){
	    this.controlledComponents = new TreeMap<Integer, ControlledSelectionComponent>();
	    this.iObj = iObj;
	    this.setRatios();

	}

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
	    int orderedOutput;
	    public SelectionComponent(Integer[] inputs){
		super(inputs, inputs.length);
	    }

	    public SelectionComponent(Integer[] inputs, int orderedOutput){
		this.inputs = inputs;
		this.outputs = new Integer[this.inputs.length]; 
		this.orderedOutput = orderedOutput;
	    }

	    @Override
	    void constitutiveClause() {
	
		int n = this.inputs.length;
		int k = this.orderedOutput;
		int[] ns = new int[4];
	    

		if(k == 0 || n <=1){
		    this.outputs = this.inputs; 
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
			ns[i] = (n + i)/ 4;
		    }
		
		}
		else{
		    for(int i = 0; i<4; i++){
			ns[i] = k / 4;
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
		
		List<Integer> concatenatedSuffixes = new ArrayList<Integer>();
		int i = 0;
		for(int ithN: ns){
		    if(k < ithN) ki = k; else ki = ithN;
		    Integer[] slice = new Integer[ithN];
		    for(int j = 0; j < ithN; j++)
			slice[j] = this.inputs[offset + j];
		    offset+=ithN;
		    SelectionComponent selcomp = new SelectionComponent(slice, k);
		    selcomp.constitutiveClause();
		    preffixes[i++] = preffix(selcomp.outputs, ki);

		    concatenatedSuffixes.addAll(suffix(selcomp.outputs, ki));
		}
		MergeComponent mergecomp = new MergeComponent(k);
		ArrayList<ArrayList<Integer>> preffixesArray = new ArrayList<ArrayList<Integer>>();
		for(Integer[] preffix: preffixes)
		    preffixesArray.add(new ArrayList<Integer>(Arrays.asList(preffix)));
		mergecomp.constitutiveClause(preffixesArray);
		ArrayList<Integer> outputs = new ArrayList<Integer>();
		outputs.addAll(Arrays.asList(mergecomp.outputs));
		outputs.addAll(concatenatedSuffixes);
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
		
		Integer[] completeInputs = new Integer[2 * this.realInputs.length];
		for(int i = 0, n = this.realInputs.length; i < n; i++){
		    completeInputs[i] = this.realInputs[i];
		    completeInputs[i + n] = this.auxiliaryInputs[i];
		}
		this.selecComp = new SelectionComponent(completeInputs);
		this.selecComp.constitutiveClause();
		this.setOutputs(this.selecComp.outputs);
		controlledComponents.put(base, this);
	    }
	    public void setInputs(Integer[] realInputs){
		this.realInputs = realInputs;
		this.auxiliaryInputs = new Integer[this.realInputs.length];
		for(int i = 0, n = this.auxiliaryInputs.length; i < n ; i++)
		    this.auxiliaryInputs[i] = getFreshVar();

	    }

	    
	    public void setOutputs(Integer[] outputs) {
		this.outputs = outputs;

		for(int outputI = 0, n = this.outputs.length;outputI < n; outputI++ ){

		    int lit = this.outputs[outputI];
		    auxVariablesInverseIndex.putIndex(lit, new SDIndex(iObj, outputI + 1, 1, base));
		}
	    }
	    public Integer getOutput(Integer kD){
		int index = kD - 1;
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

	    void constitutiveClause(Integer[] input1, Integer[] input2) {
		int jMax = input1.length + input2.length;
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

	    public MergeComponent(int nOutput){
		super(nOutput);
	    }
	    
	    void constitutiveClause(){};
	    void constitutiveClause(List<ArrayList<Integer>> inputsList) {
		assert inputsList.size() == 4;
		Integer[][] inputsArray = new Integer[4][];
		int iInput = 0;
		for(ArrayList<Integer> input: inputsList)
		    inputsArray[iInput++] = input.toArray(new Integer[0]);


		int k = this.outputs.length;
		if(inputsArray[1].length == 0){
		    this.outputs = inputsArray[0];
		    return;
		}
		for(int i = 0, n = inputsArray.length; i < n; i++){
		    for(Integer a: inputsArray[i])
			System.out.println(a);
			}


		if(inputsArray[0].length == 1){
		    SelectionComponent selcomp = new SelectionComponent(concatenate(inputsArray), k);
		    selcomp.constitutiveClause();
		    return;
		}
		List<ArrayList<Integer>> inputsListOdd = new ArrayList<ArrayList<Integer>>(4);		    
		List<ArrayList<Integer>> inputsListEven = new ArrayList<ArrayList<Integer>>(4);		    
		for(int i = 0; i < 4; i++){
		    inputsListOdd.add(new ArrayList<Integer>());
		    inputsListEven.add(new ArrayList<Integer>());
}
		int parity = 1;
		int sizeOdd = 0, sizeEven = 0;
		for(int i = 0, n = inputsArray.length; i < n; i++){
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
		CombineComponent combComp = new CombineComponent(k);
		combComp.constitutiveClause(preffix(mergeOdd.outputs,kOdd), preffix(mergeEven.outputs,kEven));
		ArrayList<Integer> outputs = new ArrayList<Integer>();
		outputs.addAll(Arrays.asList(combComp.outputs));
		outputs.addAll(suffix(mergeOdd.outputs, kOdd + 1));
		outputs.addAll(suffix(mergeEven.outputs, kEven + 1));
		this.outputs = outputs.toArray(new Integer[0]);
	    }
	}





	/**
	 *Method that ooses the base to be used
	 */

	private void setRatios(){
	    this.ratios = new int[]{2};
	    return;
	}

	private int getRatio(int i){
	    if(this.ratios.length <= i)
		return this.ratios[this.ratios.length -1];
	    else return this.ratios[i];
	}
	/**
	 *get Base element i.
	 */

	private int getBase(int index){
	    int result = 1;
	    for(int j = 0; j <= index; j ++ )
		result*= getRatio(j);
	    return result;		
	}

	/**
	 *get the index of the base. If not a valid base, returns -1.
	 */

	private int getBaseI(int base){
	    int i = 0;
	    int candidate = 1;
	    while(candidate < base)
		candidate *= this.getRatio(i);
	    if(candidate == base)
		return i--;
	    else
		return -1;
	}


	public ArrayList<Integer> getCarryBits(ControlledSelectionComponent select1, int ratio) {
	    ArrayList<Integer> carryBits = new ArrayList<Integer>();
	    for(int i = 0, n = select1.selecComp.nOutputs; i<n; i++)
		if((i + 1) % ratio == 0)
		    carryBits.add(select1.outputs[i]);
	    return carryBits;
	}


	public void setControlledComponents( SortedMap<Integer, ArrayList<Integer>> baseInputs) {
	    this.basesN = baseInputs.size();
	    ControlledSelectionComponent lastContComp = null;
	    
	    int iBase = 1; int lastIBase = iBase;
	    ArrayList<Integer> inputs = new ArrayList<Integer>();
	    // last base needed to expand the weights
	    int maxBase = baseInputs.lastKey();
	    do{
		Integer base = this.getBase(iBase++);
		Integer lastBase = this.getBase(lastIBase++);
		if(inputs != null)
		    inputs.clear();
		int ratio = base / lastBase;
		ArrayList<Integer> inputsWeights = baseInputs.get(base);
		if(lastContComp != null)
		    inputs.addAll(getCarryBits(lastContComp, ratio));		    
		if(inputsWeights!=null)
		    inputs.addAll(inputsWeights);
		if(base <= maxBase || inputs.size() > 0){
		    ControlledSelectionComponent contComp =
			new ControlledSelectionComponent(inputs.toArray(new Integer[0]), base);
		    lastContComp = contComp;
		    lastBase = base;
		} else
		    break;
	    }while(true);

	}
	
	public int getNextValidRoof(int upperLimit){
	    IVecInt digits = this.expandValue(upperLimit);
	    int MSBase = this.getBase(digits.size());
	    int MSDigit = digits.last();
	    int MSRange = this.ratios[this.getBaseI(MSBase)] ;
	    assert(MSDigit < MSRange - 1);
	    return MSBase * (MSDigit + 1);
	}


    public IVecInt  expandValue(int value){

	IVecInt digits = new VecInt(new int[]{});
	int i = 0;
	while(value != 0){
	    int base = getBase(i);
	    int digit = value % base;
	    digits.push(digit);
	    value = (value - digit*base)/base;
	}

	return digits;
    }

    } 

    public ArrayList<Integer> suffix(Integer[] seq, int window){
	ArrayList<Integer> result = new ArrayList<Integer>(window);
	for(int i = 0; i< window; i++)
	    result.add(seq[seq.length - window + i]);
	return result;
    }

    public Integer[] preffix(Integer[] seq, int window){
	assert( window <= seq.length);
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



    /**
     *setter of each circuit in this.circuits
     */

    private void buildCircuit(int iObj){
	Circuit circuit = this.circuits[iObj];
	HashMap<Integer, Integer> weights = this.getWeights(iObj);
	List<IVecInt> digitsList = new ArrayList<IVecInt>();
	int maxNDigits = 0;
	IVecInt digits = new VecInt(new int[]{});
	SortedMap<Integer,ArrayList<Integer>> baseInputs= new TreeMap<Integer, ArrayList<Integer>>();
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
		    if(baseInputs.containsKey(ithBase))
			baseInputs.get(ithBase).add(lit);
		    else
			baseInputs.put(ithBase, new ArrayList<Integer>(Arrays.asList(new Integer[]{lit})));
		    ithDigit--;
		}

	    }
	}
	circuit.setControlledComponents(baseInputs);
	    
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
	SDIndex index = auxVariablesInverseIndex.getIndex(id);
	int iObj = index.getIObj();
	return index.base == this.circuits[iObj].controlledComponents.lastKey();
    }
    public int getCurrentKD(int iObj){return 0;};
    public int getIObjFromY(int id){
	SDIndex index = this.auxVariablesInverseIndex.getIndex(id);
	if(index!=null)
	    return index.getIObj();
	return 0;};

    public int getKDFromY(int id){
	SDIndex index = this.auxVariablesInverseIndex.getIndex(id);
	if(index!=null)
	    return index.getKD();
	return 0;};

    public int getY(int iObj, int iKD){
	Circuit circuit = this.circuits[iObj];
	ControlledSelectionComponent controlledComp =  circuit.controlledComponents.get(circuit.controlledComponents.lastKey());
	return controlledComp.outputs[iKD];
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
	    int nextValidRoof = circuit.getNextValidRoof(upperLimit);
	    int diff = nextValidRoof - upperLimit - 1;
	    IVecInt digits = new VecInt(new int[circuit.basesN]);
	    digits = circuit.expandValue(diff);
	    // add the desired constant, through the assumptions
	    for(int i = 0, digitsN = circuit.basesN; i<digitsN; i++){
		int base = circuit.getBase(i);
		ControlledSelectionComponent contComp = circuit.controlledComponents.get(base);
		assumptions.push(contComp.auxiliaryInputs[digits.get(i)]);
	    }
	    //add the upper limit restriction, through the MSDigit.
	    int MSBBase = circuit.controlledComponents.lastKey();
	    ControlledSelectionComponent MSBContComp = circuit.controlledComponents.get(MSBBase);
	    assumptions.push(-MSBContComp.getOutput(digits.last() + 1));

}
	return assumptions;
	
	
	
    }
    

}
