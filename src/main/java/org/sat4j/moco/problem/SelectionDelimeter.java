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
import org.sat4j.core.ReadOnlyVecInt;
import org.sat4j.core.VecInt;
import org.sat4j.moco.util.Log;
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

    private ObjManager[] objManagers;
    private int[][] yTable = null;

    public SelectionDelimeter(Instance instance, PBSolver solver, boolean buildCircuit) {
	super(instance, solver);
	this.instance = instance;
	this.objManagers = new ObjManager[this.instance.nObjs()];
	this.initializeYTable();
	for(int iObj = 0, nObj = instance.nObjs() ;iObj< nObj; ++iObj){
	    this.objManagers[iObj] = new ObjManager(iObj);
	    Objective ithObjective = this.getInstance().getObj(iObj);
	    if(buildCircuit){
		objManagers[iObj].circuit.buildCircuit();
		for(int kD = 1, n = ithObjective.getWeightDiff(); kD <= n; kD++)
		    this.uglyUpperBoundClause(iObj, kD);
	    }
	}

	// Log.comment(5, "}");
    }

    /**
     * Initialize the container of the Y variables
     */
    private void initializeYTable(){
	this.yTable = new int[this.instance.nObjs()][];
	for(int iObj = 0;iObj< instance.nObjs(); ++iObj){
	    Objective ithObj = instance.getObj(iObj);
	    this.yTable[iObj] = new int[ithObj.getWeightDiff()];

	}
    }
    public SelectionDelimeter(Instance instance, PBSolver solver) {
	this(instance, solver, true);

    }

    public ObjManager getIthObjManager(int i){return this.objManagers[i];}
    static class SDIndex extends GoalDelimeter.Index{

	SDIndex(int iObj, int kD){
	    super(iObj, kD);
	}
    };

    public class ObjManager{
	int iObj;
	Circuit circuit;
	DigitalEnv digitalEnv;

	ObjManager(int iObj){
	    this.iObj = iObj;
	    
	    this.circuit = new Circuit(getSolver()){
		    public void buildCircuit(){
			SortedMap<Integer, ArrayList<Integer>> baseInputs = getInputsFromWeights(iObj);
			ControlledComponent lastContComp = null;
			ArrayList<Integer> inputs = new ArrayList<Integer>();
			// last base needed to expand the weights
			int ratioI = 0;
			int base = 1;
			int ratio = 1;
			int maxBase = baseInputs.lastKey();
			ControlledComponent contComp;
	    
			do{
			    ratio = digitalEnv.getRatio(ratioI++);
			    inputs.clear();
			    ArrayList<Integer> inputsWeights = baseInputs.get(base);
			    if(lastContComp != null)
				inputs.addAll(getCarryBits(lastContComp.getInputs(), ratio));		    
			    if(inputsWeights!=null)
				inputs.addAll(inputsWeights);
			    if(base <= maxBase || inputs.size() != 0){
				contComp =
				    buildControlledComponent(inputs.toArray(new Integer[0]), base, ratio);
				lastContComp = contComp;
			    } else{break;}
			    base *=ratio;
			}while(true);
		    

		    }
		    /**
		     *range is the exclusive upper value the unary output may represent.
		     */
		    public ControlledComponent buildControlledComponent(Integer[] inputs, int base, int range){
			SelectionComponent selecComp = new SelectionComponent(inputs);
			selecComp.constitutiveClause();
			ModComponent modComponent = new ModComponent(selecComp.outputs, range);
			modComponent.constitutiveClause();
			ControlledComponent result  = new ControlledComponent(base, modComponent);
			return result;
		    }
		    public int getFreshVar1(){return getFreshVar();}
		    public boolean AddClause1(IVecInt setOfLiterals){return AddClause(setOfLiterals);}
		};
	    this.digitalEnv = new DigitalEnv();
	    
	}


	/**
	 *Generates the inputs created by the weights of the objective
	 *function iObj
	 */
	protected SortedMap<Integer,ArrayList<Integer>> getInputsFromWeights(int iObj){
	    DigitalEnv digitalEnv = this.getDigitalEnv();
	    SortedMap<Integer,ArrayList<Integer>> baseInputs= new TreeMap<Integer, ArrayList<Integer>>();
	    HashMap<Integer, Integer> weights = getWeights(iObj);
	    List<DigitalNumber> digitsList = new ArrayList<DigitalNumber>();
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
	public int digitalLiteral(int base, int value){
	    if( value == 0)
		return 0;
	    int index = unaryToIndex(value);
	    if(index > circuit.getControlledComponentBase(base).getOutputsSize())  
		index = circuit.getControlledComponentBase(base).getOutputsSize() - 1;
	    return circuit.getControlledComponentBase(base).getIthOutput(index);
	}
    }

    static abstract public class Circuit{
	PBSolver solver;
	SortedMap<Integer, ControlledComponent> controlledComponents = null;
	
	public Circuit(PBSolver solver){
	    this.solver = solver;
	    this.controlledComponents = new TreeMap<Integer, ControlledComponent>();
	}

	abstract public int getFreshVar1();
	abstract public boolean AddClause1(IVecInt setOfLiterals);
	abstract public void buildCircuit();

	public ControlledComponent getControlledComponentBase(int base){return this.controlledComponents.get(base);}
	abstract public class BaseComponent implements Iterable<Integer>{

	    protected Integer[] inputs;
	    protected  Integer[] outputs;

	    public BaseComponent(){
	    }

	    public Integer[] getInputs(){return this.inputs;}
	    public Integer[] getOutputs(){return this.outputs;}
	    public Integer getIthInput(int i){return this.inputs[i];}
	    public Integer getIthOutput(int i){return this.outputs[i];}
	    public int getInputsSize(){return this.inputs.length;}
	    public int getOutputsSize(){return this.outputs.length;}
	    public abstract void constitutiveClause();

	    /**
	     *Iterates the inputs
	     */
	    public IteratorInputs iterator(){return new IteratorInputs();}

	    /**
	     *Iterates the outputs
	     */
	    public IteratorOutputs iteratorOutputs(){return new IteratorOutputs();}

	    class IteratorOutputs implements Iterator<Integer>{
		Iterator<Integer> iterator = Arrays.asList(outputs).iterator();
		public Integer next(){return iterator.next();}
		public boolean hasNext(){return iterator.hasNext();}
	    }
	    class IteratorInputs implements Iterator<Integer>{
		Iterator<Integer> iterator = Arrays.asList(inputs).iterator();
		public Integer next(){return iterator.next();}
		public boolean hasNext(){return iterator.hasNext();}
	    }
	}
	public abstract class SortedComponent extends BaseComponent{

	    int sortedPortionN = 0;

	    public SortedComponent(){super();}
	    public SortedComponent(int sortedPortionN){
		this.sortedPortionN = sortedPortionN;
	    }
	    public SortedComponent(Integer[] inputs, int sortedPortionN){
		super();
		this.inputs = inputs;
		this.sortedPortionN = sortedPortionN;
		this.outputs = new Integer[sortedPortionN];
	    }
	}
	public class SelectionComponent extends SortedComponent{
	    public SelectionComponent(Integer[] inputs){
		super(inputs, inputs.length);
	    }

	    public SelectionComponent(Integer[] inputs, int sortedPortionN){
		super(inputs, sortedPortionN);
	    }

	    @Override
	    public void constitutiveClause() {
	
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

		}
		MergeComponent mergecomp = new MergeComponent(k);
		mergecomp.constitutiveClause(preffixes);
		// ArrayList<Integer> outputs = new ArrayList<Integer>();
		this.outputs = mergecomp.outputs;
		return;
	    }
	}
	/**
	 *Component with 2 inputs that selects either the max or min
	 *entry, according to its polarity
	 */
	public class optimumComponent extends SortedComponent{
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
	    public void constitutiveClause() {
		this.outputs[0]=getFreshVar1();
		if(this.inputs.length == 0)
		    return;
		if(this.inputs.length == 1){
		    this.outputs[0] = this.inputs[0];
		    return;
		}
		int sign;
		if(this.polarity) sign = 1; else sign = -1;
		IVecInt clause = new VecInt();
		for(int lit: this.inputs){
		    clause = new VecInt(new int[]{-sign * lit, sign*this.outputs[0]});
		    AddClause1(clause);
		}
		clause.clear();
		clause.push(-sign*this.outputs[0]);
		for(int lit: this.inputs){
		    clause.push(sign * lit);
		}
		AddClause1(clause);
		return;
	    }

	}
	public class CombineComponent extends SortedComponent{

	    public CombineComponent(int sortedPortionN){
		this.sortedPortionN = sortedPortionN;
	    }

	    // This is required by my stupidity.
	    public void constitutiveClause(){};

	    public void constitutiveClause(Integer[] input1, Integer[] input2){
	    	int jMax = input1.length + input2.length;
	    	this.outputs = new Integer[jMax < this.sortedPortionN? jMax : this.sortedPortionN];
	    	for(int j = 0; j < jMax; j++){
	    	    // i is the index of the pair associated to j.
	    	    int i = j / 2 ;
	    	    Integer[] pair;
	    	    if(j % 2 == 1){
	    		int max1Output = OptimumOutput(input2, i + 2, input2, i, true);
	    		int minOutput = OptimumOutput(input1, i + 1, input2, i - 1, false);
			pair = new Integer[]{max1Output, minOutput};
	    		optimumComponent max2 = new optimumComponent(pair, true);
	    		max2.constitutiveClause();
	    		this.outputs[j] = max2.outputs[0];
	    	    }
	    	    else {
			int maxOutput= this.OptimumOutput(input1, i + 1, input2, i - 1, true);
	    		int min1Output = this.OptimumOutput(input1, i, input2, i - 2, false);
			pair = new Integer[]{maxOutput, min1Output};
	    		optimumComponent min2 = new optimumComponent(pair, false);
	    		min2.constitutiveClause();
	    		this.outputs[j] = min2.outputs[0];

	    	    }
	    	}
	    	return;
	    }


	    private Integer OptimumOutput(Integer[] list0, int index0,Integer[] list1, int index1, boolean polarity){
		final int identityFlag = polarity? 0: 1;
		final int constantFlag = (identityFlag + 1) % 2;
		boolean identity = false;
	    	Integer[][] lists = new Integer[2][];
	    	lists[0] = list0;
	    	lists[1] = list1;
		Integer[] indexes = new Integer[]{index0, index1};
	    	for(int i = 0; i < 2; i++){
		    int current = forcedValue(lists[i], indexes[i]);
		    if(current == constantFlag)
			return solver.constantLiteral(polarity);
		    if(current == identityFlag)
			if(identity){
			    return solver.constantLiteral(!polarity);
			}else
			    if(i == 0)
				identity = true;
			    else{
				return list0[index0];
			    }
		}
		if(identity)
		    return list1[index1];

		Integer[] inputs = new Integer[]{list0[index0], list1[index1]};
		optimumComponent optComp = new optimumComponent(inputs , polarity);
	    	optComp.constitutiveClause();
		return optComp.getIthOutput(0);
	    }

	    private int forcedValue(Integer[] list, int index){
		if(list.length <= index)
		    return 0;
		if(index < 0)
		    return 1;
		return -1;
	    }
	}
	public class MergeComponent extends SortedComponent{
	    int sortedPortionN;
	    public MergeComponent(int sortedPortionN){
		this.sortedPortionN = sortedPortionN;
	    }
	    
	    public void constitutiveClause(){};
	    public void constitutiveClause(Integer[][] inputsArray) {
		ArrayList<Integer> outputs = new ArrayList<Integer>();
		ArrayList<Integer> suffix = new ArrayList<Integer>();
		Integer[][] toCombine = new Integer[2][];

		if(this.sortedPortionN == 0)
		    return;
		int inputN = 0;
		this.outputs = new Integer[inputN];
		int k = this.sortedPortionN;
		if(inputsArray[1].length == 0){
		    this.outputs = inputsArray[0];
		    return;
		}
		if(inputsArray[0].length == 1){
		    //TODO: use the special selector, see Karpinsky,
		    //Encoding Cardinality Constraints
		    ArrayList<Integer> smallerInput1 = new ArrayList<Integer>();
		    ArrayList<Integer> smallerInput2 = new ArrayList<Integer>();
		    {
			int i = 0;
			for(; i < 1; i++)
			    smallerInput1.addAll(Arrays.asList(inputsArray[i]));
			for(; i < 4; i++)
			    smallerInput2.addAll(Arrays.asList(inputsArray[i]));
		    }

		    SelectionComponent selcomp1 = new SelectionComponent(smallerInput1.toArray(new Integer[0]), k);
		    selcomp1.constitutiveClause();
		    SelectionComponent selcomp2 = new SelectionComponent(smallerInput2.toArray(new Integer[0]), k);
		    selcomp2.constitutiveClause();
		    toCombine[0]  = selcomp1.outputs;
		    toCombine[1] = selcomp2.outputs;
		    return;
		}

		// parit, index of input array, index of input literal;
		Integer[][][] inputsListSplit = new Integer[2][4][];
		Integer[] sizes = new Integer[2];
		sizes[0] = 0; sizes[1] = 0;
		for(int i = 0; i < 4; i++){
		    int length = inputsArray[i].length;
		    inputsListSplit[0][i] = new Integer[(length + 1) / 2]; 
		    inputsListSplit[1][i] = new Integer[length / 2]; 
		    sizes[1] += inputsListSplit[1][i].length;
		    sizes[0] += inputsListSplit[0][i].length;

		    for(int j = 0, n1 = inputsArray[i].length; j < n1; j++)
			inputsListSplit[j % 2][i][j/2] = inputsArray[i][j];
		}

		
		Integer[] ks = new Integer[2];
		ks[0] = sizes[0] < k/2 + 2? sizes[0]: k/2 + 2;
		ks[1]= sizes[1] < k/2? sizes[1]: k/2;
		MergeComponent[] mergeComp = new MergeComponent[2];

		mergeComp[0] = new MergeComponent(ks[0]);
		mergeComp[0].constitutiveClause(inputsListSplit[0]);

		mergeComp[1] = new MergeComponent(ks[1]);
		mergeComp[1].constitutiveClause(inputsListSplit[1]);

		toCombine[0] = preffix(mergeComp[0].outputs,ks[0]);
		toCombine[1] = preffix(mergeComp[1].outputs,ks[1]);
		// suffix.addAll(suffix(mergeComp[1].outputs, mergeComp[1].outputs.length - ks[1]));
		// suffix.addAll(suffix(mergeComp[0].outputs, mergeComp[0].outputs.length - ks[0]));
		
		CombineComponent combComp = new CombineComponent(k);
		combComp.constitutiveClause(toCombine[0], toCombine[1]);
		outputs.addAll(Arrays.asList(combComp.outputs));
		this.outputs = outputs.toArray(new Integer[0]);
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
	}

	/**
	 *returns the Mod of the counts of 1 inputs. Assumes
	 *inputs are sorted by value.
	 */
	public class ModComponent extends BaseComponent{
	    int modN = 0;
	    public ModComponent(Integer[] inputs, int modN){
		this.modN = modN;
		this.inputs = inputs;
	    }

	    public void constitutiveClause(){
		this.outputs = new Integer[this.inputs.length < this.modN ? this.inputs.length: this.modN - 1];

		if(this.inputs.length <= this.modN){
	
		    IVecInt clause = new VecInt(3);
		    int MSB = this.inputs[this.inputs.length - 1];
		    for(int i = 0, n = this.getOutputsSize(); i < n; i++){
			int input = this.inputs[i];
			int output = getFreshVar1();
			this.outputs[i] = output;
			clause.clear();
			clause.push(-input); clause.push(output); clause.push(MSB);
			AddClause1(clause);
			//TODO: do I need the double implication?			
		    }
		    return;
		}
		// will store one conjunction for each value in the
		// range modN - 1
		IVecInt[] clauses = new VecInt[this.modN - 1];
		for(int i = 0; i < this.modN - 1; i++)
		    clauses[i] = new VecInt();

		{ 
		    int shift = 0;
		    int i = 0; int n = this.getOutputsSize();
		    while(i < n){
			Integer[] slice = new Integer[n - i > this.modN? this.modN: n - i];
			while(n - i  <= this.modN && i < n)
			    slice[i % modN] = this.inputs[i++];
			ModComponent modComponent = new ModComponent(slice, this.modN);
			modComponent.constitutiveClause();
			for(int lit: modComponent.outputs)
			    this.outputs[shift++] = lit;
			for(int k = 0; i < this.modN - 1; k++){
			    int lit = modComponent.getIthOutput(k);
			    clauses[k].push(lit);
			}		
		    }
		    for(IVecInt clause: clauses )
			AddClause1(clause);
		}
		return;

	    }
	    
	}
	public class ControlledComponent{
	    BaseComponent comp = null;
	    int base = 0;

	    /**
	     *range is the exclusive upper value the unary output may represent.
	     */
	    public ControlledComponent(int base, BaseComponent comp ){
		this.setComp(comp);
		this.setBase(base);
		this.addControledComponent();
	    }

	    public Iterator<Integer> iteratorOutputs(){return this.comp.iteratorOutputs();}
	    public void setBase(int base){this.base = base;}
	    public void setComp(BaseComponent comp){this.comp = comp;}
	    public void addControledComponent(){controlledComponents.put(this.base, this);}
	    public Integer[] getInputs(){return this.comp.getInputs();}
	    public Integer[] getOutputs(){return this.comp.getOutputs();}
	    int getInputsSize(){return this.comp.getInputsSize();}
	    int getOutputsSize(){return this.comp.getOutputsSize();}

	    public int getIthInput(int index){
		return this.comp.getIthInput(index);
	    }
	    public Integer getIthOutput(Integer index){
		return this.comp.getIthOutput(index);
	    }


	}

	
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
	return true;
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

    public int getY(int iObj, int kD){
	int index  = this.unaryToIndex(kD);
	return this.yTable[iObj][index];
    };

    public String prettyFormatVariable(int literal)   {return "";}

    /**
     * Generate the upper limit assumptions
     */
    public IVecInt generateUpperBoundAssumptions(int[] UpperKD){

	IVecInt assumptions = new VecInt(new int[]{});
	for(int iObj = 0; iObj < this.instance.nObjs(); ++iObj){
	    Objective ithObjective = this.instance.getObj(iObj);
	    if(UpperKD[iObj]  < ithObjective.getWeightDiff())
		assumptions.push(-this.getY(iObj, UpperKD[iObj] + 1));
	    
	    ReadOnlyVecInt objectiveLits = ithObjective.getSubObjLits(0);
	    ReadOnlyVec<Real> objectiveCoeffs = ithObjective.getSubObjCoeffs(0);
	    int sign = 1;
	    int ithAbsoluteWeight;
	    for(int iX = 0, nX = ithObjective.getTotalLits(); iX <nX; iX ++){
		ithAbsoluteWeight = objectiveCoeffs.get(iX).asInt();
		sign = (ithAbsoluteWeight > 0? 1 : -1);
		ithAbsoluteWeight *= sign;
		if( ithAbsoluteWeight > UpperKD[iObj])
		    assumptions.push(- sign * objectiveLits.get(iX));
	    }

	}

	return assumptions;
    }

    public int unaryToIndex(int kD){
	return kD  - 1;

    }

    /**
     *Adds the upper bound clauses  that enforce the inclusive
     *upper limit upperLimit. Returns the blocking variables.
     *@return blocking variables
     *@param upperLimit inclusive upper limit
     *@param iObj the objective index
     */
    public int uglyUpperBoundClause(int iObj, int upperLimit){
	ObjManager objManager = this.getIthObjManager(iObj);
	DigitalNumber digits = objManager.getDigitalEnv().toDigital(upperLimit);
	IteratorContiguous iterator = digits.iterator2();

	int activator = getFreshVar();
	IVecInt clause = new VecInt(new int[]{activator});
	SDIndex sDIndex = new SDIndex(iObj, upperLimit);
	librarian.putIndex(activator, sDIndex);
	this.yTable[iObj][unaryToIndex(upperLimit)] = activator;
	int base0 = 1;
	int base = 1;
	int digit = 0;
	int ratio;
	int lit = 0;
	while(iterator.hasNext()){
	    base = iterator.currentBase();
	    digit = iterator.next();
	    ratio = base/base0;
	    base0 = base;
	    if(digit + 1 < ratio){
		lit = objManager.digitalLiteral(base, digit + 1);
		clause.push(-lit);
		AddClause(clause);
		clause.pop();
	    }
	    lit = objManager.digitalLiteral(base, digit);
	    if(lit != 0){
		clause.push(-lit);
		AddClause(clause);
	    }
	}
	return 0;
    }
}


