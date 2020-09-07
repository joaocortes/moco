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
import org.sat4j.moco.problem.SelectionDelimeter.Circuit.ControlledComponent;
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
	    int oldActivator;
	    int activator = 0;
	    if(buildCircuit){
		objManagers[iObj].circuit.buildCircuit();
		for(int kD = 1, n = ithObjective.getWeightDiff(); kD <= n; kD++){
		    oldActivator = activator;
	 	    activator = this.getIthObjManager(iObj).LexicographicOrder(kD);
		    if(kD > 1){
			Log.comment(6, "sequential clause");
			this.AddClause(new VecInt(new int[]{-activator, oldActivator}));
		    }

		}
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
	    this.digitalEnv = new DigitalEnv();
    	    this.circuit = new Circuit(getSolver()){
		    public void buildCircuit(){
			SortedMap<Integer, ArrayList<Integer>> baseInputs = getInputsFromWeights(iObj);
			ArrayList<Integer> inputs = new ArrayList<Integer>();
			// last base needed to expand the weights
			int ratioI = 0;
			int base = 1;
			int ratio = 1;
			int maxBase = baseInputs.lastKey();
			List<Integer> carryBits = null;
			int basesN = 1;
			do{
			    ratio = digitalEnv.getRatio(ratioI++);
			    inputs.clear();
			    ArrayList<Integer> inputsWeights = baseInputs.get(base);
			    if(carryBits != null)
				inputs.addAll(carryBits);		    
			    if(inputsWeights!=null)
				inputs.addAll(inputsWeights);
			    if(base <= maxBase || inputs.size() != 0){
				if(base > maxBase)
				    digitalEnv.setBasesN(basesN);
				carryBits =
				    buildControlledComponent(inputs.toArray(new Integer[0]), base, ratio);
			    } else{break;}
			    base *=ratio;
			    basesN++;
			}while(true);
		    

		    }

		    /**
		     *range is the exclusive upper value the unary output may represent.
		     */
		    public List<Integer> buildControlledComponent(Integer[] inputs, int base, int modN){
			DigitComponent digitComp = new DigitComponent(inputs, modN);
			digitComp.constitutiveClause();
			new ControlledComponent(base, digitComp);
			return digitComp.getCarryBits(modN);
		    }
		    public int getFreshVar1(){return getFreshVar();}
		    public boolean AddClause1(IVecInt setOfLiterals){return AddClause(setOfLiterals);}
		};
	    
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
	public Circuit getCircuit(){return this.circuit;}
	public int digitalLiteral(int base, int value){
	    ControlledComponent component = circuit.getControlledComponentBase(base);
	    if( value <= 0 || component.getOutputsSize() == 0)
		return 0;
	    int index = unaryToIndex(value);
	    if(index > circuit.getControlledComponentBase(base).getOutputsSize())  
		index = circuit.getControlledComponentBase(base).getOutputsSize() - 1;
	    return circuit.getControlledComponentBase(base).getIthOutput(index);
	}
	/**
	 *Adds the upper bound clauses  that enforce the inclusive
	 *upper limit upperLimit. Returns the blocking variables.
	 *@return blocking variables
	 *@param upperLimit inclusive upper limit
	 *@param iObj the objective index
	 */


	public int LexicographicOrder(int upperLimit){
	    DigitalNumber digits = digitalEnv.toDigital(upperLimit);
	    IteratorContiguous iterator = digits.iterator3();
	    int activator = getFreshVar();
	    IVecInt clause = new VecInt(new int[]{activator});
	    SDIndex sDIndex = new SDIndex(iObj, upperLimit);
	    librarian.putIndex(activator, sDIndex);
	    yTable[iObj][unaryToIndex(upperLimit)] = activator;
	    Log.comment(6, "Lexicographic order");
	    this.LexicographicOrderRecurse(iterator, clause);
	    AddClause(clause);
	    return activator;
	}

	private void LexicographicOrderRecurse(IteratorContiguous iterator, IVecInt clause){
	    int base = iterator.currentBase();
	    int ratio = digitalEnv.getRatio(iterator.getIBase());
	    int digit = iterator.next();
	    int lit = 0;
	    if(digit + 1 < ratio){
		lit = digitalLiteral(base, digit + 1);
		if(lit != 0){
		    clause.push(-lit);
		    AddClause(clause);
		    clause.pop();
		}
	    }
	    if(digit > 0){
		lit = digitalLiteral(base, digit);
		if(lit != 0)
		    clause.push(-lit);
	    }
	    if(iterator.hasNext())
		this.LexicographicOrderRecurse(iterator, clause);
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
	public ControlledCompIterator controlledCompIterator(){return new ControlledCompIterator();}
	public ControlledComponent getControlledComponentBase(int base){return this.controlledComponents.get(base);}

	class ControlledCompIterator implements Iterator<ControlledComponent>{
	    Iterator<Integer> current  = controlledComponents.keySet().iterator();
	    public ControlledComponent next(){
		ControlledComponent next = getControlledComponentBase(current.next());
		return next;
	    }
	    public boolean hasNext(){return current.hasNext();}
	}
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
		if(sortedPortionN > this.inputs.length)
		    sortedPortionN = this.inputs.length;
		this.sortedPortionN = sortedPortionN;
		this.outputs = new Integer[sortedPortionN];
	    }
    /**
     *Enforces a sequential relation between the variables.
     */
    public void enforceOrder(){
	Integer[] variables = this.outputs;
	int pastVariable = 0;
	if(variables.length <=1)
	    return;
	for(int variable: variables)
	    if(pastVariable != 0){
		IVecInt clause = new VecInt(new int[] {-variable, pastVariable});
	    Log.comment(6, "enforce ordering");
		AddClause1(clause);
		pastVariable = variable;
	    }

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
		if(n <= 4){
		    IVecInt clause = new VecInt(new int[]{});
		    Log.comment(6, "merge special case:");
		    this.recurseSpecialCase(0, 0, clause);
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
		Integer[] slice;
		for(int ithN: ns){
		    if(k < ithN) ki = k; else ki = ithN;
		    slice = new Integer[ithN];
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
		this.enforceOrder();
		return;
	    }
	    public void recurseSpecialCase(int depth,int first ,IVecInt clause){

		if(depth == this.outputs.length || first == this.inputs.length)
		    return;
		Integer currentOutput = this.getIthOutput(depth);
		if(currentOutput == null){
		    currentOutput = getFreshVar1();
		    this.outputs[depth] = currentOutput;
		}
		 
		for(int i = first, n = this.inputs.length;i < n; i++){
		    clause.push(-this.getIthInput(i));
		    clause.push(currentOutput);
		    AddClause1(clause);
		    clause.pop();
		    recurseSpecialCase(depth + 1, i + 1, clause);
		    clause.pop();
		}
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
	    Log.comment(6, "optimum component");
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
		this.enforceOrder();
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
		this.inputs = new Integer[jMax];
		{
		    int i = 0;
		    int n  = input1.length;
		    for(;i < n; i++)
			this.inputs[i] = input1[i];
		    for(;i < jMax; i++)		    
			this.inputs[i] = input2[i - n];
		}
	    	this.outputs = new Integer[jMax < this.sortedPortionN? jMax : this.sortedPortionN];
	    	for(int j = 0; j < jMax; j++){
	    	    // i is the index of the pair associated to j.
	    	    int i = j / 2 ;
	    	    Integer[] pair;
	    	    if(j % 2 == 1){
	    		int max1Output = OptimumOutput(input1, i + 2, input2, i, true);
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
		this.enforceOrder();
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
		int totalInputSize = 0;
		for(int i = 0; i < 4; i++)
		    totalInputSize += inputsArray[i].length;

		this.inputs = new Integer[totalInputSize];

		{ int k = 0;
		    for(int i = 0; i < 4; i++)
			for(int j = 0, n = inputsArray[i].length;j < n; j++ )
			    this.inputs[k++] = inputsArray[i][j];
		}
		Integer[][] toCombine = new Integer[2][];
		int k = this.sortedPortionN;

		if(this.sortedPortionN == 0){
		    return;
		}

		if(inputsArray[1].length == 0){
		    this.outputs = inputsArray[0];
		    return;
		}
		if(inputsArray[0].length == 1){
		    SelectionComponent selComp = new SelectionComponent(this.inputs, k);
		    selComp.constitutiveClause();
		    this.outputs = selComp.getOutputs();
		    return;
		}else{
		    // parity, index of input array, index of input literal;
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

		    toCombine[0] = mergeComp[0].getOutputs();
		    toCombine[1] = mergeComp[1].getOutputs();

		}		
		CombineComponent combComp = new CombineComponent(k);
		combComp.constitutiveClause(toCombine[0], toCombine[1]);
		this.outputs = combComp.outputs;
		this.enforceOrder();
		return;
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

		if(this.inputs.length < this.modN){
		    this.outputs = this.inputs;
		    return;
		}
		if(this.inputs.length == this.modN){
	
		    IVecInt clause = new VecInt(3);
		    int MSB = this.inputs[this.inputs.length - 1];
		    for(int i = 0, n = this.getOutputsSize(); i < n; i++){
			int input = this.inputs[i];
			int output = getFreshVar1();
			this.outputs[i] = output;
			clause.clear();
			clause.push(-input); clause.push(MSB);
	    Log.comment(6, "Mod component");
			for(int j = 0, m  = clause.size(); j < m; j++)
			    AddClause1(new VecInt(new int[]{-output, -clause.get(j)}));
			AddClause1(clause.push(output));
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
		    int i = 0; int n = this.getInputsSize();
		    while(i < n){
			Integer[] slice = new Integer[n - i > this.modN? this.modN: n - i];
			int i0 = i;
			while(i - i0  < this.modN && i < n)
			    slice[i % modN] = this.inputs[i++];
			ModComponent modComponent = new ModComponent(slice, this.modN);
			modComponent.constitutiveClause();
			// collects the k'th output into the k'th clause
			for(int k = 0; k < modComponent.getOutputsSize(); k++){
			    int lit = modComponent.getIthOutput(k);
			    if(lit !=0)
				clauses[k].push(lit);
			}		
		    }
		    for(int k = 0; k < this.modN - 1; k++){
			int output = getFreshVar1();
			this.outputs[k] = output;
	    Log.comment(6, "Mod component");
			for(int j = 0, m = clauses[k].size(); j < m ; j++)
			    AddClause1(new VecInt(new int[]{ -clauses[k].get(j), output}));
			AddClause1(clauses[k].push(-output));
			}

		}
		return;

	    }
	    
	}

	public class DigitComponent extends SortedComponent{
	    SelectionComponent selecComp;
	    ModComponent modComponent;
	    int modN;

	    public  DigitComponent(Integer[] inputs, int modN){
		super(inputs, modN - 1);
		this.modN = modN;
	    }

	    public void constitutiveClause(){
		this.selecComp = new SelectionComponent(inputs);
		this.selecComp.constitutiveClause();
		this.modComponent = new ModComponent(this.selecComp.outputs, this.modN);
		this.modComponent.constitutiveClause();
		this.outputs = this.modComponent.getOutputs();
		this.enforceOrder();
	    };

	    public ArrayList<Integer> getCarryBits(int ratio) {
		ArrayList<Integer> carryBits = new ArrayList<Integer>();
		for(int i = 0, n = this.selecComp.getOutputsSize(); i<n; i++)
		    if((i + 1) % ratio == 0)
			carryBits.add(this.selecComp.getIthOutput(i));
		return carryBits;
	    }
	}
	public  class ControlledComponent{
	    BaseComponent comp = null;
	    int base = 0;
	    
	    /**
	     *range is the exclusive upper value the unary output may represent.
	     */
	    public  ControlledComponent(int base, BaseComponent comp ){
		this.setComp(comp);
		this.setBase(base);
		this.addControledComponent();
	    }



	    public Iterator<Integer> iteratorOutputs(){return this.comp.iteratorOutputs();}
	    public void setBase(int base){this.base = base;}
	    public Integer getBase(){return this.base;}
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

    public Integer[] concatenate(Integer[][] seq){
	List<Integer> result = new ArrayList<Integer>();
	for(Integer[] array: seq)
	    for(Integer value: array)
		result.add(value);
	return result.toArray(new Integer[0]);
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

    /**
     *Pretty print the variable in literal. 
     */
    public String prettyFormatVariable(int literal){
	int sign =(literal>0)? 1: -1;
	int id =  literal * sign;

	if(isX(id)){
	    return (sign>0? "+":"-")+"X["+id+"] ";
	}
	if(this.isY(id)){
	    int iObj = this.getIObjFromY(id);
	    int kD = this.getKDFromY(id);
	    int k = kD; // + this.instance.getObj(iObj).getMinValue();
	    return "Y[" + iObj + ", " + k +"]"+ "::" + literal + " ";
	}
	return literal + " ";
    }

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

}


