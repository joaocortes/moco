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
import java.util.ArrayList;

import java.util.PriorityQueue;
    
import java.util.Arrays;
import java.util.TreeMap;
import java.util.Collection;


import org.sat4j.core.ReadOnlyVec;
import org.sat4j.core.VecInt;
import org.sat4j.moco.util.Real;
import org.sat4j.moco.pb.PBSolver;
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
	int[] base = null;


	abstract class BaseComponent {

	    protected List<Integer> inputs;
	    protected List<Integer> outputs;


	    public BaseComponent(){
		this.inputs = new ArrayList<Integer>();
		this.outputs =new ArrayList<Integer>();
	    }

	    public BaseComponent(List<Integer> inputs, int nOutput){
		this.inputs = inputs;
		this.outputs = new ArrayList<Integer>(nOutput);
	    }

	    abstract void constitutiveClause();

	}



	class SelectionComponent extends BaseComponent{

	    public SelectionComponent(List<Integer> inputs, int nOutput){
		super(inputs, nOutput);
	    }
	    @Override
	    void constitutiveClause() {
	
		int n = this.inputs.size();
		int k = this.outputs.size();
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
		List<ArrayList<Integer>> suffixes = new ArrayList<ArrayList<Integer>>();
		for(int ni: ns){
		    ki = k < ni? k : ni;
		    List<Integer> slice = this.inputs.subList(offset, offset + ni - 1);
		    SelectionComponent selcomp = new SelectionComponent(slice, ki);
		    preffixes.add(preffix(selcomp.outputs, ki));
		    suffixes.add(suffix(selcomp.outputs, ki + 1));
		}
		MergeComponent mergecomp = new MergeComponent(suffixes, k);
		mergecomp.constitutiveClause();
		return;
	    }
	}


	/**
	 *Component that selects the max value. Equivalent to a
	 *disjunction
	 */

	class optimumComponent extends BaseComponent{
	    boolean polarity = true;
	    public optimumComponent(List<Integer> inputs, boolean polarity){
		super(inputs, 1);
		this.polarity = polarity;
		this.outputs =  new ArrayList<Integer>(1);
	    }

	    @Override
	    void constitutiveClause() {
		this.outputs.add(getFreshVar());
		int[] intArray = null;
		// polarity, then use a max component, else use min
		    if(this.polarity)
			intArray = Arrays.stream(this.inputs.toArray(new Integer[0])).mapToInt(Integer::intValue).toArray();
		    else
			intArray = Arrays.stream(this.inputs.toArray(new Integer[0])).mapToInt(i -> -i).toArray();
		    for(int lit: intArray){
			IVecInt clause = new VecInt(new int[]{-lit, this.outputs.get(0)});
			AddClause(clause);
		    }

		return;
	    }

	}

	

	class CombineComponent extends BaseComponent{
	    List<Integer> input1;
	    List<Integer> input2;
	    public CombineComponent(List<Integer> input1, List<Integer> input2, int nOutput){
		this.outputs = new ArrayList<Integer>(nOutput);
		this.input1 = input1;
		this.input2 = input2;
	    }
	    @Override
	    void constitutiveClause() {
		// TODO Auto-generated method stub
		int jMax = input1.size() + input2.size();
		for(int j = 1; j < jMax; j++){
		    // i is the index of the pair associated to j.
		    int i = (j + 1) / 2;
		    if(j % 2 == 0) {
			List<ArrayList<Integer>> pairs = new ArrayList<ArrayList<Integer>>(4); 
			pairs.add()
			
		    }

		}
	    
	    }
	    private ArrayList<Integer> normalizedPair(List<Integer> list1, int index1,List<Integer> list2, int index2){
		ArrayList<Integer> pair = new  ArrayList<Integer>();
		if(index1 < 0)
		    pair[0]
		if(index >= list.size())
		    return null;

	    }

	}

	class MergeComponent extends BaseComponent{
	    List<ArrayList<Integer>> inputsList = null;
	    public MergeComponent(List<ArrayList<Integer>> inputsList, int nOutput){
		assert inputsList.size() == 4;
		this.inputsList = inputsList;
		this.outputs = new ArrayList<Integer>(nOutput);
	    }
	    @Override
	    void constitutiveClause() {
		int k = this.outputs.size();
		if(this.inputsList.get(1).size() == 0){
		    this.outputs = this.inputsList.get(0);
		    return;
		}
		if(this.inputsList.get(0).size() == 1){
		    SelectionComponent selcomp = new SelectionComponent(concatenate(inputsList), k);
		    selcomp.constitutiveClause();
		    return;
		}
		List<ArrayList<Integer>> inputsListOdd = new ArrayList<ArrayList<Integer>>(4);		    
		List<ArrayList<Integer>> inputsListEven = new ArrayList<ArrayList<Integer>>(4);		    
		int parity = 1;
		int sizeOdd = 0, sizeEven = 0;
		for(int i = 0, n = this.inputsList.size(); i < n; i++){
		    for(int entry: this.inputsList.get(i))
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
		MergeComponent mergeOdd = new MergeComponent(inputsListOdd, kOdd);
		mergeOdd.constitutiveClause();
		
		MergeComponent mergeEven = new MergeComponent(inputsListEven, kEven);
		mergeEven.constitutiveClause();

		CombineComponent combComp = new CombineComponent(preffix(mergeOdd.outputs,kOdd),
								 preffix(mergeEven.outputs,kEven), k);

		this.outputs.addAll(combComp.outputs);
		this.outputs.addAll(suffix(mergeOdd.outputs, kOdd + 1));
		this.outputs.addAll(suffix(mergeEven.outputs, kEven + 1));

		
	    }
	}


	public Circuit(int iObj){
	    this.iObj = iObj;
	    this.setBase();
	}



	/**
	 *Method that ooses the base to be used
	 */
	private void setBase(){
	    this.base = new int[]{2};
	    return;
	}


	private int[] expandWeight(int weight){
	    int[] result = new int[this.base.length];
	    int i = 0;	
	    for(int b: this.base){
		result[i] = weight % b;
		weight = (weight - result[i]*b)/b;
	    }
	    return result;
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

    private void buildCircuit(int iObj){}

    public boolean UpdateCurrentK(int iObj, int upperKD){return true;}
    public boolean isY(int id){return true;};
    public int getCurrentKD(int iObj){return 0;};
    public int getIObjFromY(int id){return 0;};
    public int getKDFromY(int id){return 0;};
    public int getY(int iObj, int iKD){return 0;};
    public String prettyFormatVariable(int literal)   {return "";} ;


}
