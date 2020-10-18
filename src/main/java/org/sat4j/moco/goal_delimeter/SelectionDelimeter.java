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
package org.sat4j.moco.goal_delimeter;

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
import org.sat4j.moco.algorithm.algorithm;
import org.sat4j.moco.goal_delimeter.Circuit.ControlledComponent;
import org.sat4j.moco.pb.PBSolver;
import org.sat4j.moco.problem.Instance;
import org.sat4j.moco.problem.Objective;
import org.sat4j.moco.problem.DigitalEnv;
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
	if(buildCircuit)
	    this.buildCircuits();
	// Log.comment(5, "}");
    }

    public void buildCircuits(){
	for(int iObj = 0, nObj = instance.nObjs() ;iObj< nObj; ++iObj){
	    this.objManagers[iObj] = new ObjManager(iObj);
	    Objective ithObjective = this.getInstance().getObj(iObj);
	    int oldActivator;
	    int activator = 0;
	    objManagers[iObj].buildMyself();
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

    public ObjManager getIthObjManager(int i){return this.objManagers[i];}

    static class SDIndex extends Index{

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
		    public int getFreshVar1(){return getSolver().getFreshVar();}
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
	    int activator = getSolver().getFreshVar();
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



	public int getIObj() {
	    return this.iObj;
	}



	public void buildMyself() {
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
		    public int getFreshVar1(){return getSolver().getFreshVar();}
		    public boolean AddClause1(IVecInt setOfLiterals){return AddClause(setOfLiterals);}
		};
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

    public int unaryToIndex(int kD){
	return kD  - 1;

    }
}


