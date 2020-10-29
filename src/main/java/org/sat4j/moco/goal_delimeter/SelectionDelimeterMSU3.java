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


import org.sat4j.core.ReadOnlyVec;
import org.sat4j.core.ReadOnlyVecInt;
import org.sat4j.core.VecInt;
import org.sat4j.moco.util.Log;
import org.sat4j.moco.util.Real;
import org.sat4j.moco.pb.PBSolver;
import org.sat4j.moco.problem.Instance;
import org.sat4j.moco.problem.Objective;
import org.sat4j.moco.problem.DigitalEnv;
import org.sat4j.moco.problem.DigitalEnv.DigitalNumber;
import org.sat4j.specs.IVecInt;

/**
 * Class with the implementation of the Selection network based encoder.
 * @author Joao O'Neill Cortes
 */

public class SelectionDelimeterMSU3 extends SelectionDelimeterT<SelectionDelimeterMSU3.ObjManager>{

    private int[] uncoveredMaxKD = null;
    private HashMap<Integer, Boolean> coveredLiterals = null;

    /**
     * Upper bound, exclusive
     */
    private int[] UpperBound = null;


    public SelectionDelimeterMSU3(Instance instance, PBSolver solver, boolean buildCircuit) {
	super(instance, solver, buildCircuit);
	this.uncoveredMaxKD = new int[this.instance.nObjs()];
	this.UpperBound =  new int[(this.instance.nObjs())];
	this.coveredLiterals = new HashMap<Integer, Boolean>(this.solver.nVars());
	this.initializeCoveredLiterals();
    }


    private void initializeCoveredLiterals(){
	for(int iObj = 0, nObj = this.instance.nObjs();iObj < nObj; iObj++){
	    Objective ithObjective = this.instance.getObj(iObj);
	    ReadOnlyVecInt objectiveLits = ithObjective.getSubObjLits(0);
	    ReadOnlyVec<Real> objectiveCoeffs = ithObjective.getSubObjCoeffs(0);
	    int sign = 1;
	    int ithAbsoluteWeight;
	    for(int iX = 0, nX = ithObjective.getTotalLits(); iX <nX; iX ++){
		int ithX = objectiveLits.get(iX);
		ithAbsoluteWeight = objectiveCoeffs.get(iX).asInt();
		sign = (ithAbsoluteWeight > 0? 1 : -1);
		ithAbsoluteWeight *= sign;
		this.coveredLiterals.putIfAbsent(-sign * ithX, true);
	    }
	}

    }
    // static class SDIndex extends Index{

    // 	SDIndex(int iObj, int kD){
    // 	    super(iObj, kD);
    // 	}
    // }
    ;

    public class ObjManager implements IObjManager{
	int iObj;
	Circuit circuit;
	DigitalEnv digitalEnv;

	ObjManager(int iObj){
	    this.iObj = iObj;
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


	public DigitalEnv getDigitalEnv(){return this.digitalEnv;}
	public Circuit getCircuit(){return this.circuit;}
	public int digitalLiteral(int base, int value){
	    Circuit.ControlledComponent component = circuit.getControlledComponentBase(base);
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
	    DigitalNumber.IteratorContiguous iterator = digits.iterator3();
	    int activator = getSolver().getFreshVar();
	    IVecInt clause = new VecInt(new int[]{activator});
	    SDIndex sDIndex = new SDIndex(iObj, upperLimit);
	    librarian.putIndex(activator, sDIndex);
	    setY(iObj, upperLimit, activator);
	    Log.comment(6, "Lexicographic order");
	    this.LexicographicOrderRecurse(iterator, clause);
	    AddClause(clause);
	    return activator;
	}

	private void LexicographicOrderRecurse(DigitalNumber.IteratorContiguous iterator, IVecInt clause){
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


	@Override
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
	    this.circuit.buildCircuit();
	}


	@Override
	public int getIObj() {
	    return this.iObj;
	}
	

    }

	public int getUncoveredMaxKD(int iObj) {
	    return this.uncoveredMaxKD[iObj];
	}


	public void setMaxUncoveredKD(int iObj, int a) {
	    this.uncoveredMaxKD[iObj] = a;
	}



	public int getMaxUncoveredKD(int iObj) {
	    return this.uncoveredMaxKD[iObj];
	}


	@Override
	protected ObjManager[] objManagersCreator() {
	    return new ObjManager[this.getInstance().nObjs()];
	}

	@Override
	protected ObjManager objManagerCreator(int iObj) {
	    return new ObjManager(iObj);
	}


	public HashMap<Integer, Boolean> getCoveredLiterals() {
		return coveredLiterals;
	}


	public void setCoveredLiterals(HashMap<Integer, Boolean> coveredLiterals) {
		this.coveredLiterals = coveredLiterals;
	}

    /**
     *If necessary for the construction of the current assumptions,
     *initialize more of the domain of the goal delimeter
     */
    @Override
    public boolean preAssumptionsExtend(IVecInt currentExplanation){
	Log.comment(2, "explanation: ");
	this.prettyPrintVecInt(currentExplanation);
	boolean change = false;
	// Log.comment(0, "covered x variables: " + this.coveredLiterals.size());
	IVecInt currentExplanationX = new VecInt(new int[] {});
	HashMap<Integer,Boolean> objectivesToChange = new HashMap<Integer, Boolean>(this.instance.nObjs());
	for(int lit: currentExplanation.toArray()){
	    int id = this.solver.idFromLiteral(lit);
	    if(this.isX(id)){
		currentExplanationX.push(lit);
		for(int iObj = 0; iObj < this.instance.nObjs(); ++iObj){
		    if(this.instance.getObj(iObj).getSubObj(0).weightFromLit(id) != null)
			objectivesToChange.put(iObj, null);
		}
	    }
	    else
		objectivesToChange.put(this.getIObjFromY(id), null);

	}
	change = this.uncoverXs(currentExplanationX);
	for(int iObj :objectivesToChange.keySet()){
	    // Log.comment(3, "changing upperlimit " + iObj);
	    int upperKDBefore = this.getUpperKD(iObj);
 	    if(this.getUpperKD(iObj) == this.getUpperBound(iObj))
		this.generateNext(iObj,this.getUpperKD(iObj), this.getMaxUncoveredKD(iObj));
	    this.setUpperKD(iObj, this.nextKDValue(iObj, this.getUpperKD(iObj)));
	    if(this.getUpperKD(iObj) >= this.getUpperBound(iObj))
		this.generateNext(iObj, this.getUpperKD(iObj), this.getMaxUncoveredKD(iObj));
	    this.setUpperBound(iObj, this.nextKDValue(iObj, this.getUpperKD(iObj)));
	    if(this.getUpperKD(iObj)!= upperKDBefore)
		change = true;
	}
	return change;

    }
	@Override
	public IVecInt generateUpperBoundAssumptions(IVecInt explanation, boolean checkChange) {
	    IVecInt assumptions =  super.generateUpperBoundAssumptions(explanation, checkChange);
	    for(Integer x: this.coveredLiterals.keySet())
		assumptions.push(x);
	    
	    Log.comment(2, "assumptions:");
	    this.prettyPrintVecInt(assumptions);
	    return assumptions;
	}
    /**
     *Uncover leafs
     */
    private boolean uncoverXs(IVecInt explanationX) {
	// Log.comment(5, "{ UnsatSatMSU3.uncoverXs");

	int lit = 0;
	for(int iLit = 0, n = explanationX.size(); iLit < n; iLit++){
	    lit = explanationX.get(iLit);
	    this.coveredLiterals.remove(lit);
	}
	this.updateAllUncoveredMaxKD();
	this.logUncoveredMaxKD();
	// Log.comment(5, "}");
	return true;
    }

private void updateAllUncoveredMaxKD(){
    for(int i = 0, n = this.instance.nObjs(); i < n; i++)
	this.updateUncoveredMaxKD(i);
}
    public void logUncoveredMaxKD(){
	String logUpperLimit = "uncovered max: ["+this.getUncoveredMaxKD(0);
	for(int iObj = 1; iObj < this.instance.nObjs(); ++iObj)
	    logUpperLimit +=", "+this.getUncoveredMaxKD(iObj) ;//+ this.instance.getObj(iObj).getMinValue())
	//..log
	
	logUpperLimit +="]";
	Log.comment(2, logUpperLimit );
    }

	public void updateUncoveredMaxKD(int iObj){
	    int a = 0;
	    Objective ithObjective = instance.getObj(iObj); // 
	    ReadOnlyVecInt objectiveLits = ithObjective.getSubObjLits(0);
	    ReadOnlyVec<Real> objectiveCoeffs = ithObjective.getSubObjCoeffs(0);
	    int sign = 1;
	    int ithAbsoluteWeight;
	    for(int iX = 0, nX = ithObjective.getTotalLits(); iX < nX; iX ++){
		int ithX = objectiveLits.get(iX);
		ithAbsoluteWeight = objectiveCoeffs.get(iX).asInt();
		sign = (ithAbsoluteWeight > 0? 1 : -1);
		ithAbsoluteWeight *= sign;
		if(coveredLiterals.get(-sign * ithX) == null)
		    a += ithAbsoluteWeight;
	    }
	    this.setMaxUncoveredKD(iObj, a);
	}
    public void setMaxUncoveredKDs(int iObj, int a){this.uncoveredMaxKD[iObj] = a;}

    private int getUpperBound(int iObj){
	return this.UpperBound[iObj];
    }

    /**
     *Sets the current upper bound of iObj to nowKD
     *@param newKD
     *@param iObj
     */
    private void setUpperBound(int iObj, int newKD){
	    this.UpperBound[iObj] = newKD;
    }

    
    @Override
    public int generateNext(int iObj, int kD, int inclusiveMax) {
	Integer next = super.nextKDValue(iObj, kD);
	if( next > inclusiveMax)
	    return kD;
	this.generateOneY(kD, iObj);
	next = super.nextKDValue(iObj, kD);
	if( next > inclusiveMax)
	    return kD;
	return next;
    }

    public int nextKDValue(int iObj, int kD) {
	if(kD < this.getUncoveredMaxKD(iObj))
	    return super.nextKDValue(iObj, kD);
	if(kD == this.getUncoveredMaxKD(iObj))
	    return kD;
	else return 0;
    }

    public void generateOneY(int kD, int iObj){
	int oldY;
	int oldKD = kD;
	int newKD = this.nextKDValue(iObj, oldKD);
	int newY = this.getY(iObj, newKD);
	if(newY != 0)
	    return;
	newY = this.getIthObjManager(iObj).LexicographicOrder(newKD);
	oldY = this.getY(iObj, oldKD);

	if(newKD >  this.nextKDValue(iObj, 0) && newKD > oldKD){
	    this.AddClause(new VecInt(new int[]{-newY, oldY}));
	    
	    //TODO: repair the ordering of the y variables
	    int oldNextKD = this.nextKDValue(iObj, newKD);
	    if(oldNextKD > newKD){
		int oldNextY = this.getY(iObj, oldNextKD);
		if(oldNextY == 0)
		    return;
		else
		    this.AddClause(new VecInt(new int[]{-oldNextY, newY}));
	    }	
	}

    }
}


