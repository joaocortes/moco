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
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.ArrayList;
import java.util.Iterator;

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

public class DigitalEnv {

    private int[] ratios;
    private int basesN;

    DigitalEnv(){this.ratios = new int[]{2};}
    DigitalEnv(int[] ratios){
	this.setRatios(ratios);
    }

    public DigitalNumber expandValue(int value){

	Map<Integer, Integer> digits = new HashMap<Integer, Integer>();
	int i = 0;
	while(value != 0){
	    int ratio = getRatio(i++);
	    int digit = (value % ratio);
	    digits.push(digit);
	    value-=digit;
	    value/=ratio;
	}
	if(digits.size()==0)
	    digits.push(0);
	return Digits(digits);
    }


	private void setRatios(int[] ratios){this.ratios = ratios;}

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
	    for(int j = 0; j < index; j ++ )
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
		candidate *= this.getRatio(i++);
	    if(candidate == base)
		return i+1;
	    else
		return -1;
	}

	public ArrayList<Integer> getCarryBits(ControlledSelectionComponent select1, int ratio) {
	    ArrayList<Integer> carryBits = new ArrayList<Integer>();
	    for(int i = 0, n = select1.outputs.length; i<n; i++)
		if((i + 1) % ratio == 0)
		    carryBits.add(select1.outputs[i]);
	    return carryBits;
	}


    class Digits implements Iterable<int[]> {

	int[] digits;

	public Digits(int[] digits){
	    this.digits = digits;
	}
	public Iterator<int[]> iterator(){
	    return Arrays.asList(this.digits).iterator();
	};


}
    

    
}
