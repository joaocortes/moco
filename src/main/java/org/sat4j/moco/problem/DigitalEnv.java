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

import java.util.SortedMap;
import java.util.TreeMap;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Map.Entry;


/**
 * Class with the implementation of Digital environment, for
 * representation of numbers in arbitrary multi radix bases
 * @author Joao O'Neill Cortes
 */

public class DigitalEnv {

    private int[] ratios;
    private int basesN;

    DigitalEnv(){this.ratios = new int[]{2};}

    DigitalEnv(int[] ratios){
	this.setRatios(ratios);
    }

    public DigitalNumber toDigital(int value){

	SortedMap<Integer, Integer> digits = new TreeMap<Integer, Integer>();
	int i = 0;
	int base = 1;
	while(value != 0){
	    int ratio = getRatio(i++);
	    int digit = (value % ratio);
	    digits.put(base, digit);
	    value-=digit;
	    value/=ratio;
	    base *= ratio;
	}
	if(digits.size()==0)
	    digits.put(1, 0);
	return new DigitalNumber(digits);
    }

    public int toInt(DigitalNumber number){
	DigitalNumber.IteratorJumps iterator = number.iterator();
	int result = 0;
	while(iterator.hasNext()){
	    result += iterator.next() * iterator.currentBase();
	}
	return result;    
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

	private int getRatio(int i){
	    if(this.ratios.length <= i)
		return this.ratios[this.ratios.length -1];
	    else return this.ratios[i];
	}
    public int getBase(int index){
	int result = 1;
	for(int j = 0; j < index; j ++ )
	    result*= getRatio(j);
	return result;		
    }

	/**
	 *get Base element i.
	 */

	public int getBase(int index){
	    int result = 1;
	    for(int j = 0; j < index; j ++ )
		result*= getRatio(j);
	    return result;		
	}

	/**
	 *get the index of the base. If not a valid base, returns -1.
	 */

	public int getBaseI(int base){
	    int i = 0;
	    int candidate = 1;
	    while(candidate < base)
		candidate *= this.getRatio(i++);
	    if(candidate == base)
		return i+1;
	    else
		return -1;
	}

	public ArrayList<Integer> getCarryBits(int[] outputs, int ratio) {
	    ArrayList<Integer> carryBits = new ArrayList<Integer>();
	    for(int i = 0, n = outputs.length; i<n; i++)
		if((i + 1) % ratio == 0)
		    carryBits.add(outputs[i]);
	    return carryBits;
	}


    class DigitalNumber implements Iterable<Integer>{

	SortedMap<Integer, Integer> digits;

	public DigitalNumber(SortedMap<Integer, Integer> digits){
	    this.digits = digits;
	}
	public Iterator<Integer> iterator(){
	    return new CustomIterator1();	
	}

	public Iterator<Integer> iterator(int type){
	    switch(type){
	    case 1:
		return new CustomIterator1();
		break;
	    case 2:
		return new CustomIterator2();
		break;
	    default:
		return new CustomIterator1();
	    }
	}
	class CustomIterator1 implements Iterator<Integer>{
	    Iterator<Entry<Integer, Integer>>  current = digits.entrySet().iterator();
	    public boolean hasNext(){return current.hasNext();}
	    public Integer next(){return current.next().getValue();}
	    public Integer currentBase(){return current.next().getKey();};
	}

	class CustomIterator2 implements Iterator<Integer>{
	    int iBase = 0;
	    int currentBase = 1;
	    public boolean hasNext(){if(iBase < basesN  - 1) return true; else return false;}
	    public Integer next(){
		iBase++;
		currentBase *= getRatio(iBase);
		Integer result = digits.get(currentBase);
		if(result == null) return 0; else return result;
	    }
	    public int getIBase(){
		return iBase;
	    }
	}
    }
}
