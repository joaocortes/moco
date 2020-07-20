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
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;

import java.util.PriorityQueue;
import java.util.ArrayList;
import java.util.TreeMap;
import java.util.Collection;


import org.sat4j.core.ReadOnlyVec;
import org.sat4j.core.VecInt;
import org.sat4j.moco.util.Real;
import org.sat4j.moco.pb.PBSolver;
import org.sat4j.moco.problem.GenTotalEncoder.SumTree.Node;
import org.sat4j.specs.IVecInt;


/**
 * Class with the implementation of the generalized totalizor encoder.
 * Notice that the differential is both a value and an index starting at 0
 * @author Joao O'Neill Cortes
 */

public class SelectionDelimeter extends GoalDelimeter {

    private Circuit[] circuits;
    

class Circuit{
    int iObj;
    int[] base = null;


    abstract class BaseComponent {

	private int[] inputs;
	private int[] outputs;


	public BaseComponent(){
	    this.inputs = null;
	    this.outputs = null;
	}

	public BaseComponent(int[] inputs, int nOutput){
	    this.inputs = inputs;
	    this.outputs = new int[nOutput];
	}

	abstract void constitutiveClause();

    }

    class selectionComponent extends BaseComponent{
	@Override
	void constitutiveClause() {

	}
    }


    class CombineComponent extends BaseComponent{
	@Override
	void constitutiveClause() {
	    // TODO Auto-generated method stub
	    
	}

    }

    class MergeComponent extends BaseComponent{
	@Override
	void constitutiveClause() {
	    // TODO Auto-generated method stub
	    
	}
    }


    public Circuit(int iObj){
	this.iObj = iObj;
	this.setBase();
    }

    public int[] selectionComponent(int[] intput)


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

    public SelectionDelimeter(Instance instance, PBSolver solver) {
	// Log.comment(5, "{ GenTotalEncoder");
	this.instance = instance;	
	this.solver = solver;
	this.firstVariable = this.solver.nVars() + 1;
	this.circuits = new Circuit[this.instance.nObjs()];
	for(int iObj = 0, nObj = instance.nObjs() ;iObj< nObj; ++iObj){
	    this.circuits[iObj] = new Circuit(iObj);
	}

	// Log.comment(5, "}");
    }
    
    private void buildCircuit(int iObj){}

     public boolean UpdateCurrentK(int iObj, int upperKD){return true;}
     public boolean isY(int id){return true;};
     public int getCurrentKD(int iObj){return 0;};
     public int getIObjFromY(int id){return 0;};
     public int getKDFromY(int id){return 0;};
     public int getY(int iObj, int iKD){return 0;};
     public String prettyFormatVariable(int literal)   {return "";} ;


}
