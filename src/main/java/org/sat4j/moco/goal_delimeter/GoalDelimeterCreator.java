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

import org.sat4j.moco.pb.PBSolver;
import org.sat4j.moco.problem.Instance;
import org.sat4j.moco.util.Log;


public class GoalDelimeterCreator {
    
    static public GoalDelimeter<?> create(String encoding, Instance instance, PBSolver solver, boolean MSU3){
	GoalDelimeter<?> gd = null;
	switch(encoding) {
	case "SD":
	    if (MSU3) 
		gd = new SelectionDelimeterMSU3(instance, solver, true);
	    else 
		gd = new SelectionDelimeter(instance, solver, true);
	    break;
	case "GTE":
	    if (MSU3) 
		gd = new GenTotalEncoderMSU3(instance, solver);
	    else 
		gd = new GenTotalEncoder(instance, solver);
	    break;
	    
	default:
	    Log.comment("Don't know what encoding to use");
	    break;
	}
	return gd;
    }




}
