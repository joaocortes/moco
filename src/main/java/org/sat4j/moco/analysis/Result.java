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
 *   Miguel Terra-Neves, Ines Lynce and Vasco Manquinho - MOCO solver
 *******************************************************************************/
package org.sat4j.moco.analysis;

import org.moeaframework.core.NondominatedPopulation;
import org.moeaframework.core.Population;
import org.moeaframework.core.Solution;
import org.moeaframework.core.Variable;
import org.moeaframework.core.variable.EncodingUtils;
import org.sat4j.moco.pb.PBSolver;
import org.sat4j.moco.problem.Instance;
import org.sat4j.moco.util.Clock;
import org.sat4j.moco.util.Log;

/**
 * Container class for the result of an execution of the MOCO solver.
 * Stores the nondominated solutions found by the solver and if that set of solutions is the Pareto front
 * of the MOCO instance.
 * @author Miguel Terra-Neves
 */
public class Result extends GenericResult {



    /**
     * Stores the nondominated solutions found by the solver.
     */
    protected NondominatedPopulation solutions = null;

    public Result(){}
    /**
     * Creates an instance of a container for the nondominated solutions found for a given MOCO instance.
     * @param m The instance.
     */
    public Result(Instance m) {
        this.problem = new MOCOProblem(m);
        this.solutions = new NondominatedPopulation();
    }

    /**
     * Retrieves the nondominated solutions in the container.
     * @return The nondominated solutions.
     */
    NondominatedPopulation getSolutions() { return this.solutions; }
}
