package org.sat4j.moco;
import org.sat4j.moco.algorithm.UnsatSatMSU3;
import org.sat4j.moco.util.Log;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;
import org.sat4j.core.Vec;
import org.sat4j.core.VecInt;
import org.sat4j.moco.algorithm.algorithm;

import org.sat4j.moco.analysis.Result;
import org.sat4j.moco.pb.PBFactory;
import org.sat4j.moco.problem.LinearObj;
import org.sat4j.moco.problem.Instance;
import org.sat4j.moco.problem.Objective;
import org.sat4j.moco.util.Real;

public class UnsatSatMSU3Test extends algorithmTest<UnsatSatMSU3> {
    public UnsatSatMSU3Test(){};

    @Override
    public UnsatSatMSU3 instateAlgorithm(){
	return new UnsatSatMSU3(this.moco, "");

    }

    @Test
    public void testManyObjective() {
	Log.setVerbosity(1);
        LinearObj other_obj1 = new LinearObj(new VecInt(new int[] { -2, 3 }),
                                             new Vec<Real>(new Real[] { new Real(2), new Real(2) }));
        LinearObj other_obj2 = new LinearObj(new VecInt(new int[] { 2, 3 }),
                                             new Vec<Real>(new Real[] { Real.ONE.negate(), Real.ONE.negate() }));
        LinearObj other_obj3 = new LinearObj(new VecInt(new int[] { -1, 3 }),
                                             new Vec<Real>(new Real[] { new Real(2), new Real(2) }));
        LinearObj other_obj4 = new LinearObj(new VecInt(new int[] { 1, 2 }),
                                             new Vec<Real>(new Real[] { Real.ONE.negate(), Real.ONE.negate() }));
        this.moco.addObj(other_obj1);
        this.moco.addObj(other_obj2);
        this.moco.addObj(other_obj3);
        this.moco.addObj(other_obj4);
	this.solver = this.instateAlgorithm();
        this.solver.solve();
        Result result = this.solver.getResult();
        assertTrue(result.isParetoFront());

        for (int i = 0; i < result.nSolutions(); i++) {
	    boolean[] assignment = result.getAssignment(i);
	    double[] costs = result.getCosts(i);
	    System.out.println("assignment:");
	    for(boolean val: assignment)
		System.out.println(val);
	    System.out.println("costs:");
	    for(double cost: costs)
		System.out.println(cost);

        }
        assertTrue(result.nSolutions() == 4); // 
        boolean[][] front_sols = new boolean[][] { new boolean[] { false, true, true },
                                                   new boolean[] { true, false, true },
                                                   new boolean[] { true, true, false },
                                                   new boolean[] { true, true, true } };
        double[][] front_costs = new double[][] { new double[] { 1, 2, -2, 4, -1 },
                                                  new double[] { 2, 4, -1, 2, -1 },
                                                  new double[] { 3, 0, -1, 0, -2 },
                                                  new double[] { 3, 2, -2, 2, -2 } };
        Objective[] objs = new Objective[] { this.main_obj, other_obj1, other_obj2, other_obj3, other_obj4 };
        validateResult(result, objs, front_sols, front_costs);
    }

}
