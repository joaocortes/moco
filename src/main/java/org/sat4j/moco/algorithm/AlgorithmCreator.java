package org.sat4j.moco.algorithm;

import org.sat4j.moco.Params;
import org.sat4j.moco.goal_delimeter.GoalDelimeter;
import org.sat4j.moco.goal_delimeter.GoalDelimeterCreator;
import org.sat4j.moco.goal_delimeter.GoalDelimeterI;
import org.sat4j.moco.pb.PBSolver;
import org.sat4j.moco.problem.Instance;
import org.sat4j.moco.util.Log;


public class AlgorithmCreator {

    public algorithm create(Params params, Instance moco){
	algorithm algorithm1 = null;
	boolean MSU3 = false;
	boolean withGoalDelimeter;
	switch(params.getAlgorithmI()) {
	case 0:
	    algorithm1 = new ParetoMCS(moco);
	case 1:
	    UnsatSat unsatSat = new UnsatSat(moco);
	    unsatSat.setGoalDelimeter(createGoalDelimeter(params, unsatSat, unsatSat.GetGoalDelimeter(), false));
	case 2:
	    algorithm1 = new pMinimal(moco);
	case 3:
	    UnsatSatMSU3 unsatSatMSU3 = new UnsatSatMSU3(moco);
	    unsatSatMSU3.setGoalDelimeter(createGoalDelimeter(params, unsatSatMSU3, unsatSatMSU3.GetGoalDelimeter(), true));	    
	    break;
	default:
	    Log.comment("Don't know what solver to use");
	    break;
	}
	return algorithm1;
    }
    
    private  GoalDelimeterI createGoalDelimeter(Params params, algorithm algorithm1, GoalDelimeterI gd, boolean MSU3){
	PBSolver solver = algorithm1.getSolver();
	Instance instance = algorithm1.getProblem();
	gd = GoalDelimeterCreator.create(params.getEncodingGD(), instance , solver, MSU3);
	return gd;
    }

}



