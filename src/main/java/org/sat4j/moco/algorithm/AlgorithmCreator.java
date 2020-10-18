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
	String encoding = params.getEncodingGD();
	int algorithmI =  params.getAlgorithmI();
	return create(algorithmI, encoding, moco);
    }
    
     public algorithm create(int AlgorithmI, String encoding,  Instance moco){
	algorithm algorithm1 = null;
	switch(AlgorithmI) {
	case 0:
	    algorithm1 = new ParetoMCS(moco);
	    return algorithm1;
	case 1:
	    UnsatSat unsatSat = new UnsatSat(moco);
	    unsatSat.setGoalDelimeter(createGoalDelimeter(encoding, unsatSat, false));
	    return unsatSat;
	case 2:
	    pMinimal pmin = new pMinimal(moco);
	    pmin.setGoalDelimeter(createGoalDelimeter(encoding, pmin, false));
	    return pmin;
	case 3:
	    UnsatSatMSU3 unsatSatMSU3 = new UnsatSatMSU3(moco);
	    unsatSatMSU3.setGoalDelimeter(createGoalDelimeter(encoding, unsatSatMSU3, true));	    
	    return unsatSatMSU3;
	default:
	    Log.comment("Don't know what solver to use");
	    break;
	}
	return algorithm1;
    }



    private  GoalDelimeter<?> createGoalDelimeter(String encoding, algorithm algorithm1, boolean MSU3){
	PBSolver solver = algorithm1.getSolver();
	Instance instance = algorithm1.getProblem();
	return GoalDelimeterCreator.create(encoding, instance , solver, MSU3);
	
    }

}



