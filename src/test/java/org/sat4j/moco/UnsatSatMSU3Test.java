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

}
