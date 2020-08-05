package org.sat4j.moco;
import org.sat4j.moco.algorithm.UnsatSatMSU3;

public class UnsatSatMSU3Test extends algorithmTest<UnsatSatMSU3> {
    public UnsatSatMSU3Test(){};

    @Override
    public UnsatSatMSU3 instateAlgorithm(){
	return new UnsatSatMSU3(this.moco, true);

    }


}
