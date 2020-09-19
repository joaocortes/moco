package org.sat4j.moco;
import org.sat4j.moco.algorithm.UnsatSatMSU3;

public class UnsatSatGTETest extends algorithmTest<UnsatSatMSU3> {
    public UnsatSatGTETest(){};
    public UnsatSatMSU3 instateAlgorithm(){
	return new UnsatSatMSU3(this.moco, "GTE", false);
    }
}
