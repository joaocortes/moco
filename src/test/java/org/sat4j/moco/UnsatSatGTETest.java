package org.sat4j.moco;
import org.sat4j.moco.algorithm.UnsatSat;

public class UnsatSatGTETest extends algorithmTest<UnsatSat> {
    public UnsatSatGTETest(){};
    public UnsatSat instateAlgorithm(){
	return new UnsatSat(this.moco, "GTE");
    }
}
