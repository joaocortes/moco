package org.sat4j.moco;
import org.sat4j.moco.algorithm.UnsatSat;

public class UnsatSatSDTest extends algorithmTest<UnsatSat> {
    public UnsatSatSDTest(){};
    public UnsatSat instateAlgorithm(){
	return new UnsatSat(this.moco, "SD");

    }
}
