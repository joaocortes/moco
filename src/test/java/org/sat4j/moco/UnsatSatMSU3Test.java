package org.sat4j.moco;
import org.sat4j.moco.algorithm.UnsatSatMSU3;
import org.sat4j.moco.algorithm.algorithm;

public class UnsatSatMSU3Test extends algorithmTest {
    public UnsatSatMSU3Test(){};
    public algorithm instateAlgorithm(){
	return this.algCreator.create(3, "SD", this.moco);
    }



}
