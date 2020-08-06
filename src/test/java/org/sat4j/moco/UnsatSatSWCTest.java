package  org.sat4j.moco;
import org.sat4j.moco.algorithm.UnsatSat;

public class UnsatSatSWCTest extends algorithmTest<UnsatSat> {
    public UnsatSatSWCTest(){};
    public UnsatSat instateAlgorithm(){
	return new UnsatSat(this.moco, "SWC");

    }
}
