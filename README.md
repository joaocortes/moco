# HOW TO BUILD 

Run

```
$ mvn -DskipTests=true package
```

to generate the ./target/org.sat4j.threeAlgorithms-0.0.1-SNAPSHOT-jar-with-dependencies.jar jar file.

# HOW TO RUN

```
java -jar  ./target/org.sat4j.moco.threeAlgorithms-0.0.1-SNAPSHOT-jar-with-dependencies.jar -alg 2 examples/example1.opb
```

(Never used, but should work)
# HOW TO RUN THE ANALYZER

The org.sat4j.moco.jar includes an analysis tool for evaluating the quality of Pareto front approximations
through indicators such as hypervolume and inverted generational distance.

Run it using the following command:

```
$ java -cp org.sat4j.moco.jar org.sat4j.moco.analysis.Analyzer <instance file> [<label>:<output file>]+
```

`<output file>` is expected to be in the output format produced by the MOCO solver.
If there exist multiple files for different runs of the same algorithm, these should have the same `<label>`.

# HOW TO RUN THE TRANSLATOR

To print the encoding of the logical circuit of the selection delimeter into ``out.mocnf``,


```
java -jar  ./target/org.sat4j.moco.threeAlgorithms-0.0.1-SNAPSHOT-jar-with-dependencies.jar  in.opb -ib 1 -o out.mocnf
```

NOTE: Sequential encoder is not working.
