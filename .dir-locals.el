;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  ;;;(projectile-project-run-cmd . "java -jar  ./target/org.sat4j.moco.threeAlgorithms-0.0.1-SNAPSHOT-jar-with-dependencies.jar examples/exampleX.opb")
  (lsp-file-watch-threshold)
  (projectile-project-run-cmd . "export MAVEN_OPTS=\"-ea\";cat examples/exampleXX.opb;mvn exec:java -Dexec.mainClass=org.sat4j.moco.Launcher -Dexec.args=\"examples/exampleXX.opb -alg 1 -enc \\\"SD\\\"\"")
  ;; (projectile-project-run-cmd . "export MAVEN_OPTS=\"-ea\";cat examples/exampleXX.opb;mvn exec:java -Dexec.mainClass=org.sat4j.moco.Launcher -Dexec.args=\"examples/exampleXX.opb -alg 1 -enc \\\"SD\\\"  -rb 0:[20,3,3] -ul 0:5\"")
  (projectile-project-compilation-cmd . "mvn -DskipTests=true package"))
 (java-mode
  (dap-debug-template-configurations .  (
("Run unsatSat on exampleXX" :type "java" :request "launch" :args "examples/exampleXX.opb -alg 2 -v 6 -enc \"SD\"" :cwd nil :stopOnEntry :json-false :host "localhost" :request "launch"
 :name "example2" ;; :projectName  "org.sat4j.moco"
 :mainClass  "org.sat4j.moco.Launcher")
("Run analyzer on DAL result" :type "java" :request "launch" :args "-v 4 /home/Superficial/Mnemosyne/Aion/moco/mocoData/runnerData/instances/DAL/sampleA/f1-DataDisplay_0_order4.seq-A-2-1-EDCBAir.pbmo 0,*:./bugs/bugVolume0/dal0 1,GTE:./bugs/bugVolume0/dal1 1,SD:./bugs/bugVolume0/dal2 2,SDtruncJ:./bugs/bugVolume0/dal3 2,SWC:./bugs/bugVolume0/dal4 3,GTE:./bugs/bugVolume0/dal5 3,GTE++:./bugs/bugVolume0/dal6 3,GTE+++:./bugs/bugVolume0/dal7 3,GTEGTE:./bugs/bugVolume0/dal8 3,SD:./bugs/bugVolume0/dal9 3,SDtrunc:./bugs/bugVolume0/dal10 4,SDTruncOU:./bugs/bugVolume0/dal11 4,SDtruncO:./bugs/bugVolume0/dal12 4,SDtruncULO:./bugs/bugVolume0/dal13" :cwd nil :stopOnEntry :json-false :host "localhost" :request "launch"
 :name "analyze0" 
 :mainClass  " org.sat4j.moco.analysis.Analyzer")
))))
