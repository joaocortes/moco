;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  ;;;(projectile-project-run-cmd . "java -jar  ./target/org.sat4j.moco.threeAlgorithms-0.0.1-SNAPSHOT-jar-with-dependencies.jar examples/exampleX.opb")
  (lsp-file-watch-threshold)
  (projectile-project-run-cmd . "export MAVEN_OPTS=\"-ea\";cat examples/exampleX.opb;mvn exec:java -Dexec.mainClass=org.sat4j.moco.Launcher -Dexec.args=\"examples/exampleX.opb -alg 1 -enc \\\"SD\\\"\"")
  ;; (projectile-project-run-cmd . "export MAVEN_OPTS=\"-ea\";cat examples/exampleX.opb;mvn exec:java -Dexec.mainClass=org.sat4j.moco.Launcher -Dexec.args=\"examples/exampleX.opb -alg 1 -enc \\\"SD\\\"  -rb 0:[20,3,3] -ul 0:5\"")
  (projectile-project-compilation-cmd . "mvn -DskipTests=true package"))
 (java-mode
  (dap-debug-template-configurations .
				     (("Run unsatSat on exampleX" :type "java" :request "launch" :args "examples/exampleX.opb -ib 1 -v 6 -ul 0:100,1:100" :cwd nil :stopOnEntry :json-false :host "localhost" :request "launch"
				       :name "example2" ;; :projectName  "org.sat4j.moco"
				       :mainClass  "org.sat4j.moco.Launcher(org.sat4j.moco.threeAlgorithms)")
				      ("bug21: Run analyzer on PU result"
				       :type "java"
				       :request "launch"
				       :args "bugs/bug21/1aabfc32-d491-11df-9a24-00163e3d3b7c.pbmo 0.0,*,run89:bugs/bug21/solver_1aabfc32-d491-11df-9a24-00163e3d3b7c_S0.txt_cleanedk21v70e1 2.0,GTE,run91:bugs/bug21/solver_1aabfc32-d491-11df-9a24-00163e3d3b7c_S2.txt_cleanedv626gdar 8.0,GTE,run90:bugs/bug21/solver_1aabfc32-d491-11df-9a24-00163e3d3b7c_S8.txt_cleaned_cjlasxz 16.0,GTEQueue,run122:bugs/bug21/solver_1aabfc32-d491-11df-9a24-00163e3d3b7c_S16.txt_cleanedmkqy1s2e"
				       :cwd nil
				       :stopOnEntry :json-false
				       :host "localhost" 
				       :name "bug21" 
				       :mainClass  "org.sat4j.moco.analysis.Analyzer")
				      ("bug22: Run analyzer on DAL result"
				       :type "java" 
				       :request "launch"
				       :stopOnEntry
				       :json-false
				       :host "localhost"
				       :request "launch"
				       :args " -v 4 \ /home/Superficial/Mnemosyne/Aion/moco\
/mocoData/runnerData/instances/DAL/sampleA\
/f1-DataDisplay_0_order4.seq-A-2-1-EDCBAir.pbmo \
0,*:./bugs/bug22/bugVolume0/dal0 1,GTE:./bugs/bug22/bugVolume0/dal1 1,SD:./bugs/bug22/bugVolume0/dal2 2,SDtruncJ:./bugs/bug22/bugVolume0/dal3 2,SWC:./bugs/bug22/bugVolume0/dal4 3,GTE:./bugs/bug22/bugVolume0/dal5 \
3,GTE++:./bugs/bug22/bugVolume0/dal6 3,GTE+++:./bugs/bug22/bugVolume0/dal7 3,GTEGTE:./bugs/bug22/bugVolume0/dal8 3,SD:./bugs/bug22/bugVolume0/dal9 3,SDtrunc:./bugs/bug22/bugVolume0/dal10 \
4,SDTruncOU:./bugs/bug22/bugVolume0/dal11 4,SDtruncO:./bugs/bug22/bugVolume0/dal12 4,SDtruncULO:./bugs/bug22/bugVolume0/dal13"
				       :name "analyze_dal" 
				       :mainClass  " org.sat4j.moco.analysis.Analyzer")
				      ("Run analyzer on PU result" :type "java" :request "launch"
				       :args "/home/Superficial/Mnemosyne/Aion/moco/mocoSource/solver/bugs/bugAnalyzer_singleton/rand149.pbmo  _S0:/home/Superficial/Mnemosyne/Aion/moco/mocoSource/solver/bugs/bugAnalyzer_singleton/solver_rand149_S0.txt_clean _S8:/home/Superficial/Mnemosyne/Aion/moco/mocoSource/solver/bugs/bugAnalyzer_singleton/solver_rand149_S8.txt_clean _S2:/home/Superficial/Mnemosyne/Aion/moco/mocoSource/solver/bugs/bugAnalyzer_singleton/solver_rand149_S2.txt_clean" :cwd nil :stopOnEntry :json-false :host "localhost" :request "launch"
				       :name "analyze_pu" 
				       :mainClass  "org.sat4j.moco.analysis.Analyzer")
				      ))))
