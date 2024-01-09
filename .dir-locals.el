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
				       :mainClass  " org.sat4j.moco.analysis.Analyzer")
				      ("Run analyzer on PU result" :type "java" :request "launch"
				       :args "/home/Superficial/Mnemosyne/Aion/moco/mocoSource/solver/bugs/bugAnalyzer_singleton/rand149.pbmo  _S0:/home/Superficial/Mnemosyne/Aion/moco/mocoSource/solver/bugs/bugAnalyzer_singleton/solver_rand149_S0.txt_clean _S8:/home/Superficial/Mnemosyne/Aion/moco/mocoSource/solver/bugs/bugAnalyzer_singleton/solver_rand149_S8.txt_clean _S2:/home/Superficial/Mnemosyne/Aion/moco/mocoSource/solver/bugs/bugAnalyzer_singleton/solver_rand149_S2.txt_clean" :cwd nil :stopOnEntry :json-false :host "localhost" :request "launch"
				       :name "analyze1" 
				       :mainClass  " org.sat4j.moco.analysis.Analyzer")
				      ("Run analyzer on DAL result"
				       :type "java" :request "launch" :args 
				       "/home/Superficial/Mnemosyne/Aion/moco/mocoData/runnerData/instances/DAL/sampleA/f1-DataDisplay_0_order4.seq-A-2-2-EDCBAir.pbmo  1.0,GTE,run12:/home/Superficial/Mnemosyne/Aion/moco/mocoData/runnerData/finalResultsDAL/run12/output/solver_f1-DataDisplay_0_order4.seq-A-2-2-EDCBAir_S1.txt_cleanedzlv6um4q 1.0,SD,run44:/home/Superficial/Mnemosyne/Aion/moco/mocoData/runnerData/totalRuns/run44/output/solver_f1-DataDisplay_0_order4.seq-A-2-2-EDCBAir_S1.txt_cleaned7gyj89a2 2.0,SDtruncJ,run51:/home/Superficial/Mnemosyne/Aion/moco/mocoData/runnerData/totalRuns/run51/output/solver_f1-DataDisplay_0_order4.seq-A-2-2-EDCBAir_S2.txt_cleaneduks4r8s2 2.0,SWC,run13:/home/Superficial/Mnemosyne/Aion/moco/mocoData/runnerData/finalResultsDAL/run13/output/solver_f1-DataDisplay_0_order4.seq-A-2-2-EDCBAir_S2.txt_cleanedcf8m7bgz 3.0,GTE,run30:/home/Superficial/Mnemosyne/Aion/moco/mocoData/runnerData/totalRuns/run30/output/solver_f1-DataDisplay_0_order4.seq-A-2-2-EDCBAir_S3.txt_cleanedytc59yqd 3.0,GTE++,run39:/home/Superficial/Mnemosyne/Aion/moco/mocoData/runnerData/totalRuns/run39/output/solver_f1-DataDisplay_0_order4.seq-A-2-2-EDCBAir_S3.txt_cleanedb0w13pp9 3.0,GTE+++,run43:/home/Superficial/Mnemosyne/Aion/moco/mocoData/runnerData/totalRuns/run43/output/solver_f1-DataDisplay_0_order4.seq-A-2-2-EDCBAir_S3.txt_cleaned2g_pnck1 3.0,GTEGTE,run35:/home/Superficial/Mnemosyne/Aion/moco/mocoData/runnerData/totalRuns/run35/output/solver_f1-DataDisplay_0_order4.seq-A-2-2-EDCBAir_S3.txt_cleanedw9i2h662 3.0,SD,run48:/home/Superficial/Mnemosyne/Aion/moco/mocoData/runnerData/totalRuns/run48/output/solver_f1-DataDisplay_0_order4.seq-A-2-2-EDCBAir_S3.txt_cleanedmeybhf4m 3.0,SDtrunc,run50:/home/Superficial/Mnemosyne/Aion/moco/mocoData/runnerData/totalRuns/run50/output/solver_f1-DataDisplay_0_order4.seq-A-2-2-EDCBAir_S3.txt_cleaned6wiaiwed 4.0,SDTruncOU,run60:/home/Superficial/Mnemosyne/Aion/moco/mocoData/runnerData/totalRuns/run60/output/DAL/sampleA/solver_f1-DataDisplay_0_order4.seq-A-2-2-EDCBAir_S4.txt_cleaned0mtufvpj 4.0,SDtruncO,run57:/home/Superficial/Mnemosyne/Aion/moco/mocoData/runnerData/totalRuns/run57/output/DAL/sampleA/solver_f1-DataDisplay_0_order4.seq-A-2-2-EDCBAir_S4.txt_cleaned5sj6sik2 4.0,SDtruncULO,run61:/home/Superficial/Mnemosyne/Aion/moco/mocoData/runnerData/totalRuns/run61/output/DAL/sampleA/solver_f1-DataDisplay_0_order4.seq-A-2-2-EDCBAir_S4.txt_cleaned5pb9uqg9 6.0,SDtruncBad,run74:/home/Superficial/Mnemosyne/Aion/moco/mocoData/runnerData/totalRuns/run74/output/DAL/sampleA/solver_f1-DataDisplay_0_order4.seq-A-2-2-EDCBAir_S6.txt_cleanedxjbean6k 6.0,SDtruncSet,run75:/home/Superficial/Mnemosyne/Aion/moco/mocoData/runnerData/totalRuns/run75/output/DAL/sampleA/solver_f1-DataDisplay_0_order4.seq-A-2-2-EDCBAir_S6.txt_cleaned11cpng0d 6.0,SDtruncSingle,run77:/home/Superficial/Mnemosyne/Aion/moco/mocoData/runnerData/totalRuns/run77/output/DAL/sampleA/solver_f1-DataDisplay_0_order4.seq-A-2-2-EDCBAir_S6.txt_cleanedy0niexvb 7.0,SDtruncGood,run76:/home/Superficial/Mnemosyne/Aion/moco/mocoData/runnerData/totalRuns/run76/output/DAL/sampleA/solver_f1-DataDisplay_0_order4.seq-A-2-2-EDCBAir_S7.txt_cleanedwc0qim7x"
				       :cwd nil :stopOnEntry :json-false :host "localhost" :request "launch"
				       :name "analyze0" 
				       :mainClass  " org.sat4j.moco.analysis.Analyzer")
				      ))))
