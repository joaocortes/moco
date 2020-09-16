;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  ;;;(projectile-project-run-cmd . "java -jar  ./target/org.sat4j.moco.threeAlgorithms-0.0.1-SNAPSHOT-jar-with-dependencies.jar examples/exampleX.opb")
  (lsp-file-watch-threshold)
  (projectile-project-run-cmd . "export MAVEN_OPTS=\"-ea\";cat examples/exampleX.opb;mvn exec:java -Dexec.mainClass=org.sat4j.moco.Launcher -Dexec.args=\"examples/exampleX.opb -alg 1 -enc \\\"SD\\\"\"")
  (projectile-project-compilation-cmd . "mvn -DskipTests=true package"))
 (java-mode
  (dap-debug-template-configurations .  (
("Run unsatSat on exampleX" :type "java" :request "launch" :args "examples/exampleX.opb -alg 1 -v 6 -enc \"GTE\"" :cwd nil :stopOnEntry :json-false :host "localhost" :request "launch"
 :name "Run Configuration" ;; :projectName  "org.sat4j.moco"
 :mainClass  "org.sat4j.moco.Launcher")
("Run unsatSat on bugNonMonotonous" :type "java" :request "launch" :args "bugs/bugNonMonotonous/minimal.pbmo -alg 1 -v 6 -enc \"SD\"" :cwd nil :stopOnEntry :json-false :host "localhost" :request "launch"
 :name "Run Configuration" ;; :projectName  "org.sat4j.moco"
 :mainClass  "org.sat4j.moco.Launcher")
("Run unsatSat on 10192" :type "java" :request "launch" :args "finalResults/finalResultsSC/run1/instances/bp-100-20-3-10-10192-SC.pbmo -alg 1 -v 6" :cwd nil :stopOnEntry :json-false :host "localhost" :request "launch"
 :name "Run Configuration" ;; :projectName  "org.sat4j.moco"
 :mainClass  "org.sat4j.moco.Launcher")
))))
