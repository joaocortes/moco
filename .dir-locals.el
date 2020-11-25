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
("Run unsatSat on exampleXX" :type "java" :request "launch" :args "examples/exampleXX.opb -alg 3 -v 6 -enc \"SD\" -ib 1" :cwd nil :stopOnEntry :json-false :host "localhost" :request "launch"
 :name "Run Configuration" ;; :projectName  "org.sat4j.moco"
 :mainClass  "org.sat4j.moco.Launcher")
))))
