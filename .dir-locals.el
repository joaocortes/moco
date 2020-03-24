;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  ;;;(projectile-project-run-cmd . "java -jar  ./target/org.sat4j.moco.threeAlgorithms-0.0.1-SNAPSHOT-jar-with-dependencies.jar examples/example1.opb")
  (lsp-file-watch-threshold)
  (projectile-project-run-cmd . "mvn exec:java -Dexec.mainClass=org.sat4j.moco.Launcher -Dexec.args=\"examples/example1.opb -alg 1\"")
  (projectile-project-compilation-cmd . "mvn -DskipTests=true package"))
 (java-mode
  (dap-debug-template-configurations .  (("Run unsatSat on example1" :type "java" :request "launch" :args "examples/example1.opb -alg 1 -v 6" :cwd nil :stopOnEntry :json-false :host "localhost" :request "launch"
 :name "Run Configuration" ;; :projectName  "org.sat4j.moco"
 :mainClass  "org.sat4j.moco.Launcher")))
))
