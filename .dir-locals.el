;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  (projectile-project-run-cmd . "java -jar  ./target/org.sat4j.moco.threeAlgorithms-0.0.1-SNAPSHOT-jar-with-dependencies.jar examples/example2.opb")
  (projectile-project-compilation-cmd . "mvn -DskipTests=true package"))
 (java-mode
  (dap-debug-template-configurations .  (("Run unsatSat on example 1" :type "java" :request "launch" :args "examples/example1.opb -alg 1" :cwd nil :stopOnEntry :json-false :host "localhost" :request "launch"
  :classPaths nil :name "Run Configuration" :projectName nil :mainClass nil)))
  (lsp-file-watch-threshold)))

