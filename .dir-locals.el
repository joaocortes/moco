;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")
;;;

((nil
  (projectile-project-run-cmd . "java -jar ./target/org.sat4j.moco.threeAlgorithms-0.0.1-SNAPSHOT-jar-with-dependencies.jar example2.opb")
			      ;;;mvn exec:java -Dexec.mainClass=org.sat4j.moco.Launcher -Dexec.args=\"example2.opb\"
  (projectile-project-compilation-cmd . "mvn -DskipTests=true package"))
 (java-mode
  (lsp-file-watch-threshold)))

