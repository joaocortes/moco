import subprocess
import os
os.chdir("../")
baseDir = "../../moco/"

javaJarName = "./target/org.sat4j.moco.threeAlgorithms-0.0."\
        "1-SNAPSHOT-jar-with-dependencies.jar"

testsPath = "./finalResults/instances/"


def test():
    for fileName in os.listdir(testsPath):
        for solverI in range(3):
            runSolver(fileName, solverI)


def runSolver(fileName: str, solverI: int):
    subprocess.call([
        "runsolver", " -W", " 20", " --timestamp",
        " -w", "watcher.out",
        "java", "-jar", javaJarName,
        "-alg", str(solverI)])


test()
