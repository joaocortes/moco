
import subprocess
import os

javaJarName = ("../target/org.sat4j.moco.threeAlgorithms-"
               "0.0.1-SNAPSHOT-jar-with-dependencies.jar")
testsPath = "./convertedInstances/"
outputPath = "./output/"
watcherFilePrefix = "watcher_"
solverOutputFilePrefix = "solver_"
runSolverPath = "./runsolver"

def absolutize(relativePath):
    return os.path.abspath(relativePath)


javaJarName = absolutize(javaJarName)
testsPath = absolutize(testsPath)
outputPath = absolutize(outputPath)
runSolverPath = absolutize(runSolverPath)


def test(time: int):
    for fileName in os.listdir(testsPath):
        print("fileName:" + fileName)
        for solverI in range(3):
            # print(os.path.join(testsPath, fileName))
            runSolver(fileName, solverI, time)


def runSolver(fileName: str, solverI: int, time):
    outputName = os.path.basename(fileName)
    outputName = os.path.splitext(outputName)[0]
    outputName += "_S"+str(solverI)+".out"

    command = (runSolverPath + " "
               "-W " + str(time) + " "
               "-M  4096"
               "  --timestamp "
               "-w " + os.path.join(
                   outputPath, watcherFilePrefix + outputName) + " "
               "-o " + os.path.join(
                   outputPath, solverOutputFilePrefix + outputName) + " "
               "java -jar " + javaJarName + " "
               "" + os.path.join(testsPath, fileName) + " "
               #  "-v 2 "
               "-alg " + str(solverI))

    # print(command)
    subprocess.call(command, shell=True)


test(10)
