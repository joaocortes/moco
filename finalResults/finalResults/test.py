import subprocess
import os

javaJarName = "../target/org.sat4j.moco.threeAlgorithms-0.0.1-SNAPSHOT-jar-with-dependencies.jar"
testsPath = "./convertedInstances/"
outputPath = "./output/"
watcherFilePrefix = "watcher_"
solverOutputFilePrefix = "solver_"


def absolutize(relativePath):
    return os.path.abspath(relativePath)


javaJarName = absolutize(javaJarName)
testsPath = absolutize(testsPath)
outputPath = absolutize(outputPath)



def test():
    for fileName in os.listdir(testsPath):
        print("fileName:" + fileName)
        for solverI in range(3):
            # print(os.path.join(testsPath, fileName))
            runSolver(fileName, solverI)


def runSolver(fileName: str, solverI: int):
    outputName = os.path.basename(fileName)+".out"
    command = "runsolver  -W  20  --timestamp "
    command += "-w " + os.path.join(outputPath, watcherFilePrefix + outputName) + " "
    command += "-o " + os.path.join(outputPath, solverOutputFilePrefix + outputName) + " "
    command += " java -jar " + javaJarName + " "
    command += os.path.join(testsPath, fileName) + " "
    command += "-alg " + str(solverI)

    # print(command)
    subprocess.call(command, shell=True)


    # subprocess.call([
    #     "runsolver", " -W", " 20", " --timestamp",
    #     "-w " + watcherFile,
    #     "-o " + solverOutputFile,
    #     "java", "-jar " + javaJarName,
    #     fileName,
    #     "-alg", str(solverI)
    # ])


test()
