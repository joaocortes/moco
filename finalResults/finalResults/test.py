import argparse
import sys
import subprocess
import os


class Tester():

    def __init__(self, time, memoryKB: int):
        self.time = time
        self.memoryKB = memoryKB
        self.javaJarName = ("../target/org.sat4j.moco.threeAlgorithms-"
                            "0.0.1-SNAPSHOT-jar-with-dependencies.jar")
        self.testsPath = "./convertedInstances/"
        self.outputPath = "./output/"
        self.watcherFilePrefix = "watcher_"
        self.solverOutputFilePrefix = "solver_"
        self.runSolverPath = "./runsolver"
        self.javaJarName = self.absolutize(self.javaJarName)
        self.testsPath = self.absolutize(self.testsPath)
        self.runSolverPath = self.absolutize(self.runSolverPath)

    def readArguments(self):

        parser = argparse.ArgumentParser(
            "testing script to compare the 3 algorithms for finding the pareto"
            "Front."
            )
        parser.add_argument("-s", type=str, dest="sandbox", default="1",
                            help="sandbox results?")
        parser.add_argument("-alg", type=int, dest="algorithm", default=3,
                            help="algorithm to run. 0-paretoMCS, 1-unsatSat,\
                            2-pMinimal, 3-all intercalated")

        args = parser.parse_args(sys.argv[1:])
        print("args: %r\n" % args)

        if args.sandbox == "1":
            self.outputPath = os.path.join("./sandbox/", self.outputPath)
            if not os.path.exists(self.absolutize("./sandbox/")):
                os.mkdir(self.absolutize("./sandbox/"))
        self.outputPath = self.absolutize(self.outputPath)

        return args.algorithm

    def absolutize(self, relativePath):
        return os.path.abspath(relativePath)

    def test(self, alg):
        if(alg < 3):
            solverRange = range(alg,alg+1)
        else:
            solverRange = range(3)

        for fileName in os.listdir(self.testsPath):
            print("fileName:" + fileName)
            for solverI in solverRange:
                # print(os.path.join(testsPath, fileName))
                self.runSolver(fileName, solverI)

    def runSolver(self, fileName: str, solverI: int):
        outputName = os.path.basename(fileName)
        outputName = os.path.splitext(outputName)[0]
        outputName += "_S"+str(solverI)+".out"

        command = (self.runSolverPath + " "
                   "-W " + str(self.time) + " "
                   "-M " + str(self.memoryKB) + " "
                   "--timestamp "
                   "-w " + os.path.join(
                       self.outputPath,
                       self.watcherFilePrefix + outputName) + " "
                   "-o " + os.path.join(
                       self.outputPath,
                       self.solverOutputFilePrefix + outputName) + " "
                   "java -jar " + self.javaJarName + " "
                   "" + os.path.join(self.testsPath, fileName) + " "
                   #  "-v 2 "
                   "-alg " + str(solverI))

        # print(command)
        subprocess.call(command, shell=True)


tester = Tester(120, 9126000)
algorithm = tester.readArguments()
tester.test(algorithm)
