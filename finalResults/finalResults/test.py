import argparse
import sys
import subprocess
import os
from datetime import datetime


def readArguments():
    parser = argparse.ArgumentParser(
        "testing script to compare the 3 algorithms for finding the pareto"
        "Front."
        )
    parser.add_argument("-s", type=str, dest="useSandbox", default="1",
                        help="sandbox results?")
    parser.add_argument("-alg", type=int, dest="algorithm", default=3,
                        help="algorithm to run. 0-paretoMCS, 1-unsatSat,\
                        2-pMinimal, 3-all intercalated")
    parser.add_argument("-t", type=int, dest="time", default=10,
                        help="runout time")
    parser.add_argument("-m", type=int, dest="memoryKB", default=10000000,
                        help="runout memory, in KB")

    args = parser.parse_args(sys.argv[1:])
    print("args: %r\n" % args)
    return args


def sshServer(command: str, server: str):
    command = "ssh " + server + " -F /dev/null " + " \\\"" + command + "\\\""
    command = "ssh " + "aquila " + "\"" + command + "\""
    return command


servers = ("centaurus", "scutum", "octans")

nada = "sh -c \" sh -c \\\"echo \"nada\" \\\"\""

class Tester():

    def __init__(self):
        self.time = None
        self.memoryKB = None
        self.args = None
        self.algorithm = None
        self.useSandbox = None
        self.part = None

        self.javaJarName = ("../target/org.sat4j.moco.threeAlgorithms-"
                            "0.0.1-SNAPSHOT-jar-with-dependencies.jar")
        self.testsPath = "convertedInstancesSC/"
        self.outputPath = "output/"
        self.watcherFilePrefix = "watcher_"
        self.solverOutputFilePrefix = "solver_"
        # do not remove the "./" from the runsolver pathname, otherwise
        self.runSolverPath = "./runsolver"
        self.sandbox = "sandbox"
        self.tablePath = os.path.join(self.outputPath,
                                      "table_"+str(datetime.timestamp(
                                          datetime.now()))+".txt")

    def test(self):
        if self.useSandbox == "1":
            self.outputPath = os.path.join("./sandbox/", self.outputPath)
            if not os.path.exists(self.outputPath):
                os.makedirs(self.outputPath)

        if(self.algorithm < 3):
            solverRange = range(self.algorithm, self.algorithm+1)
        else:
            solverRange = range(3)

        listFiles = os.listdir(self.testsPath)
        numberFiles = len(listFiles)
        listFilesPart1 = listFiles[:numberFiles//2]
        listFilesPart2 = listFiles[numberFiles//2:]
        for solverI in solverRange:
            serverI = solverI

            process1 = self.runSolver(listFilesPart1, solverI, serverI)
            process2 = self.runSolver(listFilesPart2, solverI, serverI)

            process1.wait()
            process2.wait()

        print("done")

    def runSolver(self, listFiles: list, solverI: int, serverI: int):
        command = "cd moco/finalResults;"
        for fileName in listFiles:
            print("fileName: " + fileName)
            outputName = os.path.basename(fileName)
            outputName = os.path.splitext(outputName)[0]
            outputName += "_S"+str(solverI)+".out"
            command += (self.runSolverPath + " "
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
            command += " && "

        command += ":"
        print(command)
        # command = re.escape(command)
        command = sshServer(command, servers[serverI])
        print(command)
        process = subprocess.Popen(command, shell=True)

        return process


tester = Tester()
args = readArguments()
tester.memoryKB = args.memoryKB
tester.time = args.time
tester.algorithm = args.algorithm
tester.useSandbox = args.useSandbox
tester.test()
