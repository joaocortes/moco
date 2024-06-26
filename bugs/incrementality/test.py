import sys
import argparse
import subprocess
import os
from datetime import datetime
import shellInterface
from shellInterface import sshServer
from shellInterface import servers

javaJarName = ("./org.sat4j.moco.threeAlgorithms-"
               "0.0.1-SNAPSHOT-jar-with-dependencies.jar")
testsPath = "instances/"
outputPath = "output/"
watcherFilePrefix = "watcher_"
solverOutputFilePrefix = "solver_"
runSolverPath = "./runsolver"
sandbox = "sandbox"
tablePath = os.path.join(outputPath,
                         "table_"+str(datetime.timestamp(
                             datetime.now()))+".txt")


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


class Tester():

    def __init__(self):
        self.time = None
        self.memoryKB = None
        self.algorithm = None
        self.useSandbox = None
        self.part = None
        self.commands = []
        self.args = None

    def fillParameters(self):
        self.args = readArguments()
        self.memoryKB = self.args.memoryKB
        self.time = self.args.time
        self.algorithm = self.args.algorithm
        self.useSandbox = self.args.useSandbox

    def test(self):
        if self.useSandbox == "1":
            completeOutputPath = os.path.join("./sandbox/", outputPath)
            if not os.path.exists(completeOutputPath):
                os.makedirs(completeOutputPath)

        if(self.algorithm < 3):
            solverRange = range(self.algorithm, self.algorithm+1)
        else:
            solverRange = range(3)

        listFiles = os.listdir(testsPath)
        numberFiles = len(listFiles)
        listFilesPart1 = listFiles[:numberFiles//2]
        listFilesPart2 = listFiles[numberFiles//2:]
        for solverI in solverRange:
            self.generateCommands(listFilesPart1, solverI)
            self.generateCommands(listFilesPart2, solverI)
        self.distributeCommands()
        for process in self.runCommands():
            process.wait()
        print("done!")

    def distributeCommands(self):
        commandI = 0
        for command in self.commands:
            server = servers[commandI // 2]
            location = "./moco/bugs/incrementality/"
            command = sshServer(command, server, location)
            self.commands[commandI] = command
            # print(command)
            commandI += 1

    def runCommands(self):
        processes = []
        for command in self.commands:
            print(command)
            processes.append(subprocess.Popen(command, shell=True))
        return processes

    def generateCommands(self, listFiles: list, solverI: int):
        command = ""
        for fileName in listFiles:
            print("fileName: " + fileName)
            outputName = os.path.basename(fileName)
            outputName = os.path.splitext(outputName)[0]
            outputName += "_S"+str(solverI)+".txt"

            tokens = [runSolverPath,
                      "-W ", str(self.time),
                      "-M ", str(self.memoryKB),
                      "--timestamp "
                      "-w ", os.path.join(
                          outputPath,
                          watcherFilePrefix + outputName),
                      "-o ", os.path.join(
                          outputPath,
                          solverOutputFilePrefix + outputName),
                      "java -jar ", javaJarName,
                      "", os.path.join(testsPath, fileName),
                      #  "-v 2 "
                      "-alg ", str(solverI), ";"]
            command += shellInterface.buildCommand(tokens)

        # command = re.escape(command)
        self.commands.append(command)


tester = Tester()
tester.fillParameters()
tester.test()
