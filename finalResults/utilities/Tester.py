import math
import sys
import argparse
import subprocess
import os
from datetime import datetime
import ShellInterface

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
    parser.add_argument("-m", type=int, dest="memoryKB", default=10000000,
                        help="runout memory, in KB")

    args = parser.parse_args(sys.argv[1:])
    print("args: %r\n" % args)
    return args


class Tester:

    def __init__(self, location, servers, gateway, algorithms=(1, 2, 3),
                 encoderGD="GTE", time=10, useSandbox=1, part=3):
        self.location = location
        self.time = time
        self.memoryKB = 10000000
        # TODO: algorithm should be sensitive to the user
        self.algorithm = 4
        self.encoderGD = encoderGD
        self.useSandbox = useSandbox
        self.part = part
        self.commands = []
        self.args = None
        self.shellInterface = ShellInterface.Interface(servers, gateway)
        self.solverRange = algorithms
        self.completeOutputPath = outputPath

    def fillParameters(self):
        self.args = readArguments()
        self.memoryKB = self.args.memoryKB
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
            solverRange = self.solverRange

        listFiles = os.listdir(testsPath)
        numberFiles = len(listFiles)
        parts = len(self.shellInterface.servers)*self.part
        listFilesParts = list()
        for i in range(0, parts):
            listFilesParts.append(listFiles[i * numberFiles//parts:
                                            (i+1) * numberFiles//parts])
            print("part " + str(i) + ":")
            print(listFilesParts[i])
        for solverI in solverRange:
            for i in range(0, parts):
                self.generateCommands(listFilesParts[i], solverI)
            self.distributeCommands()
        for process in self.runCommands():
            process.wait()
        print("done!")

    def distributeCommands(self):
        commandI = -1
        for command in self.commands:
            commandI += 1
            serverI = math.floor(commandI *
                                 len(self.shellInterface.servers) / len(self.commands))
            server = self.shellInterface.servers[serverI]
            location = self.location
            command = self.shellInterface.sshServer(command, server, location)
            self.commands[commandI] = command
            # print(command)

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
                          self.completeOutputPath,
                          watcherFilePrefix + outputName),
                      "-o ", os.path.join(
                          self.completeOutputPath,
                          solverOutputFilePrefix + outputName),
                      "java -jar ", javaJarName,
                      "", os.path.join(testsPath, fileName),
                      #  "-v 2 "
                      "-alg ", str(solverI),
                      "-enc " "\"" + self.encoderGD + "\"",
                      ";"]
            command += self.shellInterface.buildCommand(tokens)

        # command = re.escape(command)
        self.commands.append(command)