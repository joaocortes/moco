import socket
import shlex
from localRun import *
superServer = servers[0]

def escapeCommand(command: str):
    tokens = shlex.split(command)
    command = ' '.join([shlex.quote(token) for token in tokens])
    return command


def buildCommand(tokens):
    command = ' '.join(tokens)
    return command


def screenifyCommand(command: str):
    # tokens = ["screen", "-dm", "\"", command, "\""]
    # command = buildCommand(tokens)
    return command


def sshServer(command: str, server: str, location: str, screenify=True):

    tokens = ["cd", shlex.quote(location), ";", command]
    commandToSend = buildCommand(tokens)
    # if screenify:
    #     commandToSend = screenifyCommand(commandToSend)
    tokens = []
    hostname = socket.gethostname()
    if hostname == superServer:
        if not hostname == server:
            tokens = ["ssh", server, "-F", "/dev/null", shlex.quote(commandToSend)]
    else:
        tokens1 = ["ssh ", server, "-F", "/dev/null ", shlex.quote(commandToSend)]
        tokens = ["ssh", gateway, shlex.quote(buildCommand(tokens1))]
    command = buildCommand(tokens)
    return command
