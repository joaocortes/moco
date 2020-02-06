import socket
import shlex


class Interface:

    def __init__(self, servers, gateway):
        self.servers = servers
        self.superServer = servers[0]
        self.gateway = gateway

    def escapeCommand(command: str):
        tokens = shlex.split(command)
        command = ' '.join([shlex.quote(token) for token in tokens])
        return command


    def buildCommand(self, tokens):
        command = ' '.join(tokens)
        return command

    def screenifyCommand(self, command: str):
        # tokens = ["screen", "-dm", "\"", command, "\""]
        # command = buildCommand(tokens)
        return command

    def sshServer(self, command: str, server: str, location: str, screenify=True):

        tokens = ["cd", shlex.quote(location), ";", command]
        commandToSend = self.buildCommand(tokens)
        # if screenify:
        #     commandToSend = screenifyCommand(commandToSend)
        tokens = []
        hostname = socket.gethostname()
        if hostname == self.superServer:
            if not hostname == server:
                tokens = ["ssh", server, "-F", "/dev/null", shlex.quote(commandToSend)]
        else:
            tokens1 = ["ssh ", server, "-F", "/dev/null ", shlex.quote(commandToSend)]
            tokens = ["ssh", self.gateway, shlex.quote(self.buildCommand(tokens1))]
            command = self.buildCommand(tokens)
        return command
