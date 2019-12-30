import socket
servers = ("centaurus", "scutum", "octans")
gateway = "aquila"
superServer = servers[0]
me = "JC"


def sshServer(command: str, server: str):
    hostname = socket.gethostname()
    if hostname == superServer:
        if not server == hostname:
            command = "ssh " + server +\
                " -F /dev/null " + " \"" + command + "\""
    else:
        command = "ssh " + server + " -F /dev/null " + " \\\"" + \
            command + "\\\""
        command = "ssh " + "aquila " + " \"" + command + "\""
    return command
