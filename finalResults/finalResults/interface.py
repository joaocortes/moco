servers = ("centaurus", "scutum", "octans")
gateway = "aquila"
me = "me"


def sshServer(command: str, server: str):

    command = "ssh " + server + " -F /dev/null " + " \\\"" + command + "\\\""
    # if not server == servers[0]:
    command = "ssh " + "aquila " + " \"" + command + "\""
    return command
