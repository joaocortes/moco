import Tester

testsPath = "instances/"
servers = ("spica", "serpens","sextans", "pictor", "scutum", "musca")
gateway = "aquila"
location = "./moco/finalResults/finalResultsSC/run4"
algorithms = (1,)
encoder = "GTE"
time = 3600
useSandbox = 0

tester = Tester.Tester(location, servers, gateway, algorithms,
                       encoder, time, useSandbox)
# tester.fillParameters()
tester.test()
