import Tester

testsPath = "instances/"
# taurus spica serpens sextans pictor pegasus scutum musca octans
# centaurus
servers = ("taurus", "sextans", "pictor", "scutum")
gateway = "aquila"
location = "./moco/finalResults/finalResultsSC/run6/"
algorithms = (1,)
encoder = "SWC"
time = 3600
useSandbox = 0

tester = Tester.Tester(location, servers, gateway, algorithms,
                       encoder, time, useSandbox, 3)
# tester.fillParameters()
tester.test()
