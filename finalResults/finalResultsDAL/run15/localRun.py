# run15
import Tester

testsPath = "instances/"
# taurus spica serpens sextans pictor pegasus scutum musca octans
# centaurus
servers = ("taurus", "spica", "sextans", "pictor", "pegasus",)
gateway = "aquila"
location = "./moco/finalResults/finalResultsDAL/run15"
algorithms = (1,)
encoder = "GTE"
time = 3600
useSandbox = 0

tester = Tester.Tester(location, servers, gateway, algorithms,
                       encoder, time, useSandbox, 2)
# tester.fillParameters()
tester.test()