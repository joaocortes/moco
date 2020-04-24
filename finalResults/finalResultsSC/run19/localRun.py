# run19
import Tester

testsPath = "instances/"
# taurus spica serpens sextans pictor pegasus scutum musca octans
# centaurus
servers = ("taurus",)
gateway = "aquila"
location = "./moco/finalResults/finalResultsSC/run19"
algorithms = (2,)
encoder = "GTE"
time = 3600
useSandbox = 0

tester = Tester.Tester(location, servers, gateway, algorithms,
                       encoder, time, useSandbox, 4)
# tester.fillParameters()
tester.test()
