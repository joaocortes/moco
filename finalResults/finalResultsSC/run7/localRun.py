import Tester

testsPath = "instances/"
# taurus spica serpens sextans pictor pegasus scutum musca octans
# centaurus
servers = ("taurus", "spica", "serpens", "sextans", "pictor", "pegasus",
           "scutum")
gateway = "aquila"
location = "./moco/finalResults/finalResultsSC/run7/"
algorithms = (2,)
encoder = "GTE"
time = 3600
useSandbox = 0

tester = Tester.Tester(location, servers, gateway, algorithms,
                       encoder, time, useSandbox, 3)
# tester.fillParameters()
tester.test()
