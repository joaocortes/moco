import Tester
# Run with GTE optimized, without bindLeaf calls
testsPath = "instances/"
# taurus spica serpens sextans pictor pegasus scutum musca octans
# centaurus
servers = ("taurus", "spica", "serpens", "pictor", "pegasus",
           "scutum", "musca", "octans", "sextans")
gateway = "aquila"
location = "./moco/finalResults/finalResultsSC/run8/"
algorithms = (1,)
encoder = "GTE"
time = 3600
useSandbox = 0

tester = Tester.Tester(location, servers, gateway, algorithms,
                       encoder, time, useSandbox, 3)
# tester.fillParameters()
tester.test()
