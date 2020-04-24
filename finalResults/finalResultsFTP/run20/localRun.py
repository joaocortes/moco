# run20
import Tester

testsPath = "instances/"
# taurus spica serpens sextans pictor pegasus scutum musca octans
# centaurus
servers = ("spica", "serpens", "sextans",)
gateway = "aquila"
location = "./moco/finalResults/finalResultsFTP/run20"
algorithms = (1,)
encoder = "GTE"
time = 3600
useSandbox = 0

tester = Tester.Tester(location, servers, gateway, algorithms,
                       encoder, time, useSandbox, 4)
# tester.fillParameters()
tester.test()
