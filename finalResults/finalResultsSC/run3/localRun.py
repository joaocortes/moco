import Tester

testsPath = "instances/"
servers = ("taurus", "pegasus", "pictor", "scutum", "octans")
gateway = "aquila"
location = "./moco/finalResults/finalResultsSC/run3/"
algorithms = (1,)
encoder = "GTE"
time = 10


tester = Tester.Tester(location, servers, gateway, algorithms, encoder, time)
tester.fillParameters()
tester.test()
