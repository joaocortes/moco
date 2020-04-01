import Tester

testsPath = "instances/"
servers = ("pictor",)
gateway = "aquila"
location = "./moco/finalResults/finalResultsSC/run3/"
algorithms = (1,)
encoder = "GTE"
time = 1800


tester = Tester.Tester(location, servers, gateway, algorithms, encoder, time)
tester.fillParameters()
tester.test()
