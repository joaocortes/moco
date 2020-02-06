import Tester

testsPath = "instances/"
servers = ("centaurus",)
gateway = "aquila"
location = "./moco/finalResults/finalResultsSC/run2/"
algorithms = (1, 2)


tester = Tester.Tester(location, servers, gateway, algorithms)
tester.fillParameters()
tester.test()
