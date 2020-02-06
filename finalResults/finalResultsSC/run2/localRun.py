import Tester

testsPath = "instances/"
servers = ("taurus",)
gateway = "aquila"
location = "./moco/finalResults/finalResultsSC/run2/"
algorithms = (2,)


tester = Tester.Tester(location, servers, gateway, algorithms)
tester.fillParameters()
tester.test()
