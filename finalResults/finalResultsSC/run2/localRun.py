import Tester

testsPath = "instances/"
servers = ("centaurus",)
gateway = "aquila"
location = "./moco/finalResults/finalResultsSC/run2/"
tester = Tester.Tester(location, servers, gateway)
tester.fillParameters()
tester.test()
