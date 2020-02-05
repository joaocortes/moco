import Tester

testsPath = "instances/"
servers = ("centaurus",)
gateway = "aquila"

tester = Tester.Tester(servers, gateway)
tester.fillParameters()
tester.test()
