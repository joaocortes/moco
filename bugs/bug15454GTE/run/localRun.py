import Tester

testsPath = "instances/"
servers = ("pictor",)
gateway = "aquila"
location = "./"
algorithms = (1,)
encoder = "GTE"
time = 1800
useSandbox=0

tester = Tester.Tester(location, servers, gateway, algorithms,
                       encoder, time, useSandbox)
# tester.fillParameters()
tester.test()
