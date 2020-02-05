import

servers = ("pictor", "octans", "pegasus")
gateway = "aquila"

tester = Tester(servers, gateway)
tester.fillParameters()
tester.test()
