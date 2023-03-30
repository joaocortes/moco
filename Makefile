jar = ./target/org.sat4j.moco.threeAlgorithms-0.0.1-SNAPSHOT.jar
source = $(shell find  ./src -type f -name *.java)
build: $(jar)

$(jar): $(source)
	mvn -T1C -o -Dmaven.test.skip -DskipTests package
clean:
	mvn clean
