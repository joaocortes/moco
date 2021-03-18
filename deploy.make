depRoot = deps
deployTarget = deployTarget

depsList := $(wildcard ./depsList/*)

deploy: deploy_deps deployMe

deploy_deps: 
	@for dep in $(depsList);\
		 do \
			if [ -f $$dep/deploy.make ]; \
			then make -C $$dep -f deploy.make deploy; \
			rsync -ruvapb --delete $$dep/$(deployTarget)/ ./$(depRoot)/`basename $$dep`; \
			else echo "How can I deploy $$dep?" ; exit 1; fi ; done

deployMe:
	@mkdir -p ./$(deployTarget)
	@rsync -ruvapb \
	--delete \
	--filter=": deployFilter.rsync" \
	--exclude="deployTarget/" \
	--exclude="bugs/" \
	--exclude=".git/" \
	--exclude="private/*" \
	. ./deployTarget

cleanDeploy:
	rm -rf deps/*
	rm -rf deployTarget/
