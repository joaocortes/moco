depRoot = deps
deployTarget = deployTarget
default: deployMe

depsList := $(wildcard ./depsList/*)

deploy: deploy_deps deployMe

deploy_deps: 
	@for dep in $(depsList);\
		 do \
			if [ -f $$dep/sync.make ]; \
			then make -C $$dep -f sync.make deploy; \
			rsync -ruvb --delete $$dep/$(deployTarget)/ ./$(depRoot)/`basename $$dep`; \
			else echo "How can I deploy $$dep?" ; exit 1; fi ; done

deployMe:
	@mkdir -p ./$(deployTarget)
	@rsync -ruvb \
	--delete \
	--include="*.make" \
	 --filter=":- .gitignore" \
	--exclude="deployTarget/" \
	--exclude="bugs/" \
	--exclude=".git/" \
	--exclude="private/*" \
	. ./deployTarget

