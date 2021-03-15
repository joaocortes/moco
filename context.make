# This file does a context switch to the context named
# 'remote'. Actually the name of the context is the name of the
# makefile with the .make extension, to simplify things.

# First, take care of dependencies,
deps := $(wildcard ./deps/*)
# Then, take care of myself.

build: build_deps build_self 

build_self:
	@if [ -f $$dep/$(context).make ]; \
		then make -f $(context).make; fi

build_deps: deploy_deps
	@for dep in $(deps);\
		 do \
			if [ -f $$dep/$(context).make ]; \
			then make -C $$dep -f context.make context=$(context) ; \
			else echo "$$dep context not defined" ; fi ; done


include sync.make
