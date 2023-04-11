# help:
# 	@echo "Usage: make -i SRC=<path/file> -> to make a specific file"
# 	@echo "       make -i                 -> to make all altered files"

.PHONY: build build_singularity run code_container docs_container R_container code_local docs_local code_singularity docs_singularity

build:
	docker build . --tag coral_indicators

build_singularity:
	docker save coral_indicators -o coral_indicators.tar 
	singularity build coral_indicators.sif docker-archive://coral_indicators.tar

# Run interactive R session in docker container
R_container:
	docker run --rm -it -v "$(shell pwd)":/home/Project coral_indicators R

code_container:
	docker run --rm -v "$(shell pwd)":/home/Project coral_indicators $(MAKE) -f R/Makefile

docs_container:
	docker run --rm -v "$(shell pwd)":/home/Project coral_indicators $(MAKE) -f docs/Makefile

code_local:
	$(MAKE) -f R/Makefile

docs_local:
	$(MAKE) -f docs/Makefile

site_local:
	@echo "Transfer to docs/Makefile"
	$(MAKE) -f docs/Makefile.site

code_singularity:
	@echo "Transfer to R/Makefile"
	module load singularity
	singularity exec -B .:/home/Project coral_indicators.sif $(MAKE) -f R/Makefile

docs_singularity:
	@echo "Transfer to docs/Makefile"
	module load singularity
	singularity exec -B .:/home/Project coral_indicators.sif $(MAKE) -f docs/Makefile

clean:
	rm -f *.log *.aux *.md *.out texput.log
