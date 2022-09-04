SHELL:=/bin/bash

# generate a mass of test cases from CSV with annotations. Each
# annotation (each line) will make an atomic test case and is
# internalized separately.

# name parts and dots in file names matter:
# XML with internal markup ../BASE.xml
# CSV with test cases       ./BASE.TEST-NAME.csv
# generated from above:
# list of test cases        ./BASE.TEST-NAME.cases
# Annotation test case      ./BASE.TEST-NAME.CASE_NUMBER.csv.annot
# internalized annotations  ./BASE.TEST-NAME.CASE_NUMBER.internalized.xml

# The generated XML files should be reviewed carefully, be corrected
# manually if necessary, and then be put under version control.

SOT ?= stack exec -- standoff

CSVS := $(wildcard *.csv)
HEADERS := $(patsubst %.csv,%.header,$(CSVS))
SPLITS := $(patsubst %.csv,%.01.csv.split,$(CSVS))
CASE_LISTS := $(patsubst %.csv,%.cases,$(CSVS))

# we need a second pass to find all splits
SPLITS_GEN := $(wildcard *.csv.split)
ANNOTS := $(patsubst %.csv.split,%.csv.annot,$(SPLITS_GEN))
INTERNALIZED_XML := $(patsubst %.csv.split,%.internalized.xml,$(SPLITS_GEN))

all:
	echo "Please run make twice: make splits && make annots"

splits: $(HEADERS) $(SPLITS)

annots: $(ANNOTS) rm_00 $(CASE_LISTS)

xml: annots $(INTERNALIZED_XML)

%.header: %.csv
	head -1 $< > $@

%.csv.split:
	split -l 1 -d --additional-suffix .csv.split \
	$(shell f=$@; echo $${f%.*.*.*}.csv) $(shell f=$@; echo $${f%.*.*.*}.) 

%.csv.annot: %.csv.split
	cat $(shell f=$@; echo $${f%.*.*.*}.header) $< > $@

%.shrinked.txt: ../%.xml
	$(SOT) -i $< -o $@ shrink \
	--offsets $(shell f=$@; echo $${f%.*.*}).offsets.dat \
	--config shrink.yaml

%.internalized.xml: %.csv.annot
	$(SOT) -i ../$(shell f=$@; echo $${f%.*.*.*.*}).xml -o $@ \
	internalize --csv-start-end \
	--offset-mapping $(shell f=$@; echo $${f%.*.*.*.*}).offsets.dat \
	--const ANNOT --attributes "feature-mapping.yaml" \
	$<

%.cases: %.csv
	ls -1 $(shell f=$@; echo $${f%.*}.*.csv.annot) > $@

# this removes files with double CSV headers
rm_00:
	rm -f *.00.csv.split
	rm -f *.00.csv.annot

clean**:
	rm -f *.internalized.xml

clean:
	rm -f *.csv.split
	rm -f *.csv.annot
	rm -f *.internalized.xml
	rm -f *.header
	rm -f *.cases
