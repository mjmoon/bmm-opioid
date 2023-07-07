# variables ----------
SCRIPT_DIR="R/scripts/"
SCRIPT=""
RMD_FILE="README.Rmd"

rscript:
	Rscript $(SCRIPT_DIR)$(SCRIPT)
rmarkdown:
	Rscript -e "rmarkdown::render('${RMD_FILE}', envir = new.env())"

# simulation ------------
gen-sim:
	$(MAKE) SCRIPT="sim/01-01-set-simulation-parameters.R" rscript
	$(MAKE) SCRIPT="sim/01-02-simulate-data.R" rscript
fit-sim:
	$(MAKE) SCRIPT="sim/02-01-fit-without-ratio-estimates.R" rscript
	$(MAKE) SCRIPT="sim/02-02-fit-with-ratio-estimates.R" rscript
plot-sim:
	$(MAKE) SCRIPT="sim/03-01-diagnostics-without-ratio-estimates.R" rscript
	$(MAKE) SCRIPT="sim/04-01-posterior-without-ratio-estimates.R" rscript
	$(MAKE) SCRIPT="sim/03-02-diagnostics-with-ratio-estimates.R" rscript
	$(MAKE) SCRIPT="sim/04-02-posterior-with-ratio-estimates.R" rscript
fit-sa:
	$(MAKE) SCRIPT="sim/02-03-fit-sensitivity-analysis.R" rscript
plot-sa:
	$(MAKE) SCRIPT="sim/03-03-diagnostics-sensitivity-analysis.R" rscript
	$(MAKE) SCRIPT="sim/04-03-posterior-sensitivity-analysis.R" rscript


# us case study ---------
download-us:
	$(MAKE) SCRIPT="us/01-01-download-data.R" rscript
parse-us:
	$(MAKE) SCRIPT="us/01-02-parse-data.R" rscript
fit-us-black:
	$(MAKE) SCRIPT="us/02-02b-fit-black.R" rscript
fit-us-white:
	$(MAKE) SCRIPT="us/02-02w-fit-white.R" rscript
fit-us:
	$(MAKE) fit-us-black fit-us-white
plot-us-black:
	$(MAKE) SCRIPT="us/03-01b-diagnostics.R" rscript
	$(MAKE) SCRIPT="us/03-02b-posterior.R" rscript
plot-us-white:
	$(MAKE) SCRIPT="us/03-01w-diagnostics.R" rscript
	$(MAKE) SCRIPT="us/03-02w-posterior.R" rscript
plot-us:
	$(MAKE) plot-us-black plot-us-white

# publications and presentations ---------------------
plot-paa:
	$(MAKE) SCRIPT="03-paa-poster.R" rscript
plot-manu:
	$(MAKE) SCRIPT="01-man-sims.R" rscript
	$(MAKE) SCRIPT="02-man-us.R" rscript

# knit -----------
knit-readmes:
	$(MAKE) RMD_FILE="README.Rmd" rmarkdown
	$(MAKE) RMD_FILE="results/sim/README.Rmd" rmarkdown
	$(MAKE) RMD_FILE="results/us/README.Rmd" rmarkdown
