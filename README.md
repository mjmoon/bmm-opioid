
*Updated: 2023-07-07.*

## Bayesian Multistate Modelling of the Opioid Crisis

The project aims to provide a theoretical for estimating the transition
probabilities between different stages of opioid use.

In this repository, you will find all codes related to the project and
short summaries and plots. To reproduce the analysis, run the following
`make` commands in the project directory:

**Simulation**

- `make gen-sim`: Generate data for simulation.
- `make fit-sim`: Fit simulated data both using the ratio estimates and
  without using the ratio estimates.
- `make plot-sim`: Generate diagnostic and exploratory plots of the
  fitted estimates.

**US case study**

- `make download-us`: Download data files containing [multiple causes of
  death
  reports](https://www.cdc.gov/nchs/data_access/vitalstatsonline.htm#Mortality_Multiple)
  and [National Survey on Drug Use and
  Health](https://www.samhsa.gov/data/data-we-collect/nsduh-national-survey-drug-use-and-health).
- `make parse-us`: Parse the downloaded data files to prepare for
  analysis.
- `make fit-us`: Fit the parsed data for both non-Hispanic black and
  non-Hispanic white US populations.
- `make plot-us`: Generate diagnostic and exploratory plots of the
  fitted estimates.

**Publications and presentations**

- `make plot-paa`: Generate plots used for the PAA 2023 poster.

You can find the resulting plots in the following directories:

- [Simulation](results/sim)
- [US case study](results/us)
- [PAA 2023 poster](results/paa-poster)
- [Manuscript](results/man)

------------------------------------------------------------------------

### Authors

- Michael J. Moon, University of Toronto [<img
  src="README_files/figure-gfm/fa-icon-48b1da2867ad7312ef1fde4a125efd4e.svg"
  style="width:1.12em;height:1em" />](//micbon.com)
- Monica Alexander, University of Toronto [<img
  src="README_files/figure-gfm/fa-icon-48b1da2867ad7312ef1fde4a125efd4e.svg"
  style="width:1.12em;height:1em" />](//monicaalexander.com)
