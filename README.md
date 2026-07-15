# simah: a microsimulation model for estimating the impact of alcohol control policies on population health 

[![Tests](https://github.com/charlotteprobst/simah/actions/workflows/tests.yml/badge.svg)](https://github.com/charlotteprobst/simah/actions/workflows/tests.yml)
[![License: LGPL v3](https://img.shields.io/badge/License-LGPL_v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0)

## Overview

**simah** is an R-based microsimulation model designed to estimate the population-level health impacts of alcohol control policies. The model simulates individual-level trajectories of alcohol consumption, mortality rates, and health inequalities across population subgroups to evaluate the potential effects of various policy interventions.

### Key Features

- Individual-based microsimulation framework
- Integration of alcohol consumption patterns, mortality risk, and health outcomes
- Ability to model policy scenarios and their differential impacts across subpopulations

## Installation

### Prerequisites

- R (version 4.3 or higher)
- RStudio (recommended)

### Github

```r
# Install directly from Github
pak::pak("charlotteprobst/simah")
```

### From source
```bash
# clone the repository
git clone https://github.com/charlotteprobst/simah.git
cd simah
```

```r
# Install in R
Rscript -e "devtools::install_dev_deps()"
Rscript -e "devtools::load_all('.')"
```

### Using Conda
```bash
conda env create -f simah_env.yaml
conda activate simah_r_env
```

### Setup: Download Required Data Files
The model requires external data files for simulation:
```bash
# Download required data files
curl -L -o inputs_data/data.rds https://figshare.com/ndownloader/files/65734905?private_link=815877ea16ec0d611e36
curl -L -o inputs_data/svy_data.rds https://figshare.com/ndownloader/files/65734908?private_link=815877ea16ec0d611e36
```

## Usage

### Basic Example
```r
# Load the package
library(simah)

# Run the microsimulation
results <- microsimulation()

# View summary statistics
summary(results)
```

### Running tests
```r
# Run all tests
devtools::test()

# Run specific test file
devtools::test("testthat/test-microsimulation.R")
```

### Developing with simah
```r
# Install development dependencies
devtools::install_dev_deps()

# Load package in development mode
devtools::load_all('.')

# Build and install
devtools::install()

# Check package
devtools::check()
```


## Contributors

- Charlotte Probst (principal): [charlotte.probst@camh.ca](mailto:charlotte.probst@camh.ca) | [ORCID](https://orcid.org/0000-0003-4360-697X)
- Julia Lemp: [julia.lemp@uni-heidelberg.de](mailto:julia.lemp@uni-heidelberg.de) | [ORCID](https://orcid.org/0000-0002-1524-3641)
- Xinyi Kou: [x.kou@sheffield.ac.uk](mailto:x.kou@sheffield.ac.uk) | [ORCID](https://orcid.org/0000-0002-3635-6178)
- Charlotte Buckley: [charlotte.buckley@liverpool.ac.uk](mailto:charlotte.buckley@liverpool.ac.uk) | [ORCID](https://orcid.org/0000-0002-8430-0347)
- João A. Duro: [j.a.duro@sheffield.ac.uk](mailto:j.a.duro@sheffield.ac.uk) | [ORCID](https://orcid.org/0000-0002-7684-4707)
- Robin Purshouse: [r.purshouse.sheffield.ac.uk](mailto:r.purshouse.sheffield.ac.uk) | [ORCID](https://orcid.org/0000-0001-5880-1925)

## License

This project is licensed under the GNU Lesser General Public License v3.0 (LGPL-3.0). See [LICENSE](LICENSE) for details.