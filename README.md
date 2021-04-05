[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# UK COVID-19 Firm Creation
## Replication Repository

Analysis of [Companies House](https://www.gov.uk/government/organisations/companies-house) new business registrations [data](http://download.companieshouse.gov.uk/en_output.html). This repo hosts the code, data files and R Notebook and allows to replicate the [monthly report analysis](https://uk-covid19-firm-creation.netlify.app/reports/).

## Repo folders
### Input
Files used for the regional and sectoral analysis. It includes:
- `convertedPC2country.csv`: postcode areas to countries 
- `Postcodes summaryCLEAN.csv`: postcode areas to longitude and latitude details
- `sic2007conversion`: UK SIC conversion to ONS Sections

### Ouput
It includes the random sample in January 2021 and January 2021. Further, it includes the results of the analysis in `.csv` format.
#### Data
- `replicData2021.csv`: random sample for January 2021
- `replicData2019.csv`: random sample for January 2019

#### Results
- `repByDay.csv`: new firm registrations per day
- `byDayPC`: new firm registrations per day and postcode area
- `byDaySector.csv`: new firm registrations per day and ONS Section
- `bySectorCompareALL.csv`: new firm registrations per ONS Section in a particular month



For comments and feedback, [contact us](mailto:i.galanakis@kent.ac.uk).
