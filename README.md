# Collecting Perishable Critical Infrastructure Operational Data for May 2024 Space Weather Events

## Overview

The **SW_May_2024** repository is part of the [ChronoStorm project](https://chronostorm.vercel.app/), funded by the U.S. National Science Foundation (NSF) under grant [#2434136](https://www.nsf.gov/awardsearch/showAward?AWD_ID=2434136&HistoricalAwards=false). This project aims to collect and analyze perishable operational data from critical infrastructure sectors during the May 2024 space weather events, and over the solar maximum of solar cycle 25. The insights derived will enhance our understanding of the vulnerabilities and resilience of such infrastructures to solar phenomena.

## Repository Contents

- `power_survey_panel.R`: Script for processing survey responses related to power sector.
- `power_interview_panel.R`: Script for processing interview responses related to power sector.
- `satellite_survey_panel.R`: Script for processing survey responses related to satellite sector.
- `satellite_interview_panel.R`: Script for processing interview responses related to satellite sector.
- `aviation_interview_panel.R`: Script for processing interview responses related to aviation sector.
- `figures/`: Directory containing visualizations generated from the data analyses.
- `ElectricityResponse.csv`: CSV containing responses from the online survey related to power sector.
- `SatelliteResponse.csv`: CSV containing responses from the online survey related to power sector.
- `.gitignore`: Specifies intentionally untracked files to ignore.
- `LICENSE`: The repository is licensed under the MIT License.

## Getting Started

To utilize the scripts in this repository:

1. **Clone the Repository**:
   ```bash
   git clone https://github.com/caitlinlaneve/SpaceWeatherMitigation.git
   ```

2. **Navigate to the Directory**:
   ```bash
   cd SpaceWeatherMitigation
   ```

3. **Install Required R Packages**:

   Make sure you have R installed. Then install the necessary packages:
   ```R
   install.packages(c("tidyverse", "ggplot2", "readr"))
   ```

4. **Run the Scripts**:
   ```R
   source("power_survey_panel.R")
   source("power_interview_panel.R")
   source("satellite_survey_panel.R")
   source("satellite_panel_responses.R")
   source("satellite_interview_panel.R")
   source("aviation_interview_panel.R")
   ```

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Acknowledgments

We gratefully acknowledge support from the U.S. National Science Foundation grant entitled:

> *RAPID: Collecting Perishable Critical Infrastructure Operational Data for May 2024 Space Weather Events* (#2434136)

This grant is co-funded by the GEO/AGS Space Weather Research program and the ENG/CMMI Humans, Disasters, and the Built Environment (HDBE) program.

## Contact

For more information, please contact [Caitlin LaNeve](https://github.com/caitlinlaneve) (claneve [ at ] gmu [ dot ] edu).

## Contributors
- Edward Oughton - Primary Investigator
- Caitlin LaNeve - Graduate Research Assistant
- Noah Rivera - Undergraduate Research Assistant
- Bob Weigel - Senior Personnel

