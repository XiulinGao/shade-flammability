Shade-Flammability
===================

Project goals
-------------

- Determine the relationship between shade tolerance and flammability in 17 grass species.
- Examine how species variations in fuel moisture content, phenology, leaf and culm traits, and canopy architecture traits will cause differences in flammability.
- Determine how different grass species response to fire + aboveground biomass clipping.

Methods
---------

We grew plants in greenhouse from seeds (requested from USDA or bought from the nature's seed company), and applied 2 different light level treatments: 0% shade and 50% shade. For each light treatment we have 5 reps per species and these are located in 5 different blocks. Each block is evenly split into 0% and 50% light treatment. Within each pot we had 2 seedlings planted because that we used one for aboveground biomass measurement 4 months after treatment and the one for burning experiments 1 year after treatment.

See table 1 for plant trait measurements and table 2 for flammability measurements.

*Table 1. Plant trait measurements*

| Trait                                           | plant burned | plant not burned |
| -----                                           | ------------ | ---------------- |
| Leaf area                                       | yes          | yes              |
| Leaf thickness                                  | yes          | no               |
| Leaf dry mass                                   | yes          | yes              |
| Tiller number                                   | yes          | no               |
| Tiller thickness                                | yes          | no               |
| Biomass density                                 | yes          | no               |
| Biomass height ratio                            | predicted    | no               |
| Leaf to non-leaf mass ratio                     | yes          | no               |
| Fuel moisture content                           | yes          | no               |
| Live mass to dead mass ratio (fresh mass basis) | yes          | no               |

*Table 2. Flammability measurements*
 
| Fire behavior component | Direct measurements                                         |
| ----------------------- | -------------------                                         |
| Consumability           | initial weight, end weight                                  |
| Fire intensity          | rate of biomass loss (balance data), maximum flame height   |
| Flame temperature       | thermocouple measurements (hobo data)                       |
| Heat release            | temperature of aluminum disk                                |
| Ignition delay          | time to ignition                                            |
| Flaming combustion      | time period when solid flame is seen                        |
| Smoldering combustion   | time period since extinction of flame to no smoke or ember  |
					
Data
------
Each data file referred to has an associated machine readable metadata file. If data file is named as e.g. canopy-tiller.csv, then associated metadata file is names as canopy-tiller-metadata.csv. The metadata file describes each variable (column) in the data file including data type, unit and a brief description.

### Species information

Studied species information is stored in data/species-info.csv, currently the file contains only species code that is used by USDA plant base and its scientific name.

###  Plant traits 

1. data/SLA-needle.csv and data/SLA-wide.csv store leaf trait measurements including leaf area (wide leaf), oven-dried leaf mass, leaf thickness, and leaf section length (for leaf resembles needle). Leaf area is directly measured for wide leaves with scanning the leaf and analyze scanned image using image J. To obtain leaf area for leaves resemble needle, we measured leaf thickness and length. Leaf area will be calculated by assuming the leaf section as cylinder.

2. data/canopy-tiller.csv and data/canopy-tiller-no-cylinder.csv store canopy traits (plant height and width), tiller number and tiller thickness measurements. Because measurements after the first burning trial were taken with enclosing the plants inside a cylinder made of wire cloth to keep biomass more packed, so canopy traits were measured in different ways and thus stored separately. To calculate plant volume for plants enclosed in the cylinder, we assume that volume of each plant is composed of two inverted truncated cones (bottom portion before 80% of the plant mass touching cylinder, top portion extend out the cylinder) and one cylinder (portion enclosed inside the cylinder starting at the point where 80% plant mass touching cylinder). To calculate volume of plants without being enclosed in cylinder, we assume each plant as an inverted truncated cone.

**Dimension of the used cylinder is 30cm in height and 12.6 cm in diameter**

3. Tiller thickness is measured by taking 3 tillers per plant, and measuring thickness at the bottom, middle, and top portion of each tiller at two perpendicular directions at each point.


4. data/fmc-leaf-culm-ratio.csv stores the total fresh mass of sub-sample from each burned plant, fresh mass of dead material of sub-sample, oven-dried leaf mass and oven-dried non-leaf mass for the sub-sample. The information is used to calculate fuel moisture content of each burned plant, dead to live material ratio (fresh mass based curing rate), and leaf to non-leaf mass ratio (dry mass based).

5. data/shade-flam-initial-biomass.csv stores the initial oven-dried aboveground biomass for each species by harvesting 10-12 seedlings right before light treatments were applied. data/shade-flam-final-biomass.csv stores the final oven-dried aboveground biomass 4 months after the light treatment by harvesting one plant in each pot. These measurements are used to assess how different species response to 50% shade by comparing the relative aboveground biomass gained by plant growing under shade.

6. data/burning-trial.csv stores flammability measurements including consumability, aluminum disc temperature, maximum flame height, ignition delay, flaming and smoldering combustion.

7. data/balance folder stores balance data for each plant that is used to calculate maximum biomass loss rate; data/hobo folder stores hobo data that is used to summarize flame temperature for each burning trial such as temperature integration above 100 celsius degree and heating duration when temp is above 100 Celsius degree.

8. data/t-h folder stores ambient temperature and relative humidity that were measured every 5 minutes on the burning date.
