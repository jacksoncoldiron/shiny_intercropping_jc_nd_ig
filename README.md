# intercropping_shinyapp
### Introduction
The Intercropping Shiny App is designed to explore the impact of climate, soil conditions, fertilizer use, and intercropping design on agricultural yield. Using an interactive interface, users can visualize and compare different yield metrics (calories, protein, USD, and LER), analyze how environmental and management factors influence intercropping success, and gain insights from advanced statistical modeling. The app serves as a tool for researchers, farmers, and policymakers to make data-driven decisions about sustainable intercropping practices. 

### Data
Data from a systematic literature search using terms and applied to Web of Science. Results in a dataset from field experiements published worldwide from 1982 and 2022. Included are (i) general information on the experiments; (ii) experimental site soil and climate conditions; 
(iii) descriptions of intercropping designs; (iv) crop management practices; (v) measurements of sole crop and interscop yields and (v) Land Equivalent Ratios.

*Source:* Paut, R., Garreau, L., Ollivier, G. et al. A global dataset of experimental intercropping and agroforestry studies in horticulture. Sci Data 11, 5 (2024). https://doi.org/10.1038/s41597-023-02831-7
Data repository: https://entrepot.recherche.data.gouv.fr/dataset.xhtml?persistentId=doi:10.57745/HV33V1

### Description of Widgets

#### Widget 1: Yield Metric Selection
**Purpose:** Allows users to choose which yield metric to display.

**Display:** A dropdown menu (selectInput) where users can select between:
- Calories
- Protein
- USD value
- Land Equivalent Ratio (LER) (given in the dataset)

**Interactivity:** The selected metric will update the primary yield visualization, allowing users to compare different measures of yield across intercropping setups.


#### Widget 2: Climate & Soil Conditions
**Purpose:** Helps users explore the relationship between intercropping performance and environmental factors.

**Display:**
- A bar graph (ggplot2::geom_bar) displaying LER across different climate conditions.
- A secondary dropdown (selectInput) or radio buttons (radioButtons) for selecting soil conditions (pH, texture, etc.).

**Interactivity:** Users select a soil variable, and the bar graph updates to compare LER performance across different climate types under the chosen soil condition.


#### Widget 3: Fertilizer Variables
**Purpose:** Examines the impact of different fertilizer levels and types on intercropping performance.

**Display:**
- A scatter plot (ggplot2::geom_point) with fertilizer levels on the x-axis and LER (or another yield metric) on the y-axis.
- A slider (sliderInput) to filter data by fertilizer application rates.

**Interactivity:** Users can adjust the slider to focus on specific fertilizer levels and see how they influence intercropping performance.


#### Widget 4: Intercropping Design Variables
**Purpose:** Investigates how different intercropping designs affect productivity.

**Display:**
- A faceted plot (ggplot2::facet_wrap()) showing different intercropping designs and their corresponding yield metrics.
- A dropdown menu (selectInput) to choose between different cropping patterns (e.g., strip cropping, relay cropping, mixed cropping).

**Interactivity:** Selecting an intercropping pattern updates the visualization to highlight its yield and efficiency.


