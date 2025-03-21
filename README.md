### Introduction
This Shiny app allows users to explore a global dataset of intercropping experiments through interactive visualizations. The app is divided into several widgets (tabs), each designed to examine a different aspect of intercropping performance.

### Data
Data from a systematic literature search using terms and applied to Web of Science. Results in a dataset from field experiements published worldwide from 1982 and 2022. Included are (i) general information on the experiments; (ii) experimental site soil and climate conditions; 
(iii) descriptions of intercropping designs; (iv) crop management practices; (v) measurements of sole crop and interscop yields and (v) Land Equivalent Ratios.

*Source:* Paut, R., Garreau, L., Ollivier, G. et al. A global dataset of experimental intercropping and agroforestry studies in horticulture. Sci Data 11, 5 (2024). https://doi.org/10.1038/s41597-023-02831-7
Data repository: https://entrepot.recherche.data.gouv.fr/dataset.xhtml?persistentId=doi:10.57745/HV33V1

---

## Widget 1: Intercropping by Continent

**Purpose:**  
Explore the cumulative number of intercropping experiments conducted across continents over time.

**Display:**  
- A line plot showing cumulative experiments by continent (`ggplot2::geom_line`)  
- A checkbox group (`checkboxGroupInput`) to select continents  
- A year filter slider (`sliderInput`)  

**Interactivity:**  
Users can select one or more continents and adjust the year range. The plot updates to reflect only the selected filters.

---

## Widget 2: Map of Experiments

**Purpose:**  
Visualize the global distribution of intercropping experiments and explore country-level summaries.

**Display:**  
- An interactive world map (`plotly::ggplotly`) showing experiment counts by country  
- A dynamic side panel that appears upon clicking a country, displaying:
  - Country name  
  - Total number of experiments  
  - Most common crop  
  - Most common intercropping pattern  

**Interactivity:**  
Clicking on a country reveals a summary panel with key information. The map’s fill color represents the number of experiments conducted in each country.

---

## Widget 3: LER by Crop Types

**Purpose:**  
Compare intercropping performance (LER) between selected crop pairs and examine experiment frequency over time.

**Display:**  
- A scatter plot of LER values for selected crop pairs  
- A dropdown (`selectInput`) for choosing crop 1  
- A dynamically updated dropdown for crop 2  
- A line chart of cumulative experiments over time for crop 1 + crop 2 combinations  

**Interactivity:**  
Selecting different crops updates both the LER scatter plot and the cumulative experiment chart. Points in the scatter plot are colored by country and shaped by intercropping design.

---

## Widget 4: Principal Component Analysis (PCA)

**Purpose:**  
Use PCA to examine multidimensional relationships between intercropping variables.

**Display:**  
- A PCA biplot (`ggfortify::autoplot`) with loadings and data points colored by continent  
- A variance bar chart for the top 6 principal components  
- Two dropdowns to choose PCs for the x- and y-axes  

**Interactivity:**  
Users select principal components to update the biplot. Arrows represent variable loadings; their direction and length show contribution and correlation. The bar chart summarizes how much variance is explained by each PC.

---