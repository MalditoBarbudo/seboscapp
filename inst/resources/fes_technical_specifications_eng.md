---
title: "Technical specifications"
author: "Víctor Granda"
date: "25/01/2021"
output: html_document
---

### Introduction

**Fes App** provides information about the forest ecosystem services in Catalonia. Data is offered in two different time scales: static and dynamic data.  
Static data is meant for ecosystem services from different sources and models calculated for a unique time point (that can or not be the same for the different services provided). Dynamic data ara services calculated from data and/or plots from Spanish National Forest Inventory (NFI) at each of its versions (NFI2, NFI3 and NFI4), as well the differences between them.  

Both time scales can be aggregated at different spatial scales:

  1. Local: One km^2 plots. In the case of the dynamic data, those plots correspond to NFI plots
  
  2. Municipality, County and Province: Official administrative units
  
  3. Natural interes areas, Special protection areas and Natura network 2000: Official interest areas

### Data sources

  + NFI - Spanish National Inventory, see **NFI App**  
  + MCSC - Catalonian soil cover map, https://www.creaf.uab.es/mcsc/  
  + ICO - Catalonian Ornitology Institute
  + Research:
      - [De Miguel et al. 2014](https://www.sciencedirect.com/science/article/pii/S0378112714004344?casa_token=RDjPTPcg-NsAAAAA:T9eqVs0L4naDoqeSjy8j9ok7UaP-B_liFtiMPzjijm1J1d53FjKNmCyhoraJssnKHtMBKHX4Dg)  
      - [De Cáceres et al. 2015](https://www.sciencedirect.com/science/article/pii/S0168192315001914?casa_token=rz3e4BmHb5YAAAAA:M2niVZK1Tz9IxF_N2pgYeBAl7zVN1omwBYQ37xM49-K0_4qvJGQUa3vrkmNPTEiHrwyk787_3w)
      - [Doblas-Miranda et al. 2013](https://bg.copernicus.org/articles/10/8353/2013/)

### Methodology

Original data comes from NFI plots (1km) and from rasters with a variable pixel size, depending on the data source. Data source with pixels below 1km are aggregated to uniformize variables to the same resolution.  
Spatial aggregation is done with all the 1km points or pixels inside the administrative unit to scale. If there are less than 3 points/pixels to aggregate, no aggregation is done.  

For more info, see [FORESCALE report](http://laboratoriforestal.creaf.uab.cat/informe_forescale.pdf), [Roces-Díaz et al. 2018a](https://www.sciencedirect.com/science/article/pii/S1470160X18304175?casa_token=4rxqNxzGDJYAAAAA:HnVQ7g-186UWhGcdPjXQpTGdtvCSTh7O3zVkjxN-LMrDyS2hg0Rw-SY_6IxacTz-idcVdxBM2g), [Roces-Díaz et al. 2018b](https://www.sciencedirect.com/science/article/pii/S004896971830175X?casa_token=J0dHQ9Th9LAAAAAA:UU_-8vGzzSbyvfb-Wsdna9oeQ5FfdrsT59ltN74HHg85mt99V_OH4QtYOdKIbV5ngNFbBHmMog) and [Roces-Díaz et al. 2021](https://www.sciencedirect.com/science/article/pii/S037811272031392X?casa_token=Tpbo2-ySl4MAAAAA:2v9T9-hv8E3OQakAAFk7HAoGeE5z2ZWQsSIaMGZxd94Mas9Ro2DT5jE-TD-W1zGDGuElBd-yOA)

### Output variables

#### Static data

Variable | Definition | Units |
:--------|:----------:|---------:|
Fauna survey | Animal species observations uploaded to Ornitho.cat (Catalonian Ornithology Institute - ICO) | Count |
Mushrooms production | Estimated production of edible mushrooms (De Miguel et al. 2014) | kg/ha/year |
Exported water | Water exported due to runoff or percolation (De Cáceres et al. 2015) | l/m2/year |
Carbon sequestration | Forest carbon sink capacacity (NFI data, Vayreda et al. 2016) | t/ha/year |
Soil organic carbon | Forest soil organic carbon stock (Doblas-Miranda et al. 2013) | t/ha |
Riparian forest cover | Riparian forest cover in a 25m buffer around rivers (Catalonia Soil Cover Map - MCSC) | % |
Slope forest cover | Forest cover in surfaces with >30% slopes (Catalonia Soil Cover Map - MCSC) | % |

#### Dynamic data

Variable | Definition | Units |
:--------|:----------:|---------:|
Erosion mitigation | Amount of soil erosion avoided by vegetation (tree and shrub canopies) calculate as the difference between maximum erosion potential (no vegetation cover) and erosion rate estimated with existing cover | t/ha/year |
Mushrooms production | Estimated production of edible mushrooms (De Miguel et al. 2014) | kg/ha/year |
Exported water | Annual average of the relative amount of water exported from each plot as the ratio of run-off (blue water) and annual precipitation (De Cáceres et al. 2015) | blue water/precipitation ratio |
Carbon sequestration | Forest carbon sink capacacity (NFI data, Vayreda et al. 2016) | t/ha/year |
Timber production |Annual timber stock rate per plot as the difference on stock (including forest growth) between NFI surveys | m3/ha/year |

### References

+ Sergio de-Miguel, José Antonio Bonet, Timo Pukkala & Juan Martínez de Aragón. (2014). Impact of forest management intensity on landscape-level mushroom productivity: A regional model-based scenario analysis, Forest Ecology and Management, 330, 218-227.  
+ Miquel De Cáceres, Jordi Martínez-Vilalta, Lluís Coll, Pilar Llorens, Pere Casals, Rafael Poyatos, Juli G. Pausas, Lluís Brotons. (2015). Coupling a water balance model with forest inventory data to predict drought stress: the role of forest structural changes vs. climate changes, Agricultural and Forest Meteorology, 213, 77-90.  
+ Doblas-Miranda, E. and Rovira, P. and Brotons, L. and Mart\'{\i}nez-Vilalta, J. and Retana, J. and Pla, M. and Vayreda, J. (2013). Soil carbon stocks and their variability across the forests, shrublands and grasslands of peninsular Spain, Biogeosciences, 10, 8353--8361.  
+ Roces-Díaz, J. V., Vayreda, J., Banqué-Casanovas, M., Díaz-Varela, E., Bonet, J. A., Brotons, L., ... & Martínez-Vilalta, J. (2018). The spatial level of analysis affects the patterns of forest ecosystem services supply and their relationships. Science of The Total Environment, 626, 1270-1283.  
+ Roces-Díaz, J. V., Vayreda, J., Banqué-Casanovas, M., Cusó, M., Anton, M., Bonet, J. A., ... & Martínez-Vilalta, J. (2018). Assessing the distribution of forest ecosystem services in a highly populated Mediterranean region. Ecological indicators, 93, 986-997  
+ Roces-Díaz, J. V., Vayreda, J., De Cáceres, M., García-Valdés, R., Banqué-Casanovas, M., Morán-Ordóñez, A., ... & Martínez-Vilalta, J. (2021). Temporal changes in Mediterranean forest ecosystem services are driven by stand development, rather than by climate-related disturbances. Forest Ecology and Management, 480, 118623  
