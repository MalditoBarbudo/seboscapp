---
title: "Especificacions tècniques"
author: "Víctor Granda"
date: "25/01/2021"
output: html_document
---

### Introducció

L'aplicació **FES App** proporciona informació sobre els serveis ecosistèmics forestals a Catalunya. Les dades s'ofereixen en dues escales temporals: dades estàtiques i dades dinàmiques.
Les dades estàtics es refereixen a serveis ecosistèmics procedents de diferents fonts i models calculats per a un únic punt en el temps (que pot no coincidir entre serveis). Les dades dinàmics són serveis calculats a partir de les dades i parcel·les de l'Inventari Forestal Nacional (IFN) en cadascuna de les seves versions (IFN2, IFN3 i IFN4), així com les diferències entre ells.

Les dues escales temporals poden ser agregats en diferents escales espacials:

  1. Local: Parcel·les d'un 1 km^2. En el cas de dades dinàmiques, aquestes corresponen a les parcel·les de l'IFN

  2. Municipi, Comarca i Província: Unitats administratives oficials

  3. Àrees d'interès natural, Àrees de protecció especial i xarxa Natura 2000: àrees d'interès oficials


### Fonts de dades

  + IFN - Inventari Forestal Nacional, veure l'aplicació **IFN App**
  + MCSC - Mapa de cobertures de sòl de Catalunya, https://www.creaf.uab.es/mcsc/
  + ICO - Institut Català d'Ornitologia
  + Recerca:
      - [De Miguel et al. 2014](https://www.sciencedirect.com/science/article/pii/S0378112714004344?casa_token=RDjPTPcg-NsAAAAA:T9eqVs0L4naDoqeSjy8j9ok7UaP-B_liFtiMPzjijm1J1d53FjKNmCyhoraJssnKHtMBKHX4Dg)  
      - [De Cáceres et al. 2015](https://www.sciencedirect.com/science/article/pii/S0168192315001914?casa_token=rz3e4BmHb5YAAAAA:M2niVZK1Tz9IxF_N2pgYeBAl7zVN1omwBYQ37xM49-K0_4qvJGQUa3vrkmNPTEiHrwyk787_3w)
      - [Doblas-Miranda et al. 2013](https://bg.copernicus.org/articles/10/8353/2013/)

### Metodologia

Les dades originals provenen de les parcel·les de l'IFN (1 km) o de ràsters amb una mida de píxel variable segons la font de dades. Totes aquelles malles menors de 1 km s'agreguen a 1 km per tenir totes les variables en la mateixa resolució.
L'agregat per escala espacial es realitza amb aquells punts locals que entren dins de la unitat administrativa a escalar (si hi ha menys de 3 punts, no es realitza l'agregació).

Per a més informació, es pot consultar [l'informe FORESCALE](http://laboratoriforestal.creaf.cat/informe_forescale.pdf), [Roces-Díaz et al. 2018a](https://www.sciencedirect.com/science/article/pii/S1470160X18304175?casa_token=4rxqNxzGDJYAAAAA:HnVQ7g-186UWhGcdPjXQpTGdtvCSTh7O3zVkjxN-LMrDyS2hg0Rw-SY_6IxacTz-idcVdxBM2g), [Roces-Díaz et al. 2018b](https://www.sciencedirect.com/science/article/pii/S004896971830175X?casa_token=J0dHQ9Th9LAAAAAA:UU_-8vGzzSbyvfb-Wsdna9oeQ5FfdrsT59ltN74HHg85mt99V_OH4QtYOdKIbV5ngNFbBHmMog) i [Roces-Díaz et al. 2021](https://www.sciencedirect.com/science/article/pii/S037811272031392X?casa_token=Tpbo2-ySl4MAAAAA:2v9T9-hv8E3OQakAAFk7HAoGeE5z2ZWQsSIaMGZxd94Mas9Ro2DT5jE-TD-W1zGDGuElBd-yOA)

### Variables de sortida

#### Dades estàtics

Variable | Definició | Unitats |
:--------|:----------:|---------:|
Observació de fauna | Observacions d'espècies animals introduïdes a l'Ornothi.cat (Institut Català d'Ornitologia - ICO) | count |
Producció de bolets | Producció de esperable de bolets comestibles (De Miguel et al. 2014) | kg/ha/year |
Aigua exportada | Aigua exportada per escorrentiu o percolació (De Cáceres et al. 2015) | l/m2/year |
Carboni segrestat | Capacitat d'embornal de carboni dels boscos (Dades IFN, Vayreda et al. 2016) | t/ha/year |
Carboni orgànic del sòl | Estoc de carboni orgànic al sòl dels boscos (Doblas-Miranda et al. 2013) | t/ha |
Cobertura de boscos de ribera | Coberta de bosc de ribera en buffer de 25m dels rius (Mapa de Cobertes de Sòl de Catalunya - MCSC) | % |
Cobertura de boscos en pendent | Coberta forestal en superficies amb pendents > 30% (Mapa de Cobertes de Sòl de Catalunya - MCSC) | % |

#### Dades dinamics

Variable | Definició | Unitats |
:--------|:----------:|---------:|
Erosió mitigada | Quantitat d'erosió de terra evitada per la vegetació (cobertes forestals i arbustives) calculada com la diferència entre el potencial màxim d'erosió (sense covertura vegetal) i la taxa d'erosió estimada amb la cobertura existent | t/ha/year |
Producció de bolets | Producció de esperable de bolets comestibles (De Miguel et al. 2014) | kg/ha/year |
Aigua exportada | Mitjana anual de la quantitat relativa d'aigua exportada de cada parcel·la calculat com la ràtio de escorrentia (aigua blava) i precipitació anual (De Càceres et al. 2015) | blue water/precipitation ratio |
Carboni segrestat | Taxa anual de segrest de carboni per parcel·la (part aèria i part subterrània), calculada com la diferència en l'emmagatzematge de carboni en els arbres entre versions de l'IFN | kg/ha/year |
Producció de fusta | Taxa anual de l'estoc de fusta per parcel·la calculat com la diferència en l'estoc (inclòs el creixement forestal) entre versions de l'IFN | m3/ha/year |

### Bibliografia

+ Sergio de-Miguel, José Antonio Bonet, Timo Pukkala & Juan Martínez de Aragón. (2014). Impact of forest management intensity on landscape-level mushroom productivity: A regional model-based scenario analysis, Forest Ecology and Management, 330, 218-227.  
+ Miquel De Cáceres, Jordi Martínez-Vilalta, Lluís Coll, Pilar Llorens, Pere Casals, Rafael Poyatos, Juli G. Pausas, Lluís Brotons. (2015). Coupling a water balance model with forest inventory data to predict drought stress: the role of forest structural changes vs. climate changes, Agricultural and Forest Meteorology, 213, 77-90.  
+ Doblas-Miranda, E. and Rovira, P. and Brotons, L. and Mart\'{\i}nez-Vilalta, J. and Retana, J. and Pla, M. and Vayreda, J. (2013). Soil carbon stocks and their variability across the forests, shrublands and grasslands of peninsular Spain, Biogeosciences, 10, 8353--8361.  
+ Roces-Díaz, J. V., Vayreda, J., Banqué-Casanovas, M., Díaz-Varela, E., Bonet, J. A., Brotons, L., ... & Martínez-Vilalta, J. (2018). The spatial level of analysis affects the patterns of forest ecosystem services supply and their relationships. Science of The Total Environment, 626, 1270-1283.  
+ Roces-Díaz, J. V., Vayreda, J., Banqué-Casanovas, M., Cusó, M., Anton, M., Bonet, J. A., ... & Martínez-Vilalta, J. (2018). Assessing the distribution of forest ecosystem services in a highly populated Mediterranean region. Ecological indicators, 93, 986-997  
+ Roces-Díaz, J. V., Vayreda, J., De Cáceres, M., García-Valdés, R., Banqué-Casanovas, M., Morán-Ordóñez, A., ... & Martínez-Vilalta, J. (2021). Temporal changes in Mediterranean forest ecosystem services are driven by stand development, rather than by climate-related disturbances. Forest Ecology and Management, 480, 118623  
