---
title: "Especificaciones técnicas"
author: "Víctor Granda"
date: "22/01/2021"
output: html_document
---

### Introducción

La aplicación **FES App** proporciona información sobre los servicios ecosistémicos forestales en Cataluña. Los datos se ofrecen en dos escalas temporales: datos estáticos y datos dinámicos.  
Los datos estáticos se refieren a servicios ecosistémicos procedentes de diferentes fuentes y modelos calculados para un único punto en el tiempo (que puede no coincidir entre servicios). Los datos dinámicos son servicios calculados a partir de los datos y parcelas del Inventario Forestal Nacional (IFN) en cada una de sus versiones (IFN2, IFN3 e IFN4), así como las diferencias entre ellos.

Las dos escalas temporales pueden ser agregados en diferentes escalas espaciales:

  1. Local: Parcelas de un 1 km^2. En el caso de datos dinámicos, éstas corresponden a las parcelas del IFN
  
  2. Municipio, Comarca y Provincia: Unidades administrativas oficiales
  
  3. Áreas de interes natural, Áreas de protección especial y red Natura 2000: Aŕeas de interés oficiales


### Fuentes de datos

  + IFN - Inventario Forestal Nacional, ver la aplicación **IFN App**  
  + MCSC - Mapa de coberturas de suelo de Cataluña, https://www.creaf.uab.es/mcsc/  
  + ICO - Instituto Catalán de Ornitología
  + Investigación:
      - [De Miguel et al. 2014](https://www.sciencedirect.com/science/article/pii/S0378112714004344?casa_token=RDjPTPcg-NsAAAAA:T9eqVs0L4naDoqeSjy8j9ok7UaP-B_liFtiMPzjijm1J1d53FjKNmCyhoraJssnKHtMBKHX4Dg)  
      - [De Cáceres et al. 2015](https://www.sciencedirect.com/science/article/pii/S0168192315001914?casa_token=rz3e4BmHb5YAAAAA:M2niVZK1Tz9IxF_N2pgYeBAl7zVN1omwBYQ37xM49-K0_4qvJGQUa3vrkmNPTEiHrwyk787_3w)
      - [Doblas-Miranda et al. 2013](https://bg.copernicus.org/articles/10/8353/2013/)

### Metodología

Los datos originales provienen de las parcelas del IFN (1km) o de rásters con un tamaño de pixel variable según la fuente de datos. Todas aquellas mallas menores de 1km se agregan a 1km para tener todas las variables en la misma resolución.  
El agregado por escala espacial se realiza con aquellos puntos locales que entran dentro de la unidad administrativa a escalar (si hay menos de 3 puntos, no se realiza la agregación).

Para mas información, se puede consultar [el informe FORESCALE](http://laboratoriforestal.creaf.cat/informe_forescale.pdf), [Roces-Díaz et al. 2018a](https://www.sciencedirect.com/science/article/pii/S1470160X18304175?casa_token=4rxqNxzGDJYAAAAA:HnVQ7g-186UWhGcdPjXQpTGdtvCSTh7O3zVkjxN-LMrDyS2hg0Rw-SY_6IxacTz-idcVdxBM2g), [Roces-Díaz et al. 2018b](https://www.sciencedirect.com/science/article/pii/S004896971830175X?casa_token=J0dHQ9Th9LAAAAAA:UU_-8vGzzSbyvfb-Wsdna9oeQ5FfdrsT59ltN74HHg85mt99V_OH4QtYOdKIbV5ngNFbBHmMog) y [Roces-Díaz et al. 2021](https://www.sciencedirect.com/science/article/pii/S037811272031392X?casa_token=Tpbo2-ySl4MAAAAA:2v9T9-hv8E3OQakAAFk7HAoGeE5z2ZWQsSIaMGZxd94Mas9Ro2DT5jE-TD-W1zGDGuElBd-yOA)

### Variables de salida

#### Datos estáticos

Variable | Definicion | Unidades |
:--------|:----------:|---------:|
Observación de fauna | Observaciones de especies animales introducidas en Ornitho.cat (Instituto Catalán de Ornitología - ICO) | count |
Producción de setas | Producción esperada de setas comestibles (De Miguel et al. 2014) | kg/ha/year |
Agua exportada | Agua exportada por escorrentía o percolación (De Cáceres et al. 2015) | l/m2/year |
Carbono secuestrado | Capacidad de sumidero de carbono de los bosques (Datos IFN, Vayreda et al. 2016) | t/ha/year |
Carbono orgánico del suelo | Stock de carbono organico en el suelo de los bosques (Doblas-Miranda et al. 2013) | t/ha |
Cobertura de bosques de ribera | Cobertura de bosque de riber en un buffer de 25m alrededor de los ríos (Mapa de Cubiertas de Suelo de Cataluña - MCSC) | % |
Cobertura de bosques en pendiente | Cobertura de bosque en superficies con pendientes >30% (Mapa de Cubiertas de Suelo de Cataluña - MCSC) | % |

#### Datos dinámicos

Variable | Definicion | Unidades |
:--------|:----------:|---------:|
Erosión del suelo mitigada | Cantidad de erosión del suelo evitada por la vegetación (cubiertas forestales y arbustivas) calculada como la diferencia entre el potencial máximo de erosión (sin covertura vegetal) y la tasa de erosión estimada con la cobertura existente | t/ha/year |
Producción de setas | Producción esperada de setas comestibles (De Miguel et al. 2014) | kg/ha/year |
Agua exportada | Promedio anual de la cantidad relativa de agua exportada de cada parcela calculado como el ratio de escorrentía (agua azul) y precipitación anual (De Cáceres et al. 2015) | blue water/precipitation ratio |
Carbono secuestrado | Tasa anual de secuestro de carbono por parcela (parte aérea y parte subterranea), calculada como la diferencia en el almacenamiento de carbono en los árboles entre versiones del IFN | kg/ha/year |
Producción de madera | Tasa anual del stock de madera por parcela calculada como la diferencia en el stock (incluido el crecimiento forestal) entre versiones del IFN | m3/ha/year |

### Bibliografía

+ Sergio de-Miguel, José Antonio Bonet, Timo Pukkala & Juan Martínez de Aragón. (2014). Impact of forest management intensity on landscape-level mushroom productivity: A regional model-based scenario analysis, Forest Ecology and Management, 330, 218-227.  
+ Miquel De Cáceres, Jordi Martínez-Vilalta, Lluís Coll, Pilar Llorens, Pere Casals, Rafael Poyatos, Juli G. Pausas, Lluís Brotons. (2015). Coupling a water balance model with forest inventory data to predict drought stress: the role of forest structural changes vs. climate changes, Agricultural and Forest Meteorology, 213, 77-90.  
+ Doblas-Miranda, E. and Rovira, P. and Brotons, L. and Mart\'{\i}nez-Vilalta, J. and Retana, J. and Pla, M. and Vayreda, J. (2013). Soil carbon stocks and their variability across the forests, shrublands and grasslands of peninsular Spain, Biogeosciences, 10, 8353--8361.  
+ Roces-Díaz, J. V., Vayreda, J., Banqué-Casanovas, M., Díaz-Varela, E., Bonet, J. A., Brotons, L., ... & Martínez-Vilalta, J. (2018). The spatial level of analysis affects the patterns of forest ecosystem services supply and their relationships. Science of The Total Environment, 626, 1270-1283.  
+ Roces-Díaz, J. V., Vayreda, J., Banqué-Casanovas, M., Cusó, M., Anton, M., Bonet, J. A., ... & Martínez-Vilalta, J. (2018). Assessing the distribution of forest ecosystem services in a highly populated Mediterranean region. Ecological indicators, 93, 986-997  
+ Roces-Díaz, J. V., Vayreda, J., De Cáceres, M., García-Valdés, R., Banqué-Casanovas, M., Morán-Ordóñez, A., ... & Martínez-Vilalta, J. (2021). Temporal changes in Mediterranean forest ecosystem services are driven by stand development, rather than by climate-related disturbances. Forest Ecology and Management, 480, 118623  
