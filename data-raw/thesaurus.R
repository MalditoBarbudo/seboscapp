library(dplyr)
library(tibble)
# variable thesaurus
variable_thesaurus <- tibble::tribble(
  # headers
  ~var_id, ~var_table, ~var_units,
  ~translation_eng, ~translation_spa, ~translation_cat,
  ~var_description_eng, ~var_description_spa, ~var_description_cat,
  # static table
  "plot_id", "static", "-",
  "Plot ID", "ID de parcela", "ID de parcel·la",
  "Unique identifier for plot", "Identificador único para cada parcela", "Identificador únic per a cada parcel·la",

  "animals_presence", "static", "count",
  "Fauna survey", "Observación de fauna", "Observació de fauna",
  "Animal species observations uploaded to Ornitho.cat (Catalonian Ornithology Institute - ICO)", "Observaciones de especies animales introducidas en Ornitho.cat (Instituto Catalán de Ornitología - ICO)", "Observacions d'espècies animals introduïdes a l'Ornothi.cat (Institut Català d'Ornitologia - ICO)",

  "mushrooms_production", "static", "kg/ha/year",
  "Mushroom production", "Producción de setas", "Producció de bolets",
  "Estimated production of edible mushrooms (De Miguel et al. 2014)", "Producción esperada de setas comestibles (De Miguel et al. 2014)", "Producció de esperable de bolets comestibles (De Miguel et al. 2014)",

  "exported_water", "static", "l/m2/year",
  "Exported water", "Agua exportada", "Aigua exportada",
  "Water exported due to runoff or percolation (De Cáceres et al. 2015)", "Agua exportada por escorrentía o percolación (De Cáceres et al. 2015)", "Aigua exportada per escorrentiu o percolació (De Cáceres et al. 2015)",

  "carbon_sequestration", "static", "t/ha/year",
  "Carbon sequestration", "Carbono secuestrado", "Carboni segrestat",
  "Forest carbon sink capacacity (NFI data, Vayreda et al. 2016)", "Capacidad de sumidero de carbono de los bosques (Datos IFN, Vayreda et al. 2016)", "Capacitat d'embornal de carboni dels boscos (Dades IFN, Vayreda et al. 2016)",

  "soil_organic_carbon", "static", "t/ha",
  "Soil organic carbon", "Carbono orgánico del suelo", "Carboni orgànic del sòl",
  "Forest soil organic carbon stock (Doblas-Miranda et al. 2013)", "Stock de carbono organico en el suelo de los bosques (Doblas-Miranda et al. 2013)", "Estoc de carboni orgànic al sòl dels boscos (Doblas-Miranda et al. 2013)",

  "riparian_forest_cover", "static", "%",
  "Riparian forest cover", "Cobertura de bosques de ribera", "Cobertura de boscos de ribera",
  "Riparian forest cover in a 25m buffer around rivers (Catalonia Soil Cover Map - MCSC)", "Cobertura de bosque de riber en un buffer de 25m alrededor de los ríos (Mapa de Cubiertas de Suelo de Cataluña - MCSC)", "Coberta de bosc de ribera en buffer de 25m dels rius (Mapa de Cobertes de Sòl de Catalunya - MCSC)",

  "slope_forest_cover", "static", "%",
  "Slope forest cover", "Cobertura de bosques en pendiente", "Cobertura de boscos en pendent",
  "Forest cover in surfaces with >30% slopes (Catalonia Soil Cover Map - MCSC)", "Cobertura de bosque en superficies con pendientes >30% (Mapa de Cubiertas de Suelo de Cataluña - MCSC)", "Coberta forestal en superficies amb pendents > 30% (Mapa de Cobertes de Sòl de Catalunya - MCSC)",

  "admin_municipality", "static", "-",
  "Municipality", "Municipio", "Municipi",
  "Municipality", "Municipio", "Municipi",

  "admin_region", "static", "-",
  "County", "Comarca", "Comarca",
  "County", "Comarca", "Comarca",

  "admin_province", "static", "-",
  "Province", "Provincia", "Provincia",
  "Province", "Provincia", "Provincia",

  "admin_natural_interest_area", "static", "-",
  "Natural interest areas", "Áreas de interés natural","Àrees d'interès natural",
  "Natural interest areas", "Áreas de interés natural","Àrees d'interès natural",

  "admin_special_protection_natural_area", "static", "-",
  "Special protection natural areas", "Áreas naturales de protección especial","Àrees naturals de protecció especial",
  "Special protection natural areas", "Áreas naturales de protección especial","Àrees naturals de protecció especial",

  "admin_natura_network_2000", "static", "-",
  "Natura 2000 network", "Red Natura 2000","Xarxa Natura 2000",
  "Natura 2000 network", "Red Natura 2000","Xarxa Natura 2000",
  # dynamic tables
  # ifn 2
  "plot_id", "plot_nfi_2_results", "-",
  "Plot ID", "ID de parcela", "ID de parcel·la",
  "Unique identifier for plot", "Identificador único para cada parcela", "Identificador únic per a cada parcel·la",

  "erosion_mitigation", "plot_nfi_2_results", "t/ha/year",
  "Erosion mitigation", "Erosión mitigada", "Erosió mitigada",
  "Amount of soil erosion avoided by vegetation (tree and shrub canopies) calculate as the difference between maximum erosion potential (no vegetation cover) and erosion rate estimated with existing cover for NFI2 (1988-92)",
  "Cantidad de erosión del suelo evitada por la vegetación (cubiertas forestales y arbustivas) calculada como la diferencia entre el potencial máximo de erosión (sin covertura vegetal) y la tasa de erosión estimada con la cobertura existente para el IFN2 (1988-92)",
  "Quantitat d'erosió de terra evitada per la vegetació (cobertes forestals i arbustives) calculada com la diferència entre el potencial màxim d'erosió (sense covertura vegetal) i la taxa d'erosió estimada amb la cobertura existent per al IFN2 (1988-92)",

  "mushrooms_production", "plot_nfi_2_results", "kg/ha/year",
  "Mushroom production", "Producción de setas", "Producció de bolets",
  "1988-92 estimated production of edible mushrooms (De Miguel et al. 2014) based on data from NFI plots", "Producción esperada de setas comestibles (De Miguel et al. 2014) para el periodo 1988-92 basada en las parcelas del IFN", "Producció de esperable de bolets comestibles (De Miguel et al. 2014) per al període 1988-92 basada en les parcel·les de l'IFN",

  "exported_water", "plot_nfi_2_results", "blue water/precipitation ratio",
  "Exported water", "Agua exportada", "Aigua exportada",
  "Annual average of the relative amount of water exported from each plot as the ratio of run-off (blue water) and annual precipitation (De Cáceres et al. 2015) for NFI2 (1988-92)",
  "Promedio anual de la cantidad relativa de agua exportada de cada parcela calculado como el ratio de escorrentía (agua azul) y precipitación anual (De Cáceres et al. 2015) para el IFN2 (1988-92)",
  "Mitjana anual de la quantitat relativa d'aigua exportada de cada parcel·la calculat com la ràtio de escorrentia (aigua blava) i precipitació anual (De Càceres et al. 2015) per l'IFN2 (1998-92)",

  "admin_municipality", "plot_nfi_2_results", "-",
  "Municipality", "Municipio", "Municipi",
  "Municipality", "Municipio", "Municipi",

  "admin_region", "plot_nfi_2_results", "-",
  "County", "Comarca", "Comarca",
  "County", "Comarca", "Comarca",

  "admin_province", "plot_nfi_2_results", "-",
  "Province", "Provincia", "Provincia",
  "Province", "Provincia", "Provincia",

  "admin_natural_interest_area", "plot_nfi_2_results", "-",
  "Natural interest areas", "Áreas de interés natural","Àrees d'interès natural",
  "Natural interest areas", "Áreas de interés natural","Àrees d'interès natural",

  "admin_special_protection_natural_area", "plot_nfi_2_results", "-",
  "Special protection natural areas", "Áreas naturales de protección especial","Àrees naturals de protecció especial",
  "Special protection natural areas", "Áreas naturales de protección especial","Àrees naturals de protecció especial",

  "admin_natura_network_2000", "plot_nfi_2_results", "-",
  "Natura 2000 network", "Red Natura 2000","Xarxa Natura 2000",
  "Natura 2000 network", "Red Natura 2000","Xarxa Natura 2000",

  # ifn 3
  "plot_id", "plot_nfi_3_results", "-",
  "Plot ID", "ID de parcela", "ID de parcel·la",
  "Unique identifier for plot", "Identificador único para cada parcela", "Identificador únic per a cada parcel·la",

  "erosion_mitigation", "plot_nfi_3_results", "t/ha/year",
  "Erosion mitigation", "Erosión mitigada", "Erosió mitigada",
  "Amount of soil erosion avoided by vegetation (tree and shrub canopies) calculate as the difference between maximum erosion potential (no vegetation cover) and erosion rate estimated with existing cover for NFI3 (2000-01)",
  "Cantidad de erosión del suelo evitada por la vegetación (cubiertas forestales y arbustivas) calculada como la diferencia entre el potencial máximo de erosión (sin covertura vegetal) y la tasa de erosión estimada con la cobertura existente para el IFN3 (2000-01)",
  "Quantitat d'erosió de terra evitada per la vegetació (cobertes forestals i arbustives) calculada com la diferència entre el potencial màxim d'erosió (sense covertura vegetal) i la taxa d'erosió estimada amb la cobertura existent per al IFN3 (2000-01)",

  "mushrooms_production", "plot_nfi_3_results", "kg/ha/year",
  "Mushroom production", "Producción de setas", "Producció de bolets",
  "2000-01 estimated production of edible mushrooms (De Miguel et al. 2014) based on data from NFI plots", "Producción esperada de setas comestibles (De Miguel et al. 2014) para el periodo 2000-01 basada en las parcelas del IFN", "Producció de esperable de bolets comestibles (De Miguel et al. 2014) per al període 2000-01 basada en les parcel·les de l'IFN",

  "exported_water", "plot_nfi_3_results", "blue water/precipitation ratio",
  "Exported water", "Agua exportada", "Aigua exportada",
  "Annual average of the relative amount of water exported from each plot as the ratio of run-off (blue water) and annual precipitation (De Cáceres et al. 2015) for NFI3 (2000-01)",
  "Promedio anual de la cantidad relativa de agua exportada de cada parcela calculado como el ratio de escorrentía (agua azul) y precipitación anual (De Cáceres et al. 2015) para el IFN3 (2000-01)",
  "Mitjana anual de la quantitat relativa d'aigua exportada de cada parcel·la calculat com la ràtio de escorrentia (aigua blava) i precipitació anual (De Càceres et al. 2015) per l'IFN3 (2000-01)",

  "carbon_sequestration", "plot_nfi_3_results", "t/ha/year",
  "Carbon sequestration", "Carbono secuestrado", "Carboni segrestat",
  "Annual carbon sequestration rate per plot (above and below ground vegetation), calculated as the difference in tree carbon storage between NFI surveys (2 vs 3)",
  "Tasa anual de secuestro de carbono por parcela (parte aérea y parte subterranea), calculada como la diferencia en el almacenamiento de carbono en los árboles entre versiones del IFN (2 vs 3)",
  "Taxa anual de segrest de carboni per parcel·la (part aèria i part subterrània), calculada com la diferència en l'emmagatzematge de carboni en els arbres entre versions de l'IFN (2 vs 3)",

  "wood", "plot_nfi_3_results", "m3/ha/year",
  "Timber production", "Producción de madera", "Producció de fusta",
  "Annual timber stock rate per plot as the difference on stock (including forest growth) between NFI surveys (2 vs 3)",
  "Tasa anual del stock de madera por parcela calculada como la diferencia en el stock (incluido el crecimiento forestal) entre versiones del IFN (2 vs 3)",
  "Taxa anual de l'estoc de fusta per parcel·la calculat com la diferència en l'estoc (inclòs el creixement forestal) entre versions de l'IFN (2 vs 3)",

  "admin_municipality", "plot_nfi_3_results", "-",
  "Municipality", "Municipio", "Municipi",
  "Municipality", "Municipio", "Municipi",

  "admin_region", "plot_nfi_3_results", "-",
  "County", "Comarca", "Comarca",
  "County", "Comarca", "Comarca",

  "admin_province", "plot_nfi_3_results", "-",
  "Province", "Provincia", "Provincia",
  "Province", "Provincia", "Provincia",

  "admin_natural_interest_area", "plot_nfi_3_results", "-",
  "Natural interest areas", "Áreas de interés natural","Àrees d'interès natural",
  "Natural interest areas", "Áreas de interés natural","Àrees d'interès natural",

  "admin_special_protection_natural_area", "plot_nfi_3_results", "-",
  "Special protection natural areas", "Áreas naturales de protección especial","Àrees naturals de protecció especial",
  "Special protection natural areas", "Áreas naturales de protección especial","Àrees naturals de protecció especial",

  "admin_natura_network_2000", "plot_nfi_3_results", "-",
  "Natura 2000 network", "Red Natura 2000","Xarxa Natura 2000",
  "Natura 2000 network", "Red Natura 2000","Xarxa Natura 2000",

  # ifn 4
  "plot_id", "plot_nfi_4_results", "-",
  "Plot ID", "ID de parcela", "ID de parcel·la",
  "Unique identifier for plot", "Identificador único para cada parcela", "Identificador únic per a cada parcel·la",

  "erosion_mitigation", "plot_nfi_4_results", "t/ha/year",
  "Erosion mitigation", "Erosión mitigada", "Erosió mitigada",
  "Amount of soil erosion avoided by vegetation (tree and shrub canopies) calculate as the difference between maximum erosion potential (no vegetation cover) and erosion rate estimated with existing cover for NFI4 (2013-17)",
  "Cantidad de erosión del suelo evitada por la vegetación (cubiertas forestales y arbustivas) calculada como la diferencia entre el potencial máximo de erosión (sin covertura vegetal) y la tasa de erosión estimada con la cobertura existente para el IFN4 (2013-17)",
  "Quantitat d'erosió de terra evitada per la vegetació (cobertes forestals i arbustives) calculada com la diferència entre el potencial màxim d'erosió (sense covertura vegetal) i la taxa d'erosió estimada amb la cobertura existent per al IFN4 (2013-17)",

  "mushrooms_production", "plot_nfi_4_results", "kg/ha/year",
  "Mushroom production", "Producción de setas", "Producció de bolets",
  "2013-17 estimated production of edible mushrooms (De Miguel et al. 2014) based on data from NFI plots", "Producción esperada de setas comestibles (De Miguel et al. 2014) para el periodo 2013-17 basada en las parcelas del IFN", "Producció de esperable de bolets comestibles (De Miguel et al. 2014) per al període 2013-17 basada en les parcel·les de l'IFN",

  "exported_water", "plot_nfi_4_results", "blue water/precipitation ratio",
  "Exported water", "Agua exportada", "Aigua exportada",
  "Annual average of the relative amount of water exported from each plot as the ratio of run-off (blue water) and annual precipitation (De Cáceres et al. 2015) for NFI4 (2012-16)",
  "Promedio anual de la cantidad relativa de agua exportada de cada parcela calculado como el ratio de escorrentía (agua azul) y precipitación anual (De Cáceres et al. 2015) para el IFN4 (2012-16)",
  "Mitjana anual de la quantitat relativa d'aigua exportada de cada parcel·la calculat com la ràtio de escorrentia (aigua blava) i precipitació anual (De Càceres et al. 2015) per l'IFN4 (2012-16)",

  "carbon_sequestration", "plot_nfi_4_results", "t/ha/year",
  "Carbon sequestration", "Carbono secuestrado", "Carboni segrestat",
  "Annual carbon sequestration rate per plot (above and below ground vegetation), calculated as the difference in tree carbon storage between NFI surveys (3 vs 4)",
  "Tasa anual de secuestro de carbono por parcela (parte aérea y parte subterranea), calculada como la diferencia en el almacenamiento de carbono en los árboles entre versiones del IFN (3 vs 4)",
  "Taxa anual de segrest de carboni per parcel·la (part aèria i part subterrània), calculada com la diferència en l'emmagatzematge de carboni en els arbres entre versions de l'IFN (3 vs 4)",

  "wood", "plot_nfi_4_results", "m3/ha/year",
  "Timber production", "Producción de madera", "Producció de fusta",
  "Annual timber stock rate per plot as the difference on stock (including forest growth) between NFI surveys (3 vs 4)",
  "Tasa anual del stock de madera por parcela calculada como la diferencia en el stock (incluido el crecimiento forestal) entre versiones del IFN (3 vs 4)",
  "Taxa anual de l'estoc de fusta per parcel·la calculat com la diferència en l'estoc (inclòs el creixement forestal) entre versions de l'IFN (3 vs 4)",

  "admin_municipality", "plot_nfi_4_results", "-",
  "Municipality", "Municipio", "Municipi",
  "Municipality", "Municipio", "Municipi",

  "admin_region", "plot_nfi_4_results", "-",
  "County", "Comarca", "Comarca",
  "County", "Comarca", "Comarca",

  "admin_province", "plot_nfi_4_results", "-",
  "Province", "Provincia", "Provincia",
  "Province", "Provincia", "Provincia",

  "admin_natural_interest_area", "plot_nfi_4_results", "-",
  "Natural interest areas", "Áreas de interés natural","Àrees d'interès natural",
  "Natural interest areas", "Áreas de interés natural","Àrees d'interès natural",

  "admin_special_protection_natural_area", "plot_nfi_4_results", "-",
  "Special protection natural areas", "Áreas naturales de protección especial","Àrees naturals de protecció especial",
  "Special protection natural areas", "Áreas naturales de protección especial","Àrees naturals de protecció especial",

  "admin_natura_network_2000", "plot_nfi_4_results", "-",
  "Natura 2000 network", "Red Natura 2000","Xarxa Natura 2000",
  "Natura 2000 network", "Red Natura 2000","Xarxa Natura 2000",

  # comp 2 3
  "plot_id", "plot_nfi2_nfi3_results", "-",
  "Plot ID", "ID de parcela", "ID de parcel·la",
  "Unique identifier for plot", "Identificador único para cada parcela", "Identificador únic per a cada parcel·la",

  "erosion_mitigation", "plot_nfi2_nfi3_results", "t/ha/year",
  "Erosion mitigation", "Erosión mitigada", "Erosió mitigada",
  "Difference on the amount of soil erosion avoided by vegetation (tree and shrub canopies) between 1988-92 and 2000-01 periods based on data from NFI plots",
  "Diferencia en la cantidad de erosión del suelo evitada por la vegetación (cubiertas forestales y arbustivas) entre los periodos 1988-92 y 2000-01 basada en las parcelas del IFN",
  "Diferència en la quantitat d'erosió de terra evitada per la vegetació (cobertes forestals i arbustives) entre els períodes 1988-92 i 2000-01 basada en les parcel·les de l'IFN",

  "mushrooms_production", "plot_nfi2_nfi3_results", "kg/ha/year",
  "Mushroom production", "Producción de setas", "Producció de bolets",
  "Difference on the estimated production of edible mushrooms (De Miguel et al. 2014) between 1988-92 and 2000-01 periods based on data from NFI plots",
  "Diferencia en la producción esperada de setas comestibles (De Miguel et al. 2014) entre los periodos 1988-92 y 2000-01 basada en las parcelas del IFN",
  "Diferència en la producció esperada de bolets comestibles (De Miguel et al. 2014) entre els períodes 1988-92 i 2000-01 basada en les parcel·les de l'IFN",

  "exported_water", "plot_nfi2_nfi3_results", "blue water/precipitation ratio",
  "Exported water", "Agua exportada", "Aigua exportada",
  "Difference on annual averages of relative amount of water exported from each plot (blue water/annual precipiation ratio, De Cáceres at al. 2015) between 1988-92 and 2000-01 periods",
  "Diferencia de los promedios anuales de la cantidad relativa de agua exportada de cada parcela (ratio agua azul/precipitación anual, De Cáceres et al. 2015) entre los periodos 1988-92 y 2000-01",
  "Diferència de las mitjanas anuales de la quantitat relativa d'aigua exportada de cada parcel·la (ràtio aigua blava/precipitació anual, De Cáceres et al. 2015) entre els períodes 1988-92 i 2000-01",

  "admin_municipality", "plot_nfi2_nfi3_results", "-",
  "Municipality", "Municipio", "Municipi",
  "Municipality", "Municipio", "Municipi",

  "admin_region", "plot_nfi2_nfi3_results", "-",
  "County", "Comarca", "Comarca",
  "County", "Comarca", "Comarca",

  "admin_province", "plot_nfi2_nfi3_results", "-",
  "Province", "Provincia", "Provincia",
  "Province", "Provincia", "Provincia",

  "admin_natural_interest_area", "plot_nfi2_nfi3_results", "-",
  "Natural interest areas", "Áreas de interés natural","Àrees d'interès natural",
  "Natural interest areas", "Áreas de interés natural","Àrees d'interès natural",

  "admin_special_protection_natural_area", "plot_nfi2_nfi3_results", "-",
  "Special protection natural areas", "Áreas naturales de protección especial","Àrees naturals de protecció especial",
  "Special protection natural areas", "Áreas naturales de protección especial","Àrees naturals de protecció especial",

  "admin_natura_network_2000", "plot_nfi2_nfi3_results", "-",
  "Natura 2000 network", "Red Natura 2000","Xarxa Natura 2000",
  "Natura 2000 network", "Red Natura 2000","Xarxa Natura 2000",

  # comp 3 4
  "plot_id", "plot_nfi3_nfi4_results", "-",
  "Plot ID", "ID de parcela", "ID de parcel·la",
  "Unique identifier for plot", "Identificador único para cada parcela", "Identificador únic per a cada parcel·la",

  "erosion_mitigation", "plot_nfi3_nfi4_results", "t/ha/year",
  "Erosion mitigation", "Erosión mitigada", "Erosió mitigada",
  "Difference on the amount of soil erosion avoided by vegetation (tree and shrub canopies) between 2000-01 and 2013-17 periods based on data from NFI plots",
  "Diferencia en la cantidad de erosión del suelo evitada por la vegetación (cubiertas forestales y arbustivas) entre los periodos 2000-01 y 2013-17 basada en las parcelas del IFN",
  "Diferència en la quantitat d'erosió de terra evitada per la vegetació (cobertes forestals i arbustives) entre els períodes 2000-01 i 2013-17 basada en les parcel·les de l'IFN",

  "mushrooms_production", "plot_nfi3_nfi4_results", "kg/ha/year",
  "Mushroom production", "Producción de setas", "Producció de bolets",
  "Difference on the estimated production of edible mushrooms (De Miguel et al. 2014) between 2000-01 and 2013-17 periods based on data from NFI plots",
  "Diferencia en la producción esperada de setas comestibles (De Miguel et al. 2014) entre los periodos 2000-01 y 2013-17 basada en las parcelas del IFN",
  "Diferència en la producció esperada de bolets comestibles (De Miguel et al. 2014) entre els períodes 2000-01 i 2013-17 basada en les parcel·les de l'IFN",

  "exported_water", "plot_nfi3_nfi4_results", "blue water/precipitation ratio",
  "Exported water", "Agua exportada", "Aigua exportada",
  "Difference on annual averages of relative amount of water exported from each plot (blue water/annual precipiation ratio, De Cáceres at al. 2015) between 2000-01 and 2012-16 periods",
  "Diferencia de los promedios anuales de la cantidad relativa de agua exportada de cada parcela (ratio agua azul/precipitación anual, De Cáceres et al. 2015) entre los periodos 2000-01 y 2012-16",
  "Diferència de las mitjanas anuales de la quantitat relativa d'aigua exportada de cada parcel·la (ràtio aigua blava/precipitació anual, De Cáceres et al. 2015) entre els períodes 2000-01 i 2012-16",

  "carbon_sequestration", "plot_nfi3_nfi4_results", "t/ha/year",
  "Carbon sequestration", "Carbono secuestrado", "Carboni segrestat",
  "Differences in the annual carbon sequestration rate per plot between 2000-01 and 2013-17 periods",
  "Diferencias en la tasa anual de secuestro de carbono por parcela entre los períodos 2000-01 y 2013-17",
  "Difèrencia en la taxa anual de segrest de carboni per parcel·la entre els perìodes 2000-01 i 2013-17",

  "wood", "plot_nfi3_nfi4_results", "m3/ha/year",
  "Timber production", "Producción de madera", "Producció de fusta",
  "Differences in the annual timber stock rate per plot between 2000-01 and 2013-17 periods",
  "Diferencia en la tasa anual del stock de madera por parcela entre los los periodos 2000-01 y 2013-17",
  "Difèrencia en la taxa anual de l'estoc de fusta per parcel·la entre els perìodes 2000-01 i 2013-17",

  "admin_municipality", "plot_nfi3_nfi4_results", "-",
  "Municipality", "Municipio", "Municipi",
  "Municipality", "Municipio", "Municipi",

  "admin_region", "plot_nfi3_nfi4_results", "-",
  "County", "Comarca", "Comarca",
  "County", "Comarca", "Comarca",

  "admin_province", "plot_nfi3_nfi4_results", "-",
  "Province", "Provincia", "Provincia",
  "Province", "Provincia", "Provincia",

  "admin_natural_interest_area", "plot_nfi3_nfi4_results", "-",
  "Natural interest areas", "Áreas de interés natural","Àrees d'interès natural",
  "Natural interest areas", "Áreas de interés natural","Àrees d'interès natural",

  "admin_special_protection_natural_area", "plot_nfi3_nfi4_results", "-",
  "Special protection natural areas", "Áreas naturales de protección especial","Àrees naturals de protecció especial",
  "Special protection natural areas", "Áreas naturales de protección especial","Àrees naturals de protecció especial",

  "admin_natura_network_2000", "plot_nfi3_nfi4_results", "-",
  "Natura 2000 network", "Red Natura 2000","Xarxa Natura 2000",
  "Natura 2000 network", "Red Natura 2000","Xarxa Natura 2000",

  # comp 2 4
  "plot_id", "plot_nfi2_nfi4_results", "-",
  "Plot ID", "ID de parcela", "ID de parcel·la",
  "Unique identifier for plot", "Identificador único para cada parcela", "Identificador únic per a cada parcel·la",

  "erosion_mitigation", "plot_nfi2_nfi4_results", "t/ha/year",
  "Erosion mitigation", "Erosión mitigada", "Erosió mitigada",
  "Difference on the amount of soil erosion avoided by vegetation (tree and shrub canopies) between 1988-92 and 2013-17 periods based on data from NFI plots",
  "Diferencia en la cantidad de erosión del suelo evitada por la vegetación (cubiertas forestales y arbustivas) entre los periodos 1988-92 y 2013-17 basada en las parcelas del IFN",
  "Diferència en la quantitat d'erosió de terra evitada per la vegetació (cobertes forestals i arbustives) entre els períodes 1988-92 i 2013-17 basada en les parcel·les de l'IFN",

  "mushrooms_production", "plot_nfi2_nfi4_results", "kg/ha/year",
  "Mushroom production", "Producción de setas", "Producció de bolets",
  "Difference on the estimated production of edible mushrooms (De Miguel et al. 2014) between 1988-92 and 2013-17 periods based on data from NFI plots",
  "Diferencia en la producción esperada de setas comestibles (De Miguel et al. 2014) entre los periodos 1988-92 y 2013-17 basada en las parcelas del IFN",
  "Diferència en la producció esperada de bolets comestibles (De Miguel et al. 2014) entre els períodes 1988-92 i 2013-17 basada en les parcel·les de l'IFN",

  "exported_water", "plot_nfi2_nfi4_results", "blue water/precipitation ratio",
  "Exported water", "Agua exportada", "Aigua exportada",
  "Difference on annual averages of relative amount of water exported from each plot (blue water/annual precipiation ratio, De Cáceres at al. 2015) between 1988-92 and 2012-16 periods",
  "Diferencia de los promedios anuales de la cantidad relativa de agua exportada de cada parcela (ratio agua azul/precipitación anual, De Cáceres et al. 2015) entre los periodos 1988-92 y 2012-16",
  "Diferència de las mitjanas anuales de la quantitat relativa d'aigua exportada de cada parcel·la (ràtio aigua blava/precipitació anual, De Cáceres et al. 2015) entre els períodes 1988-92 i 2012-16",

  "carbon_sequestration", "plot_nfi2_nfi4_results", "t/ha/year",
  "Carbon sequestration", "Carbono secuestrado", "Carboni segrestat",
  "Differences in the annual carbon sequestration rate per plot between 1988-92 and 2013-17 periods",
  "Diferencias en la tasa anual de secuestro de carbono por parcela entre los períodos 1988-92 y 2013-17",
  "Difèrencia en la taxa anual de segrest de carboni per parcel·la entre els perìodes 1988-92 i 2013-17",

  "wood", "plot_nfi2_nfi4_results", "m3/ha/year",
  "Timber production", "Producción de madera", "Producció de fusta",
  "Differences in the annual timber stock rate per plot between 1988-92 and 2013-17 periods",
  "Diferencia en la tasa anual del stock de madera por parcela entre los los periodos 1988-92 and 2013-17",
  "Difèrencia en la taxa anual de l'estoc de fusta per parcel·la entre els perìodes 1988-92 i 2013-17",

  "admin_municipality", "plot_nfi2_nfi4_results", "-",
  "Municipality", "Municipio", "Municipi",
  "Municipality", "Municipio", "Municipi",

  "admin_region", "plot_nfi2_nfi4_results", "-",
  "County", "Comarca", "Comarca",
  "County", "Comarca", "Comarca",

  "admin_province", "plot_nfi2_nfi4_results", "-",
  "Province", "Provincia", "Provincia",
  "Province", "Provincia", "Provincia",

  "admin_natural_interest_area", "plot_nfi2_nfi4_results", "-",
  "Natural interest areas", "Áreas de interés natural","Àrees d'interès natural",
  "Natural interest areas", "Áreas de interés natural","Àrees d'interès natural",

  "admin_special_protection_natural_area", "plot_nfi2_nfi4_results", "-",
  "Special protection natural areas", "Áreas naturales de protección especial","Àrees naturals de protecció especial",
  "Special protection natural areas", "Áreas naturales de protección especial","Àrees naturals de protecció especial",

  "admin_natura_network_2000", "plot_nfi2_nfi4_results", "-",
  "Natura 2000 network", "Red Natura 2000","Xarxa Natura 2000",
  "Natura 2000 network", "Red Natura 2000","Xarxa Natura 2000",
)

## code to prepare `app_translations` dataset goes here

app_translations <- tibble::tribble(
  ~text_id, ~translation_cat, ~translation_eng, ~translation_spa,
  # data version choices
  'h4_data_version', "Fixa l'escala", "Select the scale", "Selecciona la escala",
  'data_version', "Escala temporal", "Temporal scale", "Escala temporal",
  'static', 'Dades estàtics', 'Static data', 'Datos estáticos',
  'dynamic', 'Dades dinamics', 'Dynamic data', 'Datos dinámicos',
  'plot_nfi_2_results', "IFN 2 (~1988-92)", "NFI 2 (~1988-92)", "IFN 2 (~1988-92)",
  'plot_nfi_3_results', "IFN 3 (~2000-01)", "NFI 3 (~2000-01)", "IFN 3 (~2000-01)",
  'plot_nfi_4_results', "IFN 4 (~2014-17)", "NFI 4 (~2014-17)", "IFN 4 (~2014-17)",
  'plot_nfi2_nfi3_results', "Diferències IFN 2 : IFN 3 (1990 ~ 2000)", "Differences NFI 2 : NFI 3 (1990 ~ 2000)", "Diferencias IFN 2 : IFN 3 (1990 ~ 2000)",
  'plot_nfi3_nfi4_results', "Diferències IFN 3 : IFN 4 (2000 ~ 2015)", "Differences NFI 3 : NFI 4 (2000 ~ 2015)", "Diferencias IFN 3 : IFN 4 (2000 ~ 2015)",
  'plot_nfi2_nfi4_results', "Diferències IFN 2 : IFN 4 (1990 ~ 2015)", "Differences NFI 2 : NFI 4 (1990 ~ 2015)", "Diferencias IFN 2 : IFN 4 (1990 ~ 2015)",
  # data scale choices
  'data_scale', 'Escala espacial', 'Spatial scale', 'Escala espacial',
  'local', 'Local', 'Local', 'Local',
  'admin_province', 'Provincia', 'Province', 'Provincia',
  'admin_region', 'Comarca', 'County', 'Comarca',
  'admin_municipality', 'Municipi', 'Municipality', 'Municipio',
  "admin_natural_interest_area", "Àrees d'interès natural", "Natural interest areas", "Áreas de interés natural",
  "admin_special_protection_natural_area", "Àrees naturals de protecció especial", "Special protection natural areas", "Áreas naturales de protección especial",
  "admin_natura_network_2000", "Xarxa Natura 2000", "Natura 2000 network", "Red Natura 2000",
  'drawn_polygon', "Polígon dibuxat", "Drawn polygon", "Polígono dibujado",
  "file", "Arxiu de polìgons", "Polygon file", "Archivo de polígonos",
  "poly_id", "ID polígon", "Polygon ID", "ID polígono",
  # use file selection
  "user_file_sel_label", "Selecciona l'arxiu a carregar", "Select the file to upload", "Selecciona el archivo a cargar",
  "user_file_sel_buttonLabel", "Inspecciona...", "Browse...", "Inspecciona...",
  "user_file_sel_placeholder", "Cap fitxer seleccionat", "No file selected", "Ningún archivo seleccionado",
  "file_text", 'El fitxer pot ser un shapefile (comprimit en un fitxer zip) o un fitxer GeoPackage (.gpkg). Han de tenir un camp anomenat "poly_id" amb els identificadors dels polígons continguts.', 'File can be a shapefile (compressed in a zip file) or GeoPackage file (.gpkg). They must have a field called "poly_id" with the identifiers of the contained polygons.', 'El archivo puede ser un shapefile (comprimido en un archivo zip) o un archivo GeoPackage (.gpkg). Deben tener un campo llamado "poly_id" con los identificadores de los polígonos contenidos.',
  # map
  'Relief', 'Relleu (base)', 'Relief (base)', 'Relieve (base)',
  'Imaginery', 'Satèl·lit (base)', 'Imaginery (base)', 'Satélite (base)',
  "cite_div", "Dades elaborades pel CTFC i el CREAF.", "Data prepared by the CTFC and CREAF.", "Datos elaborados por el CTFC y el CREAF.",
  # tabs translations
  "main_tab_translation", "Explora", "Explore", "Explora",
  "data_translation", "Dades", "Data", "Datos",
  "save_translation", "Guardar", "Save", "Guardar",
  "help_translation", "Ajuda", "Help", "Ayuda",
  "map_translation", "Mapa", "Map", "Mapa",
  "table_translation", "Taula", "Table", "Tabla",
  # metric choices
  'mean', 'Mitjana', 'Mean', 'Media',
  'min', 'Minim', 'Minimum', 'Mínimo',
  'max', 'Maxim', 'Maximum', 'Máximo',
  'se', 'ES', 'SE', 'ES',
  'q05', 'Quartil 5', 'Quantile 5', 'Cuartil 5',
  'q95', 'Quartil 95', 'Quantile 95', 'Cuartil 95',
  'n', 'Nombre parcel·las', 'Plot number', 'Número de parcelas',
  # _metric
  '_mean', ' mitjana', ' mean', ' media',
  '_min', ' minim ', ' minimum', ' mínimo',
  '_max', ' maxim', ' maximum', ' máximo de ',
  '_se', ' ES', ' SE', ' ES',
  '_q05', ' quartil 5', ' quantile 5', ' cuartil 5',
  '_q95', ' quartil 95', ' quantile 95', ' cuartil 95',
  '_n', ' nombre parcel·las', ' plot number', ' número de parcelas',
  # viz
  "h4_servei", "Fixa el servei", "Select the service", "Selecciona el servicio",
  "h4_viz", "Visualització", "Visualization", "Visualización",
  "deselect-all-text", "Ningú", "None selected...", "Ninguno",
  "select-all-text", "Tots", "All selected...", "Todos",
  "count-selected-text-value", "{0} valors seleccionats (de {1})", "{0} values selected (of {1})", "{0} valores seleccionados (de {1})",
  "viz_color_input", "Indicador:", "Indicator:", "Indicador:",
  "viz_statistic_input", "Estadístic:", "Statistic:", "Estadístico:",
  "pal_high", "Discriminar valors alts", "Discriminate higher values", "Discriminar valores altos",
  "pal_low", "Discriminar valors baixos", "Discriminate lower values", "Discriminar valores bajos",
  "pal_normal", "Normal", "Normal", "Normal",
  "viz_pal_config_input", "Configurar paleta", "Config palette", "Configurar paleta",
  "viz_pal_reverse_input", "Invertir la paleta?", "Reverse the palette?", "¿Invertir la paleta?",
  # save
  'save_map_btn', "Guarda el map", "Save the map", "Guarda el mapa",
  'save_table_btn', "Guarda la taula", "Save the table", "Guarda la tabla",
  "csv", "Text (csv)", "Text (csv)", "Texto (csv)",
  "xlsx", "MS Excel (xlsx)", "MS Excel (xlsx)", "MS Excel (xlsx)",
  "table_output_options_input", "Selecciona el format", "Choose the output format", "Selecciona el formato",
  # help module
  "glossary_var_input", "Selecciona el indicador a descriure", "Choose the indicator to describe", "Selecciona el indicador a describir",
  "link_to_tutorials_text", "Per obtenir més informació, aneu al tutorial de l'aplicació aquí", "For more info, please go to the application tutorial here", "Para obtener más información, vaya al tutorial de la aplicación aquí.",
  "var_description_title", "Descripció:", "Description:", "Descripción:",
  "var_units_title", "Unitats:", "Units:", "Unidades:",
  # info module
  "plot_id_info_plot_title", "Parcel·la seleccionada comparada amb les altres parcel·les al mapa", "Clicked plot compared to other plots in map", "Parcela seleccionada comparada con las otras parcelas en el mapa",
  "admin_region_info_plot_title", "Comarca seleccionada comparada amb les altres comarques al mapa", "Clicked region compared to other regions in map", "Comarca seleccionada comparada con las otras comarcas en el mapa",
  "admin_municipality_info_plot_title", "Municipi seleccionado comparad amb els altres municipis al mapa", "Clicked municipality compared to other municipalities in map", "Municipio seleccionada comparada con los otros municipios en el mapa",
  "poly_id_info_plot_title", "Polìgon seleccionado comparado amb les altres polìgons al mapa", "Clicked polygon compared to other polygons in map", "Polígono seleccionada comparada con los otros polígonos en el mapa",
  "admin_natura_network_2000_info_plot_title", "Àrea seleccionada comparada amb les altres àreas al mapa", "Clicked natural area compared to other natural areas in map", "Área seleccionada comparada con las otras áreas en el mapa",
  "admin_special_protection_natural_area_info_plot_title", "Àrea seleccionada comparada amb les altres àreas al mapa", "Clicked natural area compared to other natural areas in map", "Área seleccionada comparada con las otras áreas en el mapa",
  "admin_natural_interest_area_info_plot_title", "Àrea seleccionada comparada amb les altres àreas al mapa", "Clicked natural area compared to other natural areas in map", "Área seleccionada comparada con las otras áreas en el mapa",
  # progress bar
  "progress_message", "Obtenció de dades...", "Obtaining data...", "Obteniendo datos...",
  "progress_detail_initial", "Escalant les dades", "Data scaling", "Escalando los datos",
)
