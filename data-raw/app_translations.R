## code to prepare `app_translations` dataset goes here

app_translations <- tibble::tribble(
  ~text_id, ~translation_cat, ~translation_eng, ~translation_spa,
  # fixed_var_choices
  'c1', "Observacions d'animals", 'Wildlife observations', 'Observaciones de animales',
  'p1', 'Bolets', 'Mushrooms', 'Setas',
  'p2', 'Aigua exportada', 'Exported water', 'Agua exportada',
  'r1', 'Carboni segrestat', 'Carbon sequestration', 'Carbono secuestrado',
  'r2', 'Carboni orgànic del sòl', 'Soil organic carbon', 'Carbono orgánico del suelo',
  'r3', 'Cobertura de bosc de ribera', 'Riparian forest cover', 'Cobertura de bosque de ribera',
  'r4', 'Cobertura de bosc en pendent', 'Forest cover in slopes', 'Cobertura de bosque en pendiente',
  # ecosystem services names
  'food', "Provisió d'aliments", 'Food provision', 'Provisión de alimentos',
  'water', "Provisió d'aigua", 'Water provision', 'Provisión de agua',
  'recreation', 'Esbarjo', 'Recreation', 'Recreacional',
  'climate', 'Regulació climàtica', 'Climate regulation', 'Regulación climática',
  'soil', 'Fertilitat del sòl', 'Soil fertility', 'Fertilidad del suelo',
  'flood', "Regulació d'inundacions", 'Flood regulation', 'Regulación de inundaciones',
  'erosion', "Control de l'erosió", 'Erosion control', 'Control de la erosión',
  # ES categories
  'provisioning', "Serveis d'aprovisionament", 'Provisioning services', 'Servicios de aprovisionamiento',
  'cultural', 'Serveis culturals', 'Cultural services', 'Servicios culturales',
  'regulation', 'Serveis de regulació', 'Regulating services', 'Servicios de regulación',
  # main tabs
  'static', 'Dades fixes', 'Static data', 'Datos fijos',
  'dynamic', 'Dades dinamics', 'Dynamic data', 'Datos dinámicos',
  # fixed ui
  "fixed_sidebar_h4_title", "Controls", 'Controls', "Controles",
  "fixed_var_sel", "Seleccioneu el servei que voleu visualitzar", "Select the service to visualize", "Selecciona el servicio a visualizar",
  "fixed_scale", "Seleccioneu la escala de les dades", 'Select the data scale', 'Selecciona la escala de los datos',
  "fixed_metric", "", 'Select the summary metric', 'Selecciona la métrica de agregación',
  "download", "Descarrega", "Download", "Descarga",
  'map', 'Map', 'Map', 'Mapa',
  'table', 'Table', 'Table', 'Tabla',
  'fixed_scale_summ_warning', "Al pujar des l'escala local, només s'inclouen aquells polígons amb 3 o més parcel·les", 'When scaling up from local scale, only those polygons with 3 or more plots are included', 'Al subir desde la escala local, solo se incluyen aquellos polígonos con 3 o más parcelas',
  'fixed_scale_summ_warning_title', 'Avis', 'Warning', 'Aviso',
  # scale choices
  'local', 'Local', 'Local', 'Local',
  'provinces', "Provincies", "Provinces", "Provincias",
  'counties', "Comarques", "Counties", "Comarcas",
  'municipalities', "Municipis", "Municipalities", "Municipios",
  'drawed_poly', "Polígon dibuxat", "Drawn polygon", "Polígono dibujado",
  # metric choices
  'mean', 'Mitjana', 'Mean', 'Media',
  'min', 'Minim', 'Minimum', 'Mínimo',
  'max', 'Maxim', 'Maximum', 'Máximo',
  'n', 'Nombre parcel·las', 'Plot number', 'Número de parcelas',
  # table column names, not added yet
  'admin_province', 'Provincia', 'Province', 'Provincia',
  'admin_region', 'Comarca', 'County', 'Comarca',
  'admin_municipality', 'Municipi', 'Municipality', 'Municipio',
  # map
  'Relief', 'Relleu (base)', 'Relief (base)', 'Relieve (base)',
  'Imaginery', 'Satèl·lit (base)', 'Imaginery (base)', 'Satélite (base)',






  # "poly_type_sel_label", "Seleccioneu els polígons a agregar", "Select the polygons to aggregate", "Selecciona los polígonos a agregar",
  # "user_file_sel_label", "Pengeu un fitxer", "Upload a file", "Cargar un archivo",
  # "user_file_sel_button_label", "Navegueu", "Browse", "Explora",
  # "user_file_sel_placeholder", "No s’ha seleccionat cap fitxer", "No file selected", "No se seleccionado ningún archivo",
  # "sidebar_h4_results", "Resultats", "Results", "Resultados",
  # "sidebar_h4_download", "Descarrega", "Download", "Descarga",
  # "main_panel_raster_siz_1", "Ràster per a la visualització té una mida de cel·la de 400x400 metres.", "Raster for visualization has a cell size of 400x400 meters.", "El ráster para la visualización tiene un tamaño de celda de 400x400 metros.",
  # "main_panel_raster_siz_2", "Ràster per als càlculs té una mida de cel·la de 20x20 metres.", "Raster for calculations has a cell size of 20x20 meters.", "El ráster para los cálculos tiene un tamaño de celda de 20x20 metros.",
  # "data_format_label", "Format de dades", "Data format", "Formato de los datos",
  # "data_length_label", "¿Totes les variables?", "All the variables?", "¿Todas las variables?",
  # "modal_dismiss_label", "Cancel·lar", "Dismiss", "Cancelar",
  # # lidar_val_sel choices
  # 'AB', "Àrea Basal [m²/ha]", "Basal Area [m²/ha]", "Área Basal [m²/ha]",
  # 'BAT',  "Biomassa Aèria Total [t/ha]", "Total Aerial Biomass [t/ha]", "Biomasa Aérea Total [t/ha]",
  # 'BF', "Biomassa de fulles [t/ha]", "Leaf Biomass [t/ha]", "Biomasa de Hojas [t/ha]",
  # 'CAT', "Carboni Aéri Total [t/ha]", "Total Aerial Carbon [t/ha]", "Carbono Aéreo Total [t/ha]",
  # 'DBH', "Diàmetre Normal [cm]", "Diameter at Breast Height [cm]", " Diámetro Normal [cm]",
  # 'HM', "Altura Mitjana [m]", "Mean Height [m]", "Altura Media [m]",
  # 'REC', "Recobriment [%]", "Coating [%]", "Recubrimiento [%]",
  # 'VAE', "Volum amb Escorça [m³/ha]", "Over Bark Volume[m³/ha]", "Volúmen con Corteza [m³/ha]",
  # # poly type sel choices
  # 'Catalonia', "Catalunya", "Catalonia", "Cataluña",
  # 'Provinces', "Provincies", "Provinces", "Provincias",
  # 'Counties', "Comarques", "Counties", "Comarcas",
  # 'Municipalities', "Municipis", "Municipalities", "Municipios",
  # 'Veguerias', "Vegueries", "Veguerias", "Veguerias",
  # 'Drawed polygon', "Polígon dibuxat", "Drawn polygon", "Polígono dibujado",
  # 'File upload', "Fitxer", "File upload", "Archivo",
  # # data format options
  # 'GIS', 'SIG', 'GIS', 'SIG',
  # 'TABLE', 'Table', 'Table', 'Tabla',
  # 'shp', 'Shapefile', 'Shapefile', 'Shapefile',
  # 'wkt', 'Well Known Text', 'Well Known Text', 'Well Known Text',
  # 'gpkg', 'GeoPackage', 'GeoPackage', 'GeoPackage',
  # 'csv', 'CSV', 'CSV', 'CSV',
  # 'xlsx', 'Excel', 'Excel', 'Excel',
  # # data length choices
  # 'all_columns', 'Totes les variables', 'All the variables', 'Todas las variables',
  # 'visible', 'Només la variable selecionada', 'Only the selected variable', 'Sólo la variable seleccionada',
  # # map translations
  # 'Relief', 'Relleu (base)', 'Relief (base)', 'Relieve (base)',
  # 'Imaginery', 'Satèl·lit (base)', 'Imaginery (base)', 'Satélite (base)',
  # 'poly', '{input$poly_type_sel %>% translate_app(lang_declared)} (capa)', '{input$poly_type_sel %>% translate_app(lang_declared)} (layer)', '{input$poly_type_sel %>% translate_app(lang_declared)} (capa)',
  # 'lidar', 'LiDAR (capa)', 'LiDAR (layer)', 'LiDAR (capa)',
  # # table names
  # 'poly_id', 'Polígon', 'Polygon', 'Polígono',
  # 'mean_ab', "Àrea Basal Mitjana [m²/ha]", "Mean Basal Area [m²/ha]", "Área Basal Media [m²/ha]",
  # 'mean_bat',  "Biomassa Aèria Total Mitjana [t/ha]", "Mean Total Aerial Biomass [t/ha]", "Biomasa Aérea Total Media [t/ha]",
  # 'mean_bf', "Biomassa de fulles Mitjana [t/ha]", "Mean Leaf Biomass [t/ha]", "Biomasa de Hojas Media [t/ha]",
  # 'mean_cat', "Carboni Aéri Total Mitjan [t/ha]", "Mean Total Aerial Carbon [t/ha]", "Carbono Aéreo Total Medio [t/ha]",
  # 'mean_dbh', "Diàmetre Normal Mitjan [cm]", "Mean Diameter at Breast Height [cm]", " Diámetro Normal Medio [cm]",
  # 'mean_hm', "Altura Mitjana Mitjana [m]", "Mean Height [m]", "Altura Media [m]",
  # 'mean_rec', "Recobriment Mitjan [%]", "Mean Coating [%]", "Recubrimiento Medio [%]",
  # 'mean_vae', "Volum amb Escorça Mitjan [m³/ha]", "Mean Over Bark Volume[m³/ha]", "Volúmen con Corteza Medio [m³/ha]",
  # # validate
  # 'data_res_need', 'Encara no hi ha dades', 'No data yet', 'Sin datos todavía',
  # 'map_click_need', 'No hi ha cap clic del mapa', 'No map click', 'No ha habido click en el mapa',
  # 'file_need', 'No s’ha seleccionat cap fitxer', 'No file selected', 'No se ha seleccionado ningún archivo',
  # 'custom_poly_need', 'No s’ha dibuixat cap polígon. Torneu enrere i dibuixeu un polígon abans de seleccionar "Polígon dibuxat" al menú', 'No custom polygon drawed. Please go back and draw a polygon before selecting "Drawn polygon" from the menu', 'No hay ningún polígono dibujado. Vuelva atrás y dibuje un polígono antes de seleccionar "Polígono dibujado" en el menú',
  # 'polygon_area_need', "L'àrea del polígon (o la suma de les àrees de polígons) està per sobre del valor màxim ({round(user_area/1000000, 1)} > 500 km2)", 'Polygon area (or polygons sum of areas) are above the maximum value ({round(user_area/1000000, 1)} > 500 km2)', 'El área del polígono (o la suma de las áreas de los polígonos) está por encima del valor máximo ({round(user_area/1000000, 1)} > 500 km2)',
  # 'feature_number_need', "El nombre d'objectes (polígons) és superior al valor màxim ({user_features} > 10 polígons)", 'Number of features (polygons) is above the maximum value ({user_features} > 10 polygons)', 'El número de objetos (polígonos) está por encima del valor máximo ({user_features} > 10 polígonos)',
  # # coordinates info
  # 'sidebar_h4_coords', '{input$lidar_var_sel %>% translate_app(lang_declared)} a les coordenades: {round(map_click$lng, 3)}, {round(map_click$lat, 3)}', '{input$lidar_var_sel %>% translate_app(lang_declared)} at coordinates: {round(map_click$lng, 3)}, {round(map_click$lat, 3)}', '{input$lidar_var_sel %>% translate_app(lang_declared)} en las coordenadas: {round(map_click$lng, 3)}, {round(map_click$lat, 3)}',
  # 'sidebar_p_rawraster', 'Valor per al ràster de 20x20m: {round(click_raster_values()$raw, 3)}', '20x20m raster value: {round(click_raster_values()$raw, 3)}', 'Valor para el ráster de 20x20m: {round(click_raster_values()$raw, 3)}',
  # 'sidebar_p_aggraster', 'Valor per al ràster de 400x400m: {round(click_raster_values()$agg, 3)}', '400x400m raster value: {round(click_raster_values()$agg, 3)}', 'Valor para el ráster de 400x400m: {round(click_raster_values()$agg, 3)}',

  ## TODO continue translations thesaurus
)
