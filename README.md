# base_datos_INDEC
El objetivo de este repositorio es tener los scripts que generan las bases de datos que nos solicita INDEC de manera anual para su anuario. Debe tenerse en cuenta que además de completar el año solicitado se hace una revisión de los datos de la serie para atrás. 

En 2022 nos enviaron 6 archivos (excel):

1. Turismo receptivo según vía: se completa con info de tablero (https://tableros.yvera.tur.ar/turismo_internacional/), solo apertura por vía, ya que sino se encuentran diferencias.

2. Turismo receptivo según vía con apertura de paises: la info sale del script ti_data_indec.R

3. Turismo emisivo según vía: se completa con info de tablero (https://tableros.yvera.tur.ar/turismo_internacional/), solo apertura por vía, ya que sino se encuentran diferencias.

4. Turismo interno: info de turistas, pernoctes y gasto nominal según las siguientes categorías: region origen, region destino, alojamiento, motivo y transporte. Se completa con la info del script evyth_data_indec.R

5. Turismo interno:info de turistas, excursionistas, visitantes, pernoctes (turistas) y gasto nominal (visitantes) por trimestre. Se completa con la info del script evyth_data_indec.R

6. Info PN: lo completa la persona encarga de relevar esta información 
