################################
## Espacialidad en variables ambientales
##
##  BIODATA: Decisiones previas para la modelar la biodiversidad
##  Dra. Camila Neder 
##
##  Instituto de Biodiversidad de Ecosistemas Antárticos y Subantárticos (BASE)
##
##         2025
################################

#######################
# Resulta necesario:
# - Atribuciones: Identificar autores y citas de paquetes, scripts o bases de datos utilizados
# - Determinar la resolución temporal: definir los años con los que se trabajan
# - Determinar la resolución espacial: área de estudio, proyecciones
# - Considerar otras decisiones en variables biológicas (ocurrencias) y selección de variables mencionadas en clases


#######################
## CONFIGURACIONES & ATRIBUCIONES
#######################

#Configurar directorio a donde tenemos el Script abierto
getwd()
directorio_main <- file.path(getwd())

thisPath <- function() {
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  if (length(grep("^-f$", cmdArgs)) > 0) {
    # R console option
    normalizePath(dirname(cmdArgs[grep("^-f", cmdArgs) + 1]))[1]
  } else if (length(grep("^--file=", cmdArgs)) > 0) {
    # Rscript/R console option
    scriptPath <- normalizePath(dirname(sub("^--file=", "", cmdArgs[grep("^--file=", cmdArgs)])))[1]
  } else if (Sys.getenv("RSTUDIO") == "1") {
    # RStudio
    dirname(rstudioapi::getSourceEditorContext()$path)
  } else if (is.null(attr(stub, "srcref")) == FALSE) {
    # 'source'd via R console
    dirname(normalizePath(attr(attr(stub, "srcref"), "srcfile")$filename))
  } else {
    stop("Cannot find file path")
  }
} # end of 'thisPath <- function() {'

path_main <- thisPath()

## define the paths to different folders
path_raster_data <- file.path(path_main, "Raster_data")

if (!dir.exists(path_raster_data)) {
      dir.create(path_raster_data, recursive = TRUE)
   }

# Citations: 
# (1) Neder et al.2024 https://zenodo.org/records/14236049 & DOI: https://doi.org/10.3354/meps
#
# (2) EcoCommons Australia 2024. EcoCommons Australia – a collaborative commons for 
# ecological and environmental modelling, EcoCommons, Queensland Cyber
# Infrastructure Foundation, Brisbane, Queensland. 
# Available: https://data–explorer.app.ecocommons.org.au/ 
# (Accessed: June 06, 2025).  https://doi.org/10.47486/PL108

# Version y Citar a R:
citation() # para obtener la última cita de R, se usa dicha función
version #para conocer la versión de R. Esto resulta importante para el uso de paquetes

# Lista de paquetes
#    terra y raster son semajantes, hoy en día se supone que terra es más usado y evita errores
packages <- c("terra", "enmSdmX", "sp", "gstat")

# Install & Load. Llamar en librería si están instalados, y sino instalarlos
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(char = packages, install = TRUE)

# Citar Paquetes
#paquete por paquete
citation(package = "terra") #también se recomienda revisar la publicación asociada

#en loop para la lista de paquetes que generamos antes
for (i in packages) {
  print(pacman::p_cite(package = i, copy2clip = interactive(), tex = TRUE))
}


#######################
## PARÁMETROS AMBIENTALES
#######################

#######################
## 1. Cambiamos el directorio a donde están ubicados los rasters
if(getwd() != path_raster_data) { setwd(path_raster_data) }

#######################
## 2. Creamos una lista con todos los archivos de extensión ".tiff" que estén en el directorio indicado
## Tener en cuenta que a veces los rasters desde bases de datos se descargan como archivo ".nc" (Network Common Data Form)
## Podés ver cómo convertirlos a "TIFF" en https://github.com/Cam-in/nc-to-tiff

tiff_paths <- list.files(path = path_raster_data, pattern = "\\.tif(f)?$", full.names = TRUE) #revisar extensión ".tif" o ".tiff" según casos.
length(tiff_paths)

#######################

## 3a. Podemos plotear uno de los rasters. 
par(mfrow=c(1,2))
plot(rast(tiff_paths[1]), main = "Batimetría BioOracle") #Plot Batimetría BioOracle
plot(rast(tiff_paths[2]), main = "Profundidad Fabri-Ruiz") #Plot Profundidad Fabri-Ruiz 2017
dev.off()

plot(rast(tiff_paths)) #¿Y si mejor ploteamos todos juntos?- No funcionará así porque es una lista.

## 3b. Veamos la información de algún raster
rast(tiff_paths[1]) # Características de Batimetría de BioOracle
rast(tiff_paths[2]) # Características de Profundidad de Fabri-Ruiz 2017

## 3c. Chequear tipo de dato continuo o categórico
for (i in seq_along(tiff_paths)) {
  print(basename(tiff_paths[[i]])) #para ver el nombre del raster
  if (is.factor(tiff_paths[[i]])) {
    cat(names(tiff_paths[[i]]), "es categórico.\n")
  } else {
    cat(names(tiff_paths[[i]]), "es continuo.\n")
  }
}


## Para trabajar mejor, le aplicamos una fórmula a todos diciéndole que son rasters
tiff_raster <- lapply(tiff_paths, terra::rast)
#Conversión de Geomorfología como categórico
tiff_raster[[3]] <- as.factor(tiff_raster[[3]]) #reconoce el path del raster 3 "Geomorfología" para indicarlo como categórico

for (i in seq_along(tiff_raster)) {
      if (is.factor(tiff_raster[[i]])) {
         cat(names(tiff_raster[[i]]), "es categórico.\n")
       } else {cat(names(tiff_raster[[i]]), "es continuo.\n")}
   }
  
#######################
## 4. Creación del Stack (guardado de metadatos y resample si necesario)

## 4.a Apilar todos los rasters en un raster stack "ladrillo"
##   si corremos esto ahora, no sería posible porque las extensiones son diferentes. Vamos a chequearlas...
all_raster <- terra::rast(tiff_raster) # la función rast es para decir que es un raster o un stack de raster. El paquete raster usa la función stack

## 4.b Creamos un data frame vacío para guardar los metadatos de los raster de interés.
ref_raster <- tiff_raster[[1]] #indicamos el raster referente a "bathymetry" de BioOracle
all_raster <- rast(ref_raster) #agregamos el primer objeto para el stack

raster_meta_df <- data.frame(
                      name = character(),
                      nrow = integer(),
                      ncol = integer(),
                      ncell = integer(),
                      crs = character(),
                      resolution = character(), #resolution_X_Y=character(),
                      extent = character(),
                      min_Value = numeric(),
                      max_Value = numeric(),
                      stringsAsFactors = FALSE
                    )

##  Completamos el data frame vacío para guardar los metadatos de los rasters de interés.
##  Iniciamos con el raster de referencia

raster_meta_df <- rbind(raster_meta_df, data.frame(
  nombre = basename(tiff_paths[1]), #nombre extraído desde la lista de paths
  nrow = nrow(ref_raster),
  ncol = ncol(ref_raster),
  ncell = ncell(ref_raster),
  crs = crs(ref_raster, proj = TRUE), #paste0("EPSG: ", crs(ref_raster, describe = TRUE)$code)
  resolution = paste0("X=", res(ref_raster)[1], ", Y=", res(ref_raster)[2]),
  extent = paste0("Xmin=", ext(ref_raster)[1], #round(ext(ref_raster)[1], 2) #Si se quisiese redondear valores en dos decimales
                  ", Xmax=", ext(ref_raster)[2],
                  ", Ymin=", ext(ref_raster)[3],
                  ", Ymax=", ext(ref_raster)[4]),
  min_Value = global(ref_raster, min, na.rm = TRUE)[1, 1], #Removemos los valores NA para identificar los min y max
  max_Value = global(ref_raster, max, na.rm = TRUE)[1, 1]
))

## 4.c Se repite proceso en un loop para cada raster del conjunto agrupado en  "tiff_raster", iniciando por el raster número 2
for (i in 2:length(tiff_raster)) {
  print(paste("Processing", names(tiff_raster[[i]])))
  r_temp <- tiff_raster[[i]]
  
  # Guardar metadatos antes de modificaciones con el mismo tipo de información/código que el raster de referencia
  raster_meta_df <- rbind(raster_meta_df, data.frame(
    nombre = names(tiff_raster[[i]]),
    nrow = nrow(r_temp),
    ncol = ncol(r_temp),
    ncell = ncell(r_temp),
    crs = crs(r_temp, proj = TRUE), 
    resolution = paste0("X=", res(r_temp)[1], ", Y=", res(r_temp)[2]),
    extent = paste0("Xmin=", ext(r_temp)[1], 
                    ", Xmax=", ext(r_temp)[2],
                    ", Ymin=", ext(r_temp)[3],
                    ", Ymax=", ext(r_temp)[4]),
    min_Value = global(r_temp, min, na.rm = TRUE)[1, 1], 
    max_Value = global(r_temp, max, na.rm = TRUE)[1, 1]
  ))

  
    # Verificar si no coinciden geometrías clave
  mismatch <- !compareGeom(ref_raster, r_temp, 
                           crs = TRUE, 
                           ext = TRUE, 
                           rowcol = TRUE, 
                           res = TRUE, 
                           stopOnError = FALSE)
  
  if (mismatch) {print(paste0(names(tiff_raster[[i]]), " ouch, oh no!"))}
  
  # Verificamos y corregimos proyección, extensión y resolución del raster
  {
    method <- if (is.factor(r_temp)) "near" else "bilinear" #Determinamos el método de interpolación según el raster. Si es categórico se reproyecta con nearest neighbor interpolation porque conserva los valores discretos, sino con bilinear para valores continuos
    
    #  Reproyección si CRS no coincide
    if (!crs(ref_raster) == crs(r_temp)) {
      print(paste("Repojecting", names(tiff_raster[[i]]), "using", method))
      r_temp <- project(r_temp, ref_raster, method = method)
    }
    
    # Modificar extensión si no coincide
    if (!ext(r_temp) == ext(ref_raster)) {
      print(paste("Adjust extension", names(tiff_raster[[i]])))
      r_temp <- extend(r_temp, ext(ref_raster))   # Amplía si es menor
      r_temp <- crop(r_temp, ext(ref_raster))     # Recorta si se pasa
    }
    
    # Modificar resolución si no coincide
    if (!identical(res(r_temp), res(ref_raster))) {
      print(paste("Adjust resolution", names(tiff_raster[[i]]), "using", method))
      r_temp <- resample(r_temp, ref_raster, method = method)
    }
  }
  
      tiff_raster[[i]] <- r_temp # Reemplazar el raster corregido
}
  
  
# 4.d Creación final del raster stack   
all_raster <- rast(tiff_raster)
print(all_raster)
plot(all_raster) ## Acá como resultado se ven que los plot fueron extendidos (vacío porque no hay información) pero coincidiendo con la extensión de Bathymetry de BioOracle y cambio de resolución


############################
## 5. Interpretación de lo que hicimos

### 5a. Ejemplo de cómo se ve lo que acabamos de hacer para RESAMPLE si las resoluciones y extensión no coinciden
# Creación de dos rasters simples Raster 1 (r1) y Raster 2 (r2)
par(mfrow=c(1,2))
r1 <- rast(nrows=10, ncols=10, xmin=0, xmax=10, ymin=0, ymax=10)
r1
values(r1) <- 1:ncell(r1)
plot(r1, main="Ráster 1 (Resolución gruesa)")

r2 <- rast(nrows=20, ncols=20, xmin=0, xmax=5, ymin=0, ymax=5)
values(r2) <- sample(1:100, ncell(r2), replace=TRUE)
plot(r2, main="Ráster 2 (Resolución fina)")

# Ahora remuestreamos el r2 para que coincida con el r1
par(mfrow=c(1,3))
plot(r1, main="Ráster 1 (Resolución gruesa)")
r2_resampled <- resample(r2, r1, method="bilinear") 
plot(r2_resampled, main="Ráster 2 Remuestreado (a r1)")

# Ahora remuestreamos el r1 para que coincida con el r2
r1_resampled <- resample(r1, r2, method="bilinear") 
plot(r1_resampled, main="Ráster 1 Remuestreado (a r2)")
dev.off()


### 5.b Otro ejemplo de lo que se podría hacer

# Ráster de alta resolución (fino)
r_original <- rast(nrows=20, ncols=20, xmin=0, xmax=10, ymin=0, ymax=10)
values(r_original) <- 1:ncell(r_original)
plot(r_original, main="Alta resolución (20x20)")

# Reducir resolución 2x (cada celda es el promedio de 2x2 celdas)
r_low_agg <- aggregate(r_original, fact=2, fun=mean)
plot(r_low_agg, main="Agregado (Resolución reducida con aggregate)")

# Remuestrear para que coincida con la resolución alta, con interpolación bilineal
r_resampled <- resample(r_low_agg, r_original, method="bilinear")
plot(r_resampled, main="Remuestreado (r_low_agg a grilla alta con resample)")

# Mostrar las 3 gráficas juntas
par(mfrow=c(1,3))
plot(r_original, main="Alta resolución (original)")
plot(r_low_agg, main="Aggregate (baja resolución)")
plot(r_resampled, main="Resample (interpolado)")

r_high
r_low_agg #resolución mayor al original
r_resampled #misma resolución que el original pero valores interpolados 

dev.off()


 
 
 
####################################################################################
### Otro ejemplo para entender cómo sucede cuando cuando la extensión es diferente  e interpolar los datos (a veces es mejor usar herramientas geoespaciales como ArcGIS o QGIS)
library(terra)
library(gstat)
library(enmSdmX)
library(sp)

# Crear raster original con valores aleatorios
r <- terra::rast(nrows=10, ncols=10, xmin=0, xmax=10, ymin=0, ymax=10)
values(r) <- runif(ncell(r), 1, 100)
plot(r, main="Raster original")

# Convertir raster a puntos (SpatialPointsDataFrame) para gstat
pts <- as.points(r)
pts_sp <- spatVectorToSpatial(pts)
names(pts_sp@data) <- "value"

# Crear raster destino (grilla fina donde queremos interpolar)
r_target <- rast(nrows=100, ncols=100, xmin=0, xmax=10, ymin=0, ymax=10)

# Crear puntos de destino para la predicción (SpatialPointsDataFrame)
grid <- as.points(r_target) # da error porque son datos vacíos
grid_sp <- spatVectorToSpatial(grid)

# Ejecutar interpolación IDW con gstat
idw_result <- gstat::idw(value ~ 1, locations = pts_sp, newdata = grid_sp, idp = 2)

# Extraer valores interpolados (en el slot 'var1.pred')
interpolated_values <- idw_result$var1.pred

# Añadir valores interpolados al raster destino
values(r_target) <- interpolated_values

# Ahora r_target es el raster interpolado
plot(r_target, main = "Interpolación IDW")
 

 

