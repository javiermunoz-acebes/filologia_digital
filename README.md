# Análisis Toponímico de Alemania
## Herramienta interactiva para el estudio de topónimos alemanes

**GIR Filología Digital**  
Universidad de Valladolid, 2026

---

## Índice

1. [Resumen del proyecto](#resumen)
2. [Fuente de datos](#fuente-de-datos)
3. [Proceso de preparación de datos](#preparacion-datos)
4. [Desarrollo de la aplicación](#desarrollo-aplicacion)
5. [Funcionalidades](#funcionalidades)
6. [Aplicaciones en investigación](#aplicaciones-investigacion)
7. [Guía de uso](#guia-uso)
8. [Requisitos técnicos](#requisitos-tecnicos)

---

## 1. Resumen del proyecto {#resumen}

Este proyecto desarrolla una herramienta interactiva para el análisis toponímico de Alemania, centrada en el estudio de los sufijos toponímicos y su distribución geográfica. La aplicación permite explorar **10,949 topónimos** georeferenciados de todas las unidades administrativas alemanas, facilitando investigaciones en:

- **Filología germánica**: análisis de formaciones léxicas y evolución lingüística
- **Dialectología**: distribución geográfica de variantes (bajo alemán vs. alto alemán)
- **Historia lingüística**: identificación de sustratos eslavos en el este de Alemania
- **Onomástica**: estudio de patrones en la denominación de lugares

### Características principales

**Análisis contextual dinámico**: Las estadísticas se generan automáticamente al buscar sufijos específicos  
**Visualización cartográfica**: Mapas interactivos con clustering de puntos  
**Análisis cuantitativo**: Distribución por Bundesland y estadísticas descriptivas  

---

## 2. Fuente de datos {#fuente-de-datos}

### Origen de los datos

Los datos provienen del **Bundesamt für Kartographie und Geodäsie (BKG)**, la Oficina Federal de Cartografía y Geodesia de Alemania, que proporciona datos geográficos oficiales de libre acceso.

**Fuente oficial**:  
[VG250 - Verwaltungsgebiete 1:250.000](https://gdz.bkg.bund.de/index.php/default/verwaltungsgebiete-1-250-000-stand-01-01-vg250-01-01.html)

### Opciones de descarga disponibles

El BKG ofrece múltiples formatos para diferentes necesidades:

| Formato | Georeferencia | Contenido | Tamaño | Enlace |
|---------|---------------|-----------|--------|--------|
| **Shape** | GK3 | Ebenen | 65 MB | [Descargar](https://daten.gdz.bkg.bund.de/produkte/vg/vg250_ebenen_0101/aktuell/vg250_01-01.gk3.shape.ebenen.zip) |
| **Shape** | TM32 | Ebenen | 67 MB | [Descargar](https://daten.gdz.bkg.bund.de/produkte/vg/vg250_ebenen_0101/aktuell/vg250_01-01.tm32.shape.ebenen.zip) |
| **Shape** | UTM32s | Ebenen | 67 MB | [Descargar](https://daten.gdz.bkg.bund.de/produkte/vg/vg250_ebenen_0101/aktuell/vg250_01-01.utm32s.shape.ebenen.zip) |
| **GeoPackage** | UTM32s | Ebenen | 70 MB | [Descargar](https://daten.gdz.bkg.bund.de/produkte/vg/vg250_ebenen_0101/aktuell/vg250_01-01.utm32s.gpkg.ebenen.zip) |
| **Excel** | — | Tablas | 7 MB | [Descargar](https://daten.gdz.bkg.bund.de/produkte/vg/vg250_ebenen_0101/aktuell/vg250_01-01.ee.excel.ebenen.zip) |

### Formato seleccionado

Para este proyecto elegimos el **formato Excel** por dos razones prácticas:

1. **Accesibilidad**: No requiere software GIS especializado
2. **Facilidad de procesamiento**: Compatible con R y herramientas de análisis de datos estándar
3. **Estructura tabular**: Ideal para unir información de atributos y coordenadas

### Contenido del archivo Excel

El archivo `verwaltungsgebiete.xlsx` contiene múltiples hojas. Las dos que nos interesan son:

- **`VGTB_ATT_VG`** (15,972 registros): Atributos de las entidades administrativas
  - `AGS`: Código oficial de identificación (Amtlicher Gemeindeschlüssel)
  - `GEN`: Nombre de la entidad (topónimo)
  - `BEZ`: Tipo de entidad (Stadt, Gemeinde, etc.)
  - `LKZ`: Código del Bundesland (estado federal)

- **`VG250_PK`** (10,949 registros): Coordenadas geográficas
  - `AGS`: Código de identificación
  - `LON_DEZ`: Longitud decimal
  - `LAT_DEZ`: Latitud decimal

---

## 3. Proceso de preparación de datos {#preparacion-datos}

### Desafío principal: unión de tablas

El mayor reto técnico fue unir las dos hojas del Excel mediante el campo `AGS`, que presentaba **formatos incompatibles**:

- En `VG250_PK`: AGS es **numérico** (tipo `int64`): `1001000`
- En `VGTB_ATT_VG`: AGS es **texto**: `"01001000"` (con cero inicial)

Esta discrepancia impedía la unión directa de las tablas, resultando en pérdida masiva de datos.

### Solución implementada

El script de preparación (`generar_tsv_v2.R`) resuelve este problema en varios pasos:

#### 1. Lectura del archivo Excel

```r
library(readxl)
library(dplyr)
library(stringi)
library(readr)

# Cargar las dos hojas relevantes
att <- read_xlsx("verwaltungsgebiete.xlsx", sheet = "VGTB_ATT_VG")
pk  <- read_xlsx("verwaltungsgebiete.xlsx", sheet = "VG250_PK")
```

#### 2. Normalización del código AGS

La clave está en convertir correctamente el AGS numérico a formato de texto con **8 dígitos**, rellenando con ceros a la izquierda:

```r
# Convertir entero a string con formato de 8 dígitos
pk <- pk %>%
  mutate(AGS = sprintf("%08.0f", as.numeric(AGS)))

# En att, simplemente convertir a texto
att <- att %>%
  mutate(AGS = as.character(AGS))
```

#### 3. Selección y limpieza de datos

```r
# Seleccionar solo coordenadas necesarias
pk_small <- pk %>%
  select(AGS, lon = LON_DEZ, lat = LAT_DEZ)

# Filtrar entidades válidas (excluir separadores y registros vacíos)
att_sel <- att %>%
  filter(!is.na(AGS), AGS != "--------") %>%
  select(AGS, gen = GEN, bez = BEZ, lkz = LKZ)
```

#### 4. Unión de tablas (join)

```r
# Inner join: solo entidades con coordenadas
df <- att_sel %>%
  inner_join(pk_small, by = "AGS")
```

#### 5. Normalización de texto

Para facilitar las búsquedas, normalizamos los topónimos:

```r
norm_txt <- function(x, strip_accents = TRUE){
  x <- tolower(trimws(as.character(x)))
  if(strip_accents) x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x
}

df_final <- df %>%
  transmute(
    toponimo = norm_txt(gen, FALSE),
    provincia = paste0(norm_txt(bez, FALSE), " (", lkz, ")"),
    lon = as.numeric(lon),
    lat = as.numeric(lat)
  )
```

**Efectos de la normalización**:
- Minúsculas: `München` → `munchen`
- Sin acentos: `Düsseldorf` → `dusseldorf`
- Trimmed: elimina espacios extra

#### 6. Filtrado geográfico

Limitamos el área a Alemania continental:

```r
LAT_MIN <- 47   # Sur de Baviera
LAT_MAX <- 56   # Norte de Schleswig-Holstein
LON_MIN <- 5    # Oeste de Renania del Norte-Westfalia
LON_MAX <- 16   # Este de Sajonia

df_final <- df_final %>%
  filter(
    !is.na(lon), !is.na(lat),
    lat >= LAT_MIN, lat <= LAT_MAX,
    lon >= LON_MIN, lon <= LON_MAX
  )
```

#### 7. Exportación a TSV

```r
write_tsv(df_final, "~/Desktop/toponimos_de_final.tsv")
```

**Archivo resultante**: `toponimos_de_final.tsv`
- 10,949 filas (topónimos)
- 4 columnas: `toponimo`, `provincia`, `lon`, `lat`
- Encoding UTF-8
- Listo para cargar en la aplicación Shiny

### Distribución de los datos por Bundesland

| Código | Bundesland | N° topónimos |
|--------|------------|--------------|
| RP | Rheinland-Pfalz | 2,300 |
| BY | Bayern | 2,221 |
| SH | Schleswig-Holstein | 1,106 |
| BW | Baden-Württemberg | 1,103 |
| NI | Niedersachsen | 964 |
| MV | Mecklenburg-Vorpommern | 724 |
| TH | Thüringen | 605 |
| HE | Hessen | 425 |
| SN | Sachsen | 418 |
| BB | Brandenburg | 413 |
| NW | Nordrhein-Westfalen | 396 |
| ST | Sachsen-Anhalt | 218 |
| SL | Saarland | 52 |
| HB | Bremen | 2 |
| HH | Hamburg | 1 |
| BE | Berlin | 1 |

---

## 4. Desarrollo de la aplicación {#desarrollo-aplicacion}

### Tecnología utilizada

La aplicación se desarrolló con **R Shiny**, un framework para crear aplicaciones web interactivas con R. Shiny es ideal para proyectos de humanidades digitales porque:

No requiere conocimientos de HTML/CSS/JavaScript  
Integración nativa con análisis estadístico de R  
Componentes interactivos listos para usar (mapas, gráficos, tablas)  
Despliegue sencillo (local o en servidor)  

### Librerías empleadas

```r
library(shiny)            # Framework web interactivo
library(bslib)            # Temas Bootstrap 5 modernos
library(shinycssloaders)  # Indicadores de carga
library(leaflet)          # Mapas interactivos
library(dplyr)            # Manipulación de datos
library(stringi)          # Procesamiento de texto Unicode
library(htmlwidgets)      # Widgets HTML interactivos
library(webshot2)         # Exportación de mapas a PNG
library(htmltools)        # Construcción de HTML
library(DT)               # Tablas interactivas
```

### Arquitectura de la aplicación

La aplicación sigue el patrón **UI/Server** de Shiny:

```
┌─────────────────────────────────────────┐
│            USUARIO                      │
│   (navegador web)                       │
└──────────────┬──────────────────────────┘
               │
               ▼
┌─────────────────────────────────────────┐
│              UI (interfaz)              │
│  - Barra lateral (búsqueda y filtros)  │
│  - Panel principal (mapa y estadísticas)│
└──────────────┬──────────────────────────┘
               │
               ▼
┌─────────────────────────────────────────┐
│          SERVER (lógica)                │
│  1. Carga datos (TSV)                   │
│  2. Detecta si búsqueda es un sufijo    │
│  3. Filtra topónimos                    │
│  4. Genera análisis contextual          │
│  5. Renderiza mapa y gráficos           │
└─────────────────────────────────────────┘
```

### Innovación: análisis dinámico contextual

A diferencia de otras herramientas toponímicas que clasifican todos los datos por adelantado, nuestra aplicación implementa **análisis dinámico**:

**Enfoque implementado**:
```
Inicio → Mostrar todos los datos 
         ↓
Usuario busca "dorf" → Detectar que es sufijo → Analizar solo "dorf"
         ↓
Mostrar estadísticas contextuales + mapa filtrado
```

**Ventajas**:
1. **Rendimiento**: Carga instantánea (no pre-procesa datos)
2. **Flexibilidad**: Fácil añadir nuevos sufijos sin regenerar archivos
3. **Contextualidad**: Análisis específico de lo que interesa al investigador
4. **Escalabilidad**: Funcionaría igual con más topónimos

---

## 5. Funcionalidades {#funcionalidades}

### 5.1. Búsqueda y filtrado

La aplicación ofrece dos modos de búsqueda:

**Modo "Contiene"** (por defecto):
- Busca el término en cualquier parte del topónimo
- Ejemplo: `"burg"` encuentra → Flensburg, Hamburg, Burgau, Straßburg

**Modo "Exacta"**:
- Coincidencia exacta del topónimo completo
- Ejemplo: `"berlin"` encuentra → solo Berlin 

**Opciones adicionales**:
- **Ignorar acentos**: Permite buscar `dusseldorf` y encontrar `Düsseldorf`
- **Max. puntos**: Control de cuántos topónimos mostrar (100-11,000)
- **Clustering**: Agrupa puntos cercanos para ofrecer mapas más legibles

### 5.2. Análisis contextual de sufijos

Cuando el usuario busca un **sufijo conocido** (ej: `dorf`, `burg`, `bach`), la aplicación:

1. **Detecta automáticamente** que la búsqueda es un sufijo toponímico
2. **Muestra un panel de análisis** con:
   - **Tipo semántico**: asentamiento, geografía, vegetación, eslavo, patronímico
   - **Significado etimológico**: pueblo/aldea, castillo, arroyo, etc.
   - **Total de ocurrencias**: cuántos topónimos tienen ese sufijo
   - **Región predominante**: dónde se concentra geográficamente


### 5.3. Diccionario de sufijos reconocidos

La aplicación incluye un diccionario con **27 sufijos** clasificados en 5 categorías:

#### Asentamientos (7 sufijos)
| Sufijo | Significado | Dialectología |
|--------|-------------|---------------|
| `-dorf` | pueblo/aldea | General |
| `-burg` | fortaleza/castillo | General |
| `-heim` | hogar | General |
| `-hausen` | casas | Alto alemán (sur) |
| `-husen` | casas | Bajo alemán (norte) |
| `-stadt` | ciudad | General |
| `-stedt` | lugar | Bajo alemán |

#### Geografía (7 sufijos)
| Sufijo | Significado | Ejemplos |
|--------|-------------|----------|
| `-berg` | montaña/colina | Heidelberg, Nürnberg |
| `-tal` | valle | Wuppertal, Rheintal |
| `-bach` | arroyo | Auerbach, Griesbach |
| `-furt` | vado | Frankfurt, Erfurt |
| `-brunn` | fuente | Heilbronn, Paderborn |
| `-born` | fuente (norte) | Paderborn, Osnabrück |
| `-brück` | puente | Saarbrücken, Innsbruck |

#### Vegetación (5 sufijos)
| Sufijo | Significado | Contexto |
|--------|-------------|----------|
| `-wald` | bosque | Schwarzwald, Odenwald |
| `-feld` | campo | Bielefeld, Krefeld |
| `-holz` | madera/bosque | Reutlingen, Kirchholz |
| `-au` | pradera ribereña | Passau, Lindau (sur) |
| `-rode` | claro/rozado | Wernigerode, Rode |

#### Eslavos (4 sufijos)
| Sufijo | Origen | Región |
|--------|--------|--------|
| `-itz` | Eslavo | Este (MV, SN, BB) |
| `-ow` | Eslavo | Este (MV principalmente) |
| `-in` | Eslavo | Este |
| `-witz` | Eslavo -ovice | Este |

#### Patronímicos (3 sufijos)
| Sufijo | Significado | Región dialectal |
|--------|-------------|------------------|
| `-ingen` | gente de X | Suabo (suroeste) |
| `-ing` | gente de X | Bávaro (sur) |
| `-ungen` | gente de X | General |

### 5.4. Visualización cartográfica

El mapa interactivo utiliza **Leaflet**, una de las  bibliotecas de mapas web más usada:

**Características**:
- Zoom suave
- Agrupamiento automático de puntos cercanos
- Paneles informativos al hacer clic
- Ajuste automático de vista (fitBounds)
- Tile layer: CartoDB Positron (estilo minimalista)

### 5.5. Estadísticas y gráficos

La pestaña **Estadísticas** ofrece:

#### Panel de análisis de sufijo
(Solo visible cuando se busca un sufijo reconocido)

- Descripción lingüística del sufijo
- Conteo total de ocurrencias
- Distribución porcentual por Bundesland
- Identificación de región predominante

#### Gráfico de distribución geográfica

Gráfico de barras horizontal que muestra:
- Eje X: Número de topónimos
- Eje Y: Códigos de Bundesländer (BB, BY, BW, etc.)
- Ordenado de mayor a menor frecuencia

#### Tabla de datos

Tabla interactiva con:
- **Filtros por columna**: búsqueda independiente en cada campo
- **Paginación**: 25 registros por página
- **Ordenación**: clic en encabezado para ordenar
- **Exportación**: descargar resultados filtrados

**Columnas**:
1. `toponimo`: Nombre normalizado
2. `bundesland`: Código del estado (BY, SH, etc.)
3. `provincia`: Tipo de entidad + estado
4. `lat`, `lon`: Coordenadas decimales

### 5.6. Exportación de resultados

#### Descargar mapa (PNG)

- Formato: PNG de alta resolución (1400×900 px)
- Tecnología: webshot2 (captura del widget Leaflet)
- Nombre de archivo: `mapa_toponimos_[sufijo]_2026-02-14.png`

#### Descargar datos (TSV)

- Formato: Tab-Separated Values (TSV)
- Encoding: UTF-8
- Contenido: Topónimos filtrados según búsqueda actual
- Nombre: `toponimos_[sufijo]_2026-02-14.tsv`

---

## 6. Aplicaciones en investigación {#aplicaciones-investigacion}

### 6.1. Filología germánica

**Análisis de productividad de afijos**

Pregunta de investigación: *¿Qué sufijos toponímicos son más productivos en alemán?*


## 6.2. Dialectología histórica

**Isoglosas toponímicas**

Comparación de variantes dialectales:

| Fenómeno | Alto alemán (sur) | Bajo alemán (norte) |
|----------|-------------------|---------------------|
| "Casas" | `-hausen` | `-husen`  |
| "Lugar" | `-stadt`  | `-stedt`  |

### 6.3. Historia lingüística: sustrato eslavo

**Cuantificación de la frontera eslavo-germánica**

Búsqueda de topónimos eslavos por región

**Sufijos eslavos**:
- `-itz` (156): Chemnitz, Colditz, Görlitz
- `-ow` (199): Güstrow, Teltow, Pankow
- `-in` (322): Schwerin, Stettin, Usedom
- `-witz` (38): Clausnitz, Reinhardtsgrimma


## 7. Guía de uso {#guia-uso}

### 7.1. Inicio rápido

#### Paso 1: Preparar datos

```r
# Ejecutar script de preparación (una sola vez)
source("generar_tsv_v2.R")

# Esto genera: ~/Desktop/toponimos_de_final.tsv
```

#### Paso 2: Lanzar aplicación

```r
# Abrir en RStudio
# Archivo > Abrir: app_toponimia_de_v2.R

# Clic en botón "Run App" (esquina superior derecha)
```

#### Paso 3: Explorar

La aplicación se abre en una ventana de navegador mostrando **todos los topónimos** de Alemania.

### 7.2. Casos de uso típicos

#### Búsqueda simple

**Objetivo**: Encontrar todos los topónimos que contienen "furt" (vado)

1. Escribir `furt` en el campo de búsqueda
2. Modo: **Contiene** (por defecto)
3. El mapa muestra: Frankfurt, Erfurt, Schweinfurt, etc.
4. **Panel de análisis** aparece automáticamente:
   ```
   Sufijo: -furt
   Tipo: geografía
   Significado: vado
   Total: ~28 topónimos
   Región principal: Sur
   ```

#### Análisis dialectal

**Objetivo**: Comparar `-hausen` (sur) vs `-husen` (norte)

1. Buscar `hausen`
   - Resultado: 334 topónimos
   - Región principal: Oeste(Alto alemán central)

2. Buscar `husen`
   - Resultado: 14 
   - Ver mapa → Norte(Bajo alemán)

#### Exportar resultados para publicación

**Escenario**: Artículo sobre topónimos eslavos

1. Buscar `itz` → 282 resultados
2. **Descargar mapa**: botón "Descargar mapa"
   - Obtiene: `mapa_toponimos_itz_2026-02-14.png`
   - Usar en: Figura 1 del artículo

3. **Descargar datos**: botón "Descargar datos (TSV)"
   - Obtiene: `toponimos_itz_2026-02-14.tsv`
   - Usar en: Análisis estadístico adicional en R

4. Repetir para `-ow`, `-in`, `-witz`

5. En R, combinar los TSV y generar gráfico de distribución

#### Identificar patrones históricos

**Ejemplo**: Búsqueda de `-ow`

Si los resultados se concentran en MV (Mecklemburgo-Pomerania), esto indica:
1. **Sustrato eslavo**: Los eslavos habitaron esa zona hasta el siglo XII
2. **Germanización tardía**: Los nombres se conservaron tras la conquista alemana
3. **Frontera lingüística**: Marcador de la frontera histórica Elba-Oder

---

## 8. Requisitos técnicos {#requisitos-tecnicos}

### 8.1. Software necesario

#### R (versión ≥ 4.0)

**Instalación**:
- Windows: [https://cran.r-project.org/bin/windows/base/](https://cran.r-project.org/bin/windows/base/)
- macOS: [https://cran.r-project.org/bin/macosx/](https://cran.r-project.org/bin/macosx/)
- Linux: Desde repositorios de la distribución

#### RStudio (recomendado)

**Instalación**:
[https://posit.co/download/rstudio-desktop/](https://posit.co/download/rstudio-desktop/)

RStudio facilita:
- Ejecución de apps Shiny con un botón
- Edición de código con autocompletado
- Visualización de datos y gráficos

### 8.2. Paquetes R requeridos

#### Instalación automática

```r
# Ejecutar una sola vez:
install.packages(c(
  "shiny",
  "bslib",
  "shinycssloaders",
  "leaflet",
  "dplyr",
  "stringi",
  "htmlwidgets",
  "webshot2",
  "htmltools",
  "DT",
  "readxl",
  "readr"
))
```

---


## 9. Conclusión

Este proyecto demuestra cómo las **herramientas digitales pueden democratizar la investigación filológica**. Lo que antes requería:

- Años de compilación manual de topónimos
- Conocimientos de cartografía profesional
- Acceso a bibliotecas especializadas

Ahora es accesible mediante:

- Una aplicación web interactiva
- Datos oficiales de libre acceso
- Software de código abierto (R + Shiny)

### Contribuciones del proyecto

1. **Metodológicas**: Flujo de trabajo reproducible (descarga → procesamiento → visualización)
2. **Técnicas**: Solución al problema de unión de datos con formatos incompatibles
3. **Pedagógicas**: Herramienta accesible para estudiantes de filología germánica
4. **Científicas**: Cuantificación de patrones toponímicos (ej: 38% de topónimos eslavos en MV)


---

## Licencia y contacto

### Licencia del proyecto

**Código**: MIT License (software libre, modificable y redistribuible)

**Datos**: Los datos originales provienen del BKG (licencia GeoNutzV) y son de dominio público para uso no comercial.

### Contacto

**Responsable**: GIR Filología Digital  
**Institución**: Universidad de Valladolid  
**Email**: gir.filologia.digital@uva.es  

### Citar este proyecto

```
GIR Filología Digital (2026). Análisis Toponímico de Alemania: 
Herramienta interactiva para el estudio de topónimos alemanes [Software]. 
Universidad de Valladolid. https://github.com/filologia_digital/toponimia_alemania
```

---

**Última actualización**: 14 de febrero de 2026  
**Versión del documento**: 1.0
