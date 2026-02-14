library(readxl)
library(dplyr)
library(stringi)
library(readr)

options(scipen = 999)

# Extensión aproximada de Alemania
LAT_MIN <- 47
LAT_MAX <- 56
LON_MIN <- 5
LON_MAX <- 16

norm_txt <- function(x, strip_accents = TRUE){
  x <- tolower(trimws(as.character(x)))
  if(strip_accents) x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x
}

# -------------------------------
# 1) DESCARGAR EXCEL DESDE GITHUB
# -------------------------------

# URL del archivo en GitHub (usar raw.githubusercontent.com)
github_url <- "https://github.com/javiermunoz-acebes/filologia_digital/raw/main/toponimia_alemania/verwaltungsgebiete.xlsx"

# Crear carpeta temporal si no existe
if (!dir.exists("datos")) {
  dir.create("datos")
}

# Ruta local donde guardar el archivo
xls_path <- "datos/verwaltungsgebiete.xlsx"

# Descargar solo si no existe ya
if (!file.exists(xls_path)) {
  cat("📥 Descargando archivo desde GitHub...\n")
  download.file(
    url = github_url,
    destfile = xls_path,
    mode = "wb",  # IMPORTANTE: modo binario para Excel
    quiet = FALSE
  )
  cat("✓ Descarga completada\n\n")
} else {
  cat("✓ Archivo ya existe en:", xls_path, "\n\n")
}

# -------------------------------
# 2) LEER EXCEL
# -------------------------------

cat("📂 Leyendo datos del Excel...\n")

# La información que nos interesa está en las hojas:
# - "VGTB_ATT_VG" (atributos)
# - "VG250_PK" (coordenadas)
att <- read_xlsx(xls_path, sheet = "VGTB_ATT_VG")
pk  <- read_xlsx(xls_path, sheet = "VG250_PK")

cat("Datos leídos:\n")
cat(sprintf("  VGTB_ATT_VG: %d registros\n", nrow(att)))
cat(sprintf("  VG250_PK: %d registros\n", nrow(pk)))

# -------------------------------
# 3) NORMALIZAR AGS
# -------------------------------

cat("\n🔧 Normalizando códigos AGS...\n")

# AGS en pk es numérico, hay que convertirlo a string con 8 dígitos
pk <- pk %>%
  mutate(AGS = sprintf("%08.0f", as.numeric(AGS)))

# En att, convertir a string
att <- att %>%
  mutate(AGS = as.character(AGS))

cat("✓ AGS normalizado\n")

# -------------------------------
# 4) PREPARAR Y UNIR DATOS
# -------------------------------

cat("\n🔗 Uniendo atributos y coordenadas...\n")

# Seleccionar coordenadas
pk_small <- pk %>%
  select(AGS, lon = LON_DEZ, lat = LAT_DEZ)

# Seleccionar entidades válidas
att_sel <- att %>%
  filter(!is.na(AGS), AGS != "--------") %>%
  select(AGS, gen = GEN, bez = BEZ, lkz = LKZ)

# Unir por AGS
df <- att_sel %>%
  inner_join(pk_small, by = "AGS")

cat(sprintf("✓ Total después del join: %d\n", nrow(df)))

# Mostrar distribución por Bundesland
cat("\nDistribución por Bundesland:\n")
print(table(df$lkz))

# -------------------------------
# 5) PREPARAR TABLA FINAL
# -------------------------------

cat("\n  Preparando datos para la aplicación...\n")

df_final <- df %>%
  transmute(
    toponimo = norm_txt(gen, FALSE),
    provincia = paste0(norm_txt(bez, FALSE), " (", lkz, ")"),
    lon = as.numeric(lon),
    lat = as.numeric(lat)
  ) %>%
  filter(
    !is.na(lon), !is.na(lat),
    lat >= LAT_MIN, lat <= LAT_MAX,
    lon >= LON_MIN, lon <= LON_MAX
  )

cat(sprintf("✓ Puntos después de filtrar: %d\n", nrow(df_final)))

# -------------------------------
# 6) GUARDAR TSV
# -------------------------------

outfile <- "toponimos_de_final.tsv"
write_tsv(df_final, outfile)
cat(sprintf("\n Archivo guardado en: %s\n", outfile))
cat(sprintf("   Tamaño: %.1f KB\n", file.size(outfile) / 1024))

cat("\n✨ ¡Proceso completado!\n")
