
# AYUDANTÍA 3 · Análisis descriptivo, bivariado y gráficos
# Métodos Cuantitativos para la Administración Pública
# FAGOB · 10 de abril de 2026


# 0. PAQUETES ----

pacman::p_load(
  tidyverse,    # colección de paquetes para manipulación de datos
  dplyr,        # para manipular datos (group_by, summarise...)
  janitor,      # para tablas rápidas de frecuencia (tabyl)
  knitr,        # para renderizar y crear tablas (kable)
  kableExtra,   # para formatear tablas (kable_classic, add_footnote...)
  ggplot2,      # para gráficos (barras, boxplot, dispersión)
  stargazer,    # para estadísticos descriptivos en formato tabla
  sjmisc        # para tablas de frecuencia con etiquetas (frq)
)

options(scipen = 999) # desactivar notación científica
rm(list = ls())       # limpiar ambiente de trabajo


# 1. INPUT: CARGAR DATOS PROCESADOS ----


# Cargamos la base procesada generada al final de AY-2

casen_proc <- readRDS("output/casen_proc.rds")

# o bien, directamente del link [seleccionar los códigos y presionare ctrl + shift + c para desactivar modo comentario]

# url_casen <- "https://raw.githubusercontent.com/Cristobal-Mejias-G/Clase-2-MC-FAGOB/main/output/casen_proc.rds"
# 
# tmp <- tempfile(fileext = ".rds")
# 
# download.file(url_casen, tmp, mode = "wb") # 'wb' es clave para archivos binarios como .rds
# 
# casen_proc <- readRDS(tmp)


# Primera mirada
glimpse(casen_proc)
names(casen_proc)


# BLOQUE 1: TABLAS DE FRECUENCIA ----


# -- 1.1 Pipeline dplyr + kableExtra (enfoque principal) --
# Patrón: crear objeto con dplyr, luego formatear con kableExtra
# dplyr procesa los datos y kableExtra "viste" la tabla
# El objeto intermedio (tab_pobreza) separa el procesamiento del formato

# Paso 1: Procesar datos
tab_pobreza <- casen_proc %>%
  dplyr::group_by(pobreza) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::mutate(prop = round(n / sum(n) * 100, 2))

tab_pobreza

# Paso 2: Formatear tabla (Compatible con HTML, PDF y Word)
tabla1 <- tab_pobreza %>%
  kableExtra::kbl(
    align     = "c",
    col.names = c("Situación de pobreza", "N", "%"),
    caption   = "Tabla 1. Distribución por situación de pobreza"
  ) %>%
  kableExtra::kable_classic(full_width = FALSE, position = "center", font_size = 14) %>%
  kableExtra::footnote(general = "Fuente: Elaboración propia en base a CASEN 2024.",
                       general_title = "")

tabla1


# -- 1.2 janitor::tabyl() — alternativa rápida para exploración --
# Útil para explorar cruces en pocas líneas; menos control sobre el formato

casen_proc %>%
  janitor::tabyl(pobreza, sexo) %>%
  kableExtra::kable(caption = "Tabla rápida: pobreza según sexo") %>%
  kableExtra::kable_styling(full_width = TRUE)


# -- 1.3 sjmisc::frq() — frecuencias con etiquetas --
# Conveniente cuando las variables tienen etiquetas tipo haven

sjmisc::frq(casen_proc$pobreza)
sjmisc::frq(casen_proc$sexo)


# BLOQUE 2: ESTADÍSTICOS DESCRIPTIVOS POR GRUPO ----

# Patrón: Procesamiento (dplyr) -> Formato (kableExtra)

tab_ingreso <- casen_proc %>%
  dplyr::group_by(sexo) %>%
  dplyr::summarise(
    n       = n(),
    min     = min(ingreso, na.rm = TRUE),
    max     = max(ingreso, na.rm = TRUE),
    media   = round(mean(ingreso, na.rm = TRUE), 0),
    sd      = round(sd(ingreso, na.rm = TRUE), 0),
    mediana = median(ingreso, na.rm = TRUE)
  )

# Mostramos la tabla formateada

tabla2 <- tab_ingreso %>%
  kableExtra::kbl(
    align     = "c",
    col.names = c("Sexo", "N", "Mínimo", "Máximo", "Media", "DS", "Mediana"),
    caption   = "Tabla 2. Ingreso laboral según sexo",
    format.args = list(big.mark = ".", decimal.mark = ",") # Formatear números con puntos de miles (opcional pero muy didáctico)
  ) %>%
  kableExtra::kable_classic(full_width = FALSE, position = "center", font_size = 14) %>%
  kableExtra::footnote(general = "Fuente: Elaboración propia en base a CASEN 2024.",
                       general_title = "")

tabla2


# Lo mismo puede hacerse agrupando por otra variable, como tramo de edad

tab_ingreso_edad <- casen_proc %>%
  dplyr::group_by(tramo_edad) %>%
  dplyr::summarise(
    n       = n(),
    media   = round(mean(ingreso, na.rm = TRUE), 0),
    sd      = round(sd(ingreso, na.rm = TRUE), 0),
    mediana = median(ingreso, na.rm = TRUE)
  )


tabla3 <- tab_ingreso_edad %>%
  kableExtra::kbl(
    align     = "c",
    col.names = c("Tramo etario", "N", "Media", "DS", "Mediana"),
    caption   = "Tabla 3. Ingreso laboral según tramo etario"
  ) %>%
  kableExtra::kable_classic(full_width = FALSE, position = "center", font_size = 14) %>%
  kableExtra::footnote(general = "Fuente: Elaboración propia en base a CASEN 2024.",
                       general_title = "")

tabla3


# Alternativa rápida: `stargazer`

stargazer::stargazer(
  as.data.frame(casen_proc),
  type         = "text",
  summary.stat = c("mean", "sd", "min", "max")


# BLOQUE 3: GRÁFICOS CON GGPLOT2 ----


# -- 3.1 Gráfico de barras: Frecuencia de pobreza --

ggplot(data = casen_proc, mapping = aes(x = pobreza, fill = pobreza)) + # CAPA 1: Datos y ejes (El lienzo)
  geom_bar() +                                                         # CAPA 2: Geometría (La forma)
  labs(
    title = "Distribución por situación de pobreza",
    x     = "Situación de pobreza",
    y     = "Frecuencia",
    caption = "Fuente: Elaboración propia en base a CASEN 2024."
  ) +                                                                  # CAPA 3: Etiquetas (Contexto)
  theme_minimal() +                                                    # CAPA 4: Estética general (El estilo)
  theme(legend.position = "none") + 
  scale_fill_manual(values = c("Pobreza severa" = "#d73027",           
                               "Pobreza por ingresos" = "#f46d43", 
                               "Pobreza multidimensional" = "#fdae61", 
                               "No pobre" = "#4575b4"))





# -- 3.2 Barras agrupadas: Pobreza según sexo --

ggplot(casen_proc, aes(x = tramo_ingreso, fill = sexo)) + 
  geom_bar(position = "dodge") +                                       # Capa de geometría con posición ajustada
  scale_fill_manual(values = c("darkblue", "steelblue")) +             # Capa de escalas: controlamos los colores
  labs(
    title = "Distribución de tramo de ingreso según sexo",
    x     = "Tramo de ingreso",
    y     = "Frecuencia",
    fill  = "Sexo"
  ) +
  theme_light()                                                        # Un tema diferente para mostrar variedad
  theme_minimal()

  
  
  

  # -- 3.3 Boxplot: Distribución de ingresos --
  
  casen_proc %>% 
    dplyr::filter(!is.na(ingreso)) %>%                                   # Capa de pre-procesamiento
    ggplot(aes(x = sexo, y = ingreso, fill = sexo)) + 
    geom_boxplot(alpha = 0.7) +                                          # Geometría con transparencia
    scale_y_continuous(labels = scales::dollar_format(prefix = "$", big.mark = ".")) + # Capa de formato de ejes
    labs(
      title = "Ingreso laboral según sexo",
      x     = "Sexo",
      y     = "Ingreso ($)"
    ) +
    theme_minimal() +
    theme(legend.position = "none")


  # -- 3.4 Gráfico de dispersión con línea de tendencia --
  
# geom_smooth(method = "lm") agrega recta de regresión con IC al 95%
# alpha controla la transparencia de los puntos (útil con muchos datos)

ggplot(casen_proc %>% dplyr::filter(!is.na(ingreso)),
       aes(x = edad, y = ingreso)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  labs(
    title = "Relación entre edad e ingreso laboral",
    x     = "Edad",
    y     = "Ingreso laboral ($)"
  ) +
  theme_minimal()



# EJERCICIO AUTÓNOMO (10 min) ----

# Con casen_proc, trabaja con las variables: actividad, tramo_edad, ingreso_pc

# 1. Genera una tabla de frecuencias de actividad con dplyr + kableExtra
#    Debe incluir N, porcentaje, título y nota al pie.
# [Tu código aquí]

# 2. Genera una tabla de estadísticos descriptivos de ingreso_pc por tramo_edad
#    Reporta: n, media, sd, mediana.
# [Tu código aquí]

# 3. Genera un boxplot de ingreso_pc según actividad
# [Tu código aquí]



 