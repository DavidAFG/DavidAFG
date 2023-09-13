#Inserto librerías
library(readxl)
library(tidyverse)
library(zoo)
library(janitor)
library(pals)
library(plotly)
library(plyr)
library(reshape2)

#Vector con nuevos nombres de columnas
nom_columnas <- c("Periodo", "PIB_Nacional", "PIB_AGS", "PIB_BC", "PIB_BCS", "PIB_CAMP", "PIB_CHIS", "PIB_CHIH", 
                  "PIB_COAH", "PIB_COL", "PIB_CDMX", "PIB_DGO", "PIB_GTO", "PIB_GRO", "PIB_HGO", 
                  "PIB_JAL", "PIB_MEX", "PIB_MICH", "PIB_MOR", "PIB_NAY", "PIB_NL", 
                  "PIB_OAX", "PIB_PUE", "PIB_QRO", "PIB_QR", "PIB_SLP", "PIB_SIN", 
                  "PIB_SON", "PIB_TAB", "PIB_TAMPS", "PIB_TLAX", "PIB_VER", "PIB_YUC", 
                  "PIB_ZAC", "Pob_total", "Pob_ocupada")

#Inserto las bases de datos
bie1985_2004 <- read_xls("indicadores_bie80_03.xls", skip = 3)
names(bie1985_2004) <- nom_columnas #Se renombran las columnas
bie1985_2004 <- head(bie1985_2004, -10) # Se quitan las últimas 10 líneas que son bibliografía

bie2004_2023 <- read_xls("indicadores_bie.xls", skip = 3)
names(bie2004_2023) <- nom_columnas #Se renombran las columnas
bie2004_2023 <- head(bie2004_2023, -10)


# -------------------------- Tratamiento de la poblacion ---------------------

pob <- read_xlsx("Poblacion_01.xlsx", skip = 3)
pob <- head(pob, -18)
pob <- pob[-1,]
pob_tratada <- pob %>% 
  select(-"Grupo quinquenal de edad") %>% 
  t() %>% 
  as.data.frame()

poblacion <- pob_tratada %>% 
  rownames_to_column(., var = "Periodo")

poblacion_num <- poblacion %>% 
  mutate_all(as.numeric)

class(poblacion_num$Pob_Nacional)

crec_poblacional <- poblacion_num %>% 
  transmute_at(vars(!Periodo), ~((./lag(.)-1)/5), names = ".cr") %>% 
  mutate(Periodo = c(1995, 2000, 2005, 2010, 2020))
  
poblacion_crecimiento <- poblacion_num %>% 
  left_join(crec_poblacional, by = c("Periodo"), suffix = c("",".cr"))
  
  
col_nueva <- c("Pob_Nacional", "Pob_AGS", "Pob_BC", "Pob_BCS", "Pob_CAMP", "Pob_CHIS", "Pob_CHIH", 
               "Pob_COAH", "Pob_COL", "Pob_CDMX", "Pob_DGO", "Pob_GTO", "Pob_GRO", "Pob_HGO", 
               "Pob_JAL", "Pob_MEX", "Pob_MICH", "Pob_MOR", "Pob_NAY", "Pob_NL", 
               "Pob_OAX", "Pob_PUE", "Pob_QRO", "Pob_QR", "Pob_SLP", "Pob_SIN", 
               "Pob_SON", "Pob_TAB", "Pob_TAMPS", "Pob_TLAX", "Pob_VER", "Pob_YUC", 
               "Pob_ZAC")
colnames(pob_tratada) <- col_nueva
pob_tratada <- pob_tratada[-1,]


### ------- Fin trato poblacion -----

#Se hace backward fill para que las observaciones de PIB y de población se alineen
bie2004_2023_filled <- bie2004_2023 %>% 
  fill(Pob_total, Pob_ocupada, .direction = 'down') %>% 
  filter_at(vars(-Pob_total, -Pob_ocupada), all_vars(!is.na(.)))

#Se junta la base con todos los años, ya con nuevos nombres
bie85_23 <- rbind(bie1985_2004,bie2004_2023_filled)  %>% 
  mutate_all(as.numeric)


class(bie85_23$Periodo)

bie_poblacion <- bie85_23 %>% 
  left_join(poblacion_crecimiento, by = ("Periodo"), suffix = c("", ".pob"))

poblaciones_imputadas <- bie_poblacion %>% 
  filter(Periodo > 2004) %>% 
  mutate(Pob_Nacional = ifelse(is.na(Pob_Nacional), rollmean(Pob_Nacional, k = 5, fill = NA), Pob_Nacional))

#Se multiplican los valores de PIB para que representen pesos y no millones de pesos
bie_ajustada <- bie_poblacion %>%
  mutate_at(vars(PIB_Nacional:PIB_ZAC), list(~ . * 1000000)) 

bie_indicadores <- bie_ajustada %>% 
  mutate(PIB_capita = PIB_Nacional/Pob_total,
         PIB_trabajador = PIB_Nacional/Pob_ocupada,
         PIB_Nacional_pct = (PIB_Nacional / lag(PIB_Nacional) - 1) * 100,
         PIB_AGS_pct = (PIB_AGS / lag(PIB_AGS) - 1) * 100,
         PIB_BC_pct = (PIB_BC / lag(PIB_BC) - 1) * 100,
         PIB_BCS_pct = (PIB_BCS / lag(PIB_BCS) - 1) * 100,
         PIB_CAMP_pct = (PIB_CAMP / lag(PIB_CAMP) - 1) * 100,
         PIB_CHIS_pct = (PIB_CHIS / lag(PIB_CHIS) - 1) * 100,
         PIB_CHIH_pct = (PIB_CHIH / lag(PIB_CHIH) - 1) * 100,
         PIB_COAH_pct = (PIB_COAH / lag(PIB_COAH) - 1) * 100,
         PIB_COL_pct = (PIB_COL / lag(PIB_COL) - 1) * 100,
         PIB_CDMX_pct = (PIB_CDMX / lag(PIB_CDMX) - 1) * 100,
         PIB_DGO_pct = (PIB_DGO / lag(PIB_DGO) - 1) * 100,
         PIB_GTO_pct = (PIB_GTO / lag(PIB_GTO) - 1) * 100,
         PIB_GRO_pct = (PIB_GRO / lag(PIB_GRO) - 1) * 100,
         PIB_HGO_pct = (PIB_HGO / lag(PIB_HGO) - 1) * 100,
         PIB_JAL_pct = (PIB_JAL / lag(PIB_JAL) - 1) * 100,
         PIB_MEX_pct = (PIB_MEX / lag(PIB_MEX) - 1) * 100,
         PIB_MICH_pct = (PIB_MICH / lag(PIB_MICH) - 1) * 100,
         PIB_MOR_pct = (PIB_MOR / lag(PIB_MOR) - 1) * 100,
         PIB_NAY_pct = (PIB_NAY / lag(PIB_NAY) - 1) * 100,
         PIB_NL_pct = (PIB_NL / lag(PIB_NL) - 1) * 100,
         PIB_OAX_pct = (PIB_OAX / lag(PIB_OAX) - 1) * 100,
         PIB_PUE_pct = (PIB_PUE / lag(PIB_PUE) - 1) * 100,
         PIB_QRO_pct = (PIB_QRO / lag(PIB_QRO) - 1) * 100,
         PIB_QR_pct = (PIB_QR / lag(PIB_QR) - 1) * 100,
         PIB_SLP_pct = (PIB_SLP / lag(PIB_SLP) - 1) * 100,
         PIB_SIN_pct = (PIB_SIN / lag(PIB_SIN) - 1) * 100,
         PIB_SON_pct = (PIB_SON / lag(PIB_SON) - 1) * 100,
         PIB_TAB_pct = (PIB_TAB / lag(PIB_TAB) - 1) * 100,
         PIB_TAMPS_pct = (PIB_TAMPS / lag(PIB_TAMPS) - 1) * 100,
         PIB_TLAX_pct = (PIB_TLAX / lag(PIB_TLAX) - 1) * 100,
         PIB_VER_pct = (PIB_VER / lag(PIB_VER) - 1) * 100,
         PIB_YUC_pct = (PIB_YUC / lag(PIB_YUC) - 1) * 100,
         PIB_ZAC_pct = (PIB_ZAC / lag(PIB_ZAC) - 1) * 100,
         PIB_capita_pct = (PIB_capita / lag(PIB_capita) - 1) * 100,
         PIB_trabajador_pct = (PIB_trabajador / lag(PIB_trabajador) - 1) * 100
         )

indicadores_promedio <- bie_indicadores %>% 
  summarize(Nacional = mean(PIB_Nacional_pct, na.rm = TRUE),
            AGS = mean(PIB_AGS_pct, na.rm = TRUE),
            BC = mean(PIB_BC_pct, na.rm = TRUE),
            BCS = mean(PIB_BCS_pct, na.rm = TRUE),
            CAMP = mean(PIB_CAMP_pct, na.rm = TRUE),
            CHIS = mean(PIB_CHIS_pct, na.rm = TRUE),
            CHIH = mean(PIB_CHIH_pct, na.rm = TRUE),
            COAH = mean(PIB_COAH_pct, na.rm = TRUE),
            COL = mean(PIB_COL_pct, na.rm = TRUE),
            CDMX = mean(PIB_CDMX_pct, na.rm = TRUE),
            DGO = mean(PIB_DGO_pct, na.rm = TRUE),
            GTO = mean(PIB_GTO_pct, na.rm = TRUE),
            GRO = mean(PIB_GRO_pct, na.rm = TRUE),
            HGO = mean(PIB_HGO_pct, na.rm = TRUE),
            JAL = mean(PIB_JAL_pct, na.rm = TRUE),
            MEX = mean(PIB_MEX_pct, na.rm = TRUE),
            MICH = mean(PIB_MICH_pct, na.rm = TRUE),
            MOR = mean(PIB_MOR_pct, na.rm = TRUE),
            NAY = mean(PIB_NAY_pct, na.rm = TRUE),
            NL = mean(PIB_NL_pct, na.rm = TRUE),
            OAX = mean(PIB_OAX_pct, na.rm = TRUE),
            PUE = mean(PIB_PUE_pct, na.rm = TRUE),
            QRO = mean(PIB_QRO_pct, na.rm = TRUE),
            QR = mean(PIB_QR_pct, na.rm = TRUE),
            SLP = mean(PIB_SLP_pct, na.rm = TRUE),
            SIN = mean(PIB_SIN_pct, na.rm = TRUE),
            SON = mean(PIB_SON_pct, na.rm = TRUE),
            TAB = mean(PIB_TAB_pct, na.rm = TRUE),
            TAMPS = mean(PIB_TAMPS_pct, na.rm = TRUE),
            TLAX = mean(PIB_TLAX_pct, na.rm = TRUE),
            VER = mean(PIB_VER_pct, na.rm = TRUE),
            YUC = mean(PIB_YUC_pct, na.rm = TRUE),
            ZAC = mean(PIB_ZAC_pct, na.rm = TRUE)) %>% 
  t() %>% 
  as.data.frame() %>% 
  rename(crec_prom = V1) %>% 
  rownames_to_column(., var = "Estados")
#                          ---- Visualizaciones ----

#Variacion pib per cápita y Pib per trabajador
bie_indicadores %>% 
  filter(Periodo >= 2005) %>% 
  ggplot(data = ., aes(x = Periodo)) +
    #geom_line(aes(y = PIB_Nacional, group = 1), color = "green") +
    geom_line(aes(y = PIB_capita_pct, color = "PIB per capita", group = 1)) +
    geom_line(aes(y = PIB_trabajador_pct, color = "PIB por trabajador", group = 1)) +
    scale_color_manual(name = "Variable", values = c("red","blue")) +
    ylab("Variacion porcentual") +
    xlab("Año") +
    ggtitle("PIB per Capita y PIB por trabajador") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme(plot.title = element_text(hjust = 0.5))

# PIB pc & P pt absolutos
bie_indicadores %>% 
  filter(Periodo >= 2005) %>% 
  ggplot(data = ., aes(x = Periodo)) +
  #geom_line(aes(y = PIB_Nacional, group = 1), color = "green") +
  geom_line(aes(y = PIB_capita, color = "PIB per capita", group = 1)) +
  geom_line(aes(y = PIB_trabajador, color = "PIB por trabajador", group = 1)) +
  scale_color_manual(name = "Variable", values = c("red","blue")) +
  ylab("Pesos mexicanos") +
  xlab("Año") +
  ggtitle("PIB per Capita y PIB por trabajador") +
  theme(plot.title = element_text(hjust = 0.5))

###  -----  Visualizaciones estados -------

# Variacion del pib anual por estado y nacional, incluye todos los estados.
ggplotly(bie_indicadores %>% 
ggplot(aes(x = Periodo)) +
  geom_line(aes(y = PIB_Nacional_pct, color = 'Nacional', group = 1)) +
  geom_line(aes(y = PIB_AGS_pct, color = 'AGS', group = 1)) +
  geom_line(aes(y = PIB_BC_pct, color = 'BC', group = 1)) +
  geom_line(aes(y = PIB_BCS_pct, color = 'BCS', group = 1)) +
  geom_line(aes(y = PIB_CAMP_pct, color = 'CAMP', group = 1)) +
  geom_line(aes(y = PIB_CHIS_pct, color = 'CHIS', group = 1)) +
  geom_line(aes(y = PIB_CHIH_pct, color = 'CHIH', group = 1)) +
  geom_line(aes(y = PIB_COAH_pct, color = 'COAH', group = 1)) +
  geom_line(aes(y = PIB_COL_pct, color = 'COL', group = 1)) +
  geom_line(aes(y = PIB_CDMX_pct, color = 'CDMX', group = 1)) +
  geom_line(aes(y = PIB_DGO_pct, color = 'DGO', group = 1)) +
  geom_line(aes(y = PIB_GTO_pct, color = 'GTO', group = 1)) +
  geom_line(aes(y = PIB_GRO_pct, color = 'GRO', group = 1)) +
  geom_line(aes(y = PIB_HGO_pct, color = 'HGO', group = 1)) +
  geom_line(aes(y = PIB_JAL_pct, color = 'JAL', group = 1)) +
  geom_line(aes(y = PIB_MEX_pct, color = 'MEX', group = 1)) +
  geom_line(aes(y = PIB_MICH_pct, color = 'MICH', group = 1)) +
  geom_line(aes(y = PIB_MOR_pct, color = 'MOR', group = 1)) +
  geom_line(aes(y = PIB_NAY_pct, color = 'NAY', group = 1)) +
  geom_line(aes(y = PIB_NL_pct, color = 'NL', group = 1)) +
  geom_line(aes(y = PIB_OAX_pct, color = 'OAX', group = 1)) +
  geom_line(aes(y = PIB_PUE_pct, color = 'PUE', group = 1)) +
  geom_line(aes(y = PIB_QRO_pct, color = 'QRO', group = 1)) +
  geom_line(aes(y = PIB_QR_pct, color = 'QR', group = 1)) +
  geom_line(aes(y = PIB_SLP_pct, color = 'SLP', group = 1)) +
  geom_line(aes(y = PIB_SIN_pct, color = 'SIN', group = 1)) +
  geom_line(aes(y = PIB_SON_pct, color = 'SON', group = 1)) +
  geom_line(aes(y = PIB_TAB_pct, color = 'TAB', group = 1)) +
  geom_line(aes(y = PIB_TAMPS_pct, color = 'TAMPS', group = 1)) +
  geom_line(aes(y = PIB_TLAX_pct, color = 'TLAX', group = 1)) +
  geom_line(aes(y = PIB_VER_pct, color = 'VER', group = 1)) +
  geom_line(aes(y = PIB_YUC_pct, color = 'YUC', group = 1)) +
  geom_line(aes(y = PIB_ZAC_pct, color = 'ZAC', group = 1)) +
  scale_color_manual(name = "Estados",values = as.vector(polychrome(33)))+
  ylab("Variacion porcentual") +
  xlab("Año") +
  ggtitle("Variacion del PIB por estado") +
  theme(plot.title = element_text(hjust = 0.5)))

bie_indicadores %>% 
  select_if(~ any(. > 10 | . < -10)) %>% 
  ggplot(aes(x = Periodo)) +
  geom_line(aes(y = PIB_AGS_pct, color = 'AGS', group = 1)) +
  geom_line(aes(y = PIB_BC_pct, color = 'BC', group = 2)) +
  geom_line(aes(y = PIB_BCS_pct, color = 'BCS', group = 1)) +
  geom_line(aes(y = PIB_CAMP_pct, color = 'CAMP', group = 5)) +
  geom_line(aes(y = PIB_CHIS_pct, color = 'CHIS', group = 1)) +
  geom_line(aes(y = PIB_COL_pct, color = 'COL', group = 1)) +
  geom_line(aes(y = PIB_CDMX_pct, color = 'CDMX', group = 1)) +
  geom_line(aes(y = PIB_HGO_pct, color = 'HGO', group = 1)) +
  geom_line(aes(y = PIB_MICH_pct, color = 'MICH', group = 1)) +
  geom_line(aes(y = PIB_MOR_pct, color = 'MOR', group = 10)) +
  geom_line(aes(y = PIB_NAY_pct, color = 'NAY', group = 1)) +
  geom_line(aes(y = PIB_NL_pct, color = 'NL', group = 1)) +
  geom_line(aes(y = PIB_PUE_pct, color = 'PUE', group = 1)) +
  geom_line(aes(y = PIB_QRO_pct, color = 'QRO', group = 1)) +
  geom_line(aes(y = PIB_QR_pct, color = 'QR', group = 1)) +
  geom_line(aes(y = PIB_SLP_pct, color = 'SLP', group = 1)) +
  geom_line(aes(y = PIB_SON_pct, color = 'SON', group = 1)) +
  geom_line(aes(y = PIB_TLAX_pct, color = 'TLAX', group = 1)) +
  geom_line(aes(y = PIB_ZAC_pct, color = 'ZAC', group = 1)) +
  scale_color_manual(name = "Estados", values = as.vector(polychrome(19)))+
  ylab("Variacion porcentual") +
  xlab("Año") +
  ggtitle("Variacion del PIB por estado") +
  theme(plot.title = element_text(hjust = 0.5))

# Crecimiento nacional y estatal promedio
indicadores_promedio %>% 
  mutate(high_nacional = ifelse(Estados == "Nacional", T,F)) %>% 
ggplot() +
  geom_col(aes(x = forcats::fct_reorder(Estados, desc(crec_prom)), y = crec_prom, fill = high_nacional)) +
  ggtitle("Crecimiento promedio de los estados de México de 1980 - 2023") +
  xlab("Estados de México") +
  ylab("Crecimiento promedio porcentual") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')

# ---------------------- Capital ---------

nom_columnas <- c("Periodo", "fcb_Nacional", "fcb_AGS", "fcb_BC", "fcb_BCS", "fcb_CAMP", "fcb_CHIS", "fcb_CHIH", 
                  "fcb_COAH", "fcb_COL", "fcb_CDMX", "fcb_DGO", "fcb_GTO", "fcb_GRO", "fcb_HGO", 
                  "fcb_JAL", "fcb_MEX", "fcb_MICH", "fcb_MOR", "fcb_NAY", "fcb_NL", 
                  "fcb_OAX", "fcb_PUE", "fcb_QRO", "fcb_QR", "fcb_SLP", "fcb_SIN", 
                  "fcb_SON", "fcb_TAB", "fcb_TAMPS", "fcb_TLAX", "fcb_VER", "fcb_YUC", 
                  "fcb_ZAC")

fcb <- read_xls("fcb.xls", skip = 3)
names(fcb) <- nom_columnas
fcb <- head(fcb, -5)

#Se expanden a millones de pesos
fcb_exp <- fcb %>% 
  mutate_at(vars(fcb_Nacional:fcb_ZAC), list(~ . * 1000000))

depreciation <- function(x1, dep) {
  inv = x1 - (1 - dep)*lag(x1)
  return(inv)
}

#Dinámica de la inversión en méxico
fcb_inv <- fcb_exp %>%
  mutate_at(vars(contains('fcb')), list(~depreciation(., 0.02)), .names = ".inv")

ggplotly(fcb_inv %>% 
  pivot_longer(cols = starts_with("fcb")) %>% 
  filter(!is.na(value)) %>% 
  ggplot()   +
  geom_line(aes(x = Periodo, y = value, group = name, color = gsub("^fcb_", "", name))) +
  scale_color_manual(name = "Estados", values = as.vector(polychrome(33))))
  
write.csv(fcb_exp, "capital.csv")


#### ------- Tablas para la Actividad --------



