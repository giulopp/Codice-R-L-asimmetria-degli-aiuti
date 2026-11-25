# ------------------------------------------------------------
# L'asimmetria degli aiuti: un'indagine sull'allocazione delle
# risorse tra global cluster OCHA
# Tesi magistrale - Università Cattolica del Sacro Cuore
# Autrice: Giulia Lopes - Relatore: prof. Vincenzo Nardelli
# Dati: OCHA FTS, Global - Requirements and Funding Data (HDX),
#       scaricati il 9 settembre 2026
# ------------------------------------------------------------


# install.packages(c("dplyr", "fixest", "ggplot2", "maps"))   # solo la prima volta
library(dplyr)
library(fixest)
library(ggplot2)


# Dati OCHA FTS (HDX): una riga = piano x anno x cluster
d <- read.csv("fts_requirements_funding_globalcluster_global.csv")

# Controllo: deve dare 10679 12
dim(d)

d <- d %>%
  # Raccordo agli 8 cluster della riforma IASC 2026 (gli altri diventano NA)
  mutate(cluster8 = case_when(
    cluster %in% c("Health") ~ "Health",
    cluster %in% c("Food Security", "Agriculture") ~ "Food Security",
    cluster %in% c("Water Sanitation Hygiene") ~ "WASH",
    cluster %in% c("Nutrition") ~ "Nutrition",
    cluster %in% c("Education") ~ "Education",
    cluster %in% c("Emergency Shelter and NFI", "Camp Coordination / Management",
                   "Protection - Housing, Land and Property") ~ "Shelter",
    cluster %in% c("Protection", "Protection - Child Protection",
                   "Protection - Gender-Based Violence", "Protection - Mine Action",
                   "Protection - Human Trafficking & Smuggling") ~ "Protection",
    cluster %in% c("Logistics", "Emergency Telecommunications") ~ "Logistics"
  )) %>%
  # Pulizia
  mutate(funding = ifelse(is.na(funding), 0, funding)) %>%
  filter(!is.na(cluster8), year <= 2025, requirements > 0) %>%
  # Una riga = piano x cluster8
  group_by(id, countryCode, year, cluster8) %>%
  summarise(requirements = sum(requirements), funding = sum(funding), .groups = "drop") %>%
  # Copertura del fabbisogno
  mutate(copertura = funding / requirements)

# Un colore fisso per ogni cluster, usato in tutti i grafici
colori <- c("Health" = "#E31A1C",
            "Food Security" = "#E6AB02",
            "WASH" = "#1F78B4",
            "Nutrition" = "#66A61E",
            "Education" = "#E7298A",
            "Shelter" = "#A6761D",
            "Protection" = "#7570B3",
            "Logistics" = "#D95F02")


# ---- Grafici descrittivi ----

# piani di risposta per anno
piani_desc <- read.csv("fts_requirements_funding_globalcluster_global.csv") %>%
  distinct(code, .keep_all = TRUE)                               

ggplot(count(piani_desc, year), aes(x = year, y = n)) +          
  geom_col(fill = "steelblue") +                                 
  scale_x_continuous(breaks = 2000:2026) +                       
  labs(x = "Anno", y = "Numero di piani") +                      
  theme(axis.text.x = element_text(angle = 45, hjust = 1))       

# Fabbisogno e finanziamento totali per cluster
totali <- d %>%
  group_by(cluster8) %>%   # raggruppa per cluster
  summarise(requirements = sum(requirements) / 1e9, # somma il fabbisogno e lo converte in miliardi
            funding = sum(funding) / 1e9)     # somma i fondi ricevuti, in miliardi

ggplot(totali, aes(x = reorder(cluster8, requirements), fill = cluster8)) +  # cluster ordinati per fabbisogno
  geom_col(aes(y = requirements), alpha = 0.3) +  # barra chiara (trasparente al 30%) = fabbisogno
  geom_col(aes(y = funding)) +    # barra piena sovrapposta = finanziato
  coord_flip() +                    # barre orizzontali: nomi dei cluster più leggibili
  scale_fill_manual(values = colori, guide = "none") +  # usa i colori definiti in "colori", senza legenda
  labs(x = NULL,      # nessun titolo sull'asse dei cluster
       y = "Miliardi di USD (chiaro = fabbisogno, scuro = finanziato)", # titolo dell'asse dei valori
       title = "Fabbisogno e finanziamento per cluster, 2000-2025")   # titolo del grafico

totali %>%
  mutate(scoperto = requirements - funding,          # fabbisogno non coperto, in miliardi
         copertura = funding / requirements) %>%     # quota del fabbisogno coperta
  arrange(desc(requirements))     
mutate(totali,
       quota_fabbisogno = round(100 * requirements / sum(requirements), 2),   # % sul fabbisogno totale
       quota_fondi = round(100 * funding / sum(funding), 2))                  # % sul totale dei fondi

# 1. Per ogni cluster e anno: quanti piani e quanto pesa il piano più grande
concentrazione <- summarise(group_by(d, cluster8, year),
                            piani = n(),                                        # piani del cluster in quell'anno
                            quota_max = max(requirements) / sum(requirements),  # peso del piano più grande
                            .groups = "drop")

# 2. Media sugli anni, per cluster
summarise(group_by(concentrazione, cluster8),
          piani_medi = round(mean(piani), 1),                   # numero medio di piani all'anno
          quota_max_media = round(100 * mean(quota_max), 1))    # peso medio del piano più grande, in %

# Copertura aggregata per anno
per_anno <- d %>%   # parte da d e salva il risultato in "per_anno"
  group_by(cluster8, year) %>%    # crea un gruppo per ogni combinazione cluster-anno
  summarise(copertura = sum(funding) / sum(requirements),  # fondi totali del gruppo / fabbisogno totale del gruppo
            .groups = "drop")  # alla fine toglie il raggruppamento

ggplot(per_anno, aes(x = year, y = copertura, colour = cluster8)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE,                   
              linetype = "dashed", linewidth = 0.7) + 
  facet_wrap(~ cluster8, ncol = 4) +
  scale_colour_manual(values = colori, guide = "none") +
  scale_x_continuous(breaks = c(2000, 2010, 2020)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Anno", y = "Copertura del fabbisogno",
       title = "Copertura aggregata per cluster e anno")

# Distribuzione della copertura nei piani (tetto 100%)
ggplot(d, aes(x = reorder(cluster8, copertura, median), y = pmin(copertura, 1), fill = cluster8)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_manual(values = colori, guide = "none") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Copertura del fabbisogno",
       title = "Copertura per cluster nei singoli piani")

summarise(group_by(d, cluster8),                     # d scritto dentro, come in ggplot(d, ...)
          q25 = quantile(copertura, 0.25),           # bordo sinistro della scatola
          mediana = median(copertura),               # linea dentro la scatola
          q75 = quantile(copertura, 0.75))           # bordo destro della scatola

# ---- Modello ----

# Tolgo le celle a zero (log(0) non esiste) e metto Health come riferimento
d <- d %>%
  filter(copertura > 0) %>%   # tiene solo le righe con copertura maggiore di zero
  mutate(cluster8 = relevel(factor(cluster8), ref = "Health"))   # trasforma cluster8 in fattore, con Health come categoria di riferimento

# Crisi lunghe: paesi con piani in almeno 15 anni.
# La definisco UNA volta sola qui e la uso sempre (modelli e grafici),
# cosi' i paesi "lunghi" sono gli stessi ovunque
lunghe <- d %>%
  summarise(anni = n_distinct(year), .by = countryCode) %>%   # anni con almeno un piano, per paese
  filter(anni >= 15) %>%
  pull(countryCode)                                           # vettore con i codici dei paesi
lunghe   # 16 paesi

# Periodo e tipo di crisi come colonne di d (servono per i modelli con interazione)
d <- d %>%
  mutate(periodo = factor(case_when(year <= 2013 ~ "2000-2013",
                                    year <= 2019 ~ "2014-2019",
                                    TRUE         ~ "2020-2025")),
         lunga = countryCode %in% lunghe)   # TRUE se il paese e' una crisi lunga

# Modello con effetti fissi di piano (| id)
# come lm(log(copertura) ~ cluster8 + factor(id)), ma mostra solo i cluster
m <- feols(log(copertura) ~ cluster8 | id, data = d, cluster = ~countryCode)
summary(m)

# Differenza % di copertura rispetto a Health, a parita' di piano (con IC 95%)
effetti <- data.frame(cluster = sub("cluster8", "", names(coef(m))),
                      effetto = 100 * (exp(coef(m)) - 1),
                      basso = 100 * (exp(confint(m)[, 1]) - 1),
                      alto = 100 * (exp(confint(m)[, 2]) - 1),
                      row.names = NULL)
effetti

ggplot(effetti, aes(x = reorder(cluster, effetto), y = effetto, colour = cluster)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = basso, ymax = alto)) +
  coord_flip() +
  scale_colour_manual(values = colori, guide = "none") +
  labs(x = NULL, y = "Differenza % di copertura rispetto a Health",
       title = "Copertura dei cluster a parità di crisi (IC 95%)")

# Effetto fisso di ogni piano (α) per paese e anno
# exp(α) = copertura attesa di Health in quel piano
α <- fixef(m)$id

piani <- d %>%
  distinct(id, countryCode, year) %>%
  mutate(α = α[as.character(id)]) %>%
  filter(!is.na(α)) %>%
  group_by(countryCode, year) %>%          # se un paese ha piu' piani nello stesso anno, faccio la media
  summarise(α = mean(α), .groups = "drop") %>%
  mutate(health = pmin(exp(α), 1),      # copertura attesa di Health (tetto 100%)
         paese = maps::iso3166$ISOname[match(countryCode, maps::iso3166$a3)])
View(piani) 
# Andamento nel tempo: ogni punto e' un paese, la linea rossa e' la mediana
ggplot(piani, aes(x = year, y = health)) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  stat_summary(fun = median, geom = "line", colour = "red", linewidth = 1) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Anno", y = "livello di finanziamento della crisi",
       title = "Quanto sono finanziate le crisi, anno per anno")

# Paesi con piani in almeno 15 anni
ggplot(filter(piani, countryCode %in% lunghe), aes(x = year, y = health)) +
  geom_line() +
  geom_point(size = 0.8) +
  facet_wrap(~ paese) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Anno", y = "livello di finanziamento della crisi",
       title = "Crisi di lungo periodo")

# Mappe per anno non usate nella tesi
mondo <- map_data("world") %>%
  mutate(countryCode = maps::iso3166$a3[match(region, maps::iso3166$mapname)])
mappa <- inner_join(mondo, piani, by = "countryCode", relationship = "many-to-many") %>%
  filter(countryCode != "RUS")   # il piano riguarda solo la Cecenia: colorare tutta la Russia inganna

ggplot() +
  geom_polygon(data = mondo, aes(x = long, y = lat, group = group), fill = "grey90") +
  geom_polygon(data = mappa, aes(x = long, y = lat, group = group, fill = health)) +
  facet_wrap(~ year, ncol = 5) +
  coord_quickmap(xlim = c(-100, 150), ylim = c(-35, 55)) +
  scale_fill_viridis_c(labels = scales::percent, limits = c(0, 1), name = "exp(α)") +
  labs(title = "Copertura attesa di Health per paese e anno") +
  theme_void()
ggsave("mappe_alfa.png", width = 15, height = 9, dpi = 300, bg = "white")   # troppo grande per il pannello Plots




# ---modello per periodi----

# 2000-2013: appelli consolidati (CAP)
m_p1 <- feols(log(copertura) ~ cluster8 | id, data = filter(d, year <= 2013),cluster = ~countryCode)

# 2014-2019: Humanitarian Programme Cycle
m_p2 <- feols(log(copertura) ~ cluster8 | id, data = filter(d, year >= 2014, year <= 2019), cluster = ~countryCode)

# 2020-2025: COVID e tagli
m_p3 <- feols(log(copertura) ~ cluster8 | id, data = filter(d, year >= 2020), cluster = ~countryCode)

# Tabella dei coefficienti a tre colonne
etable(m, m_p1, m_p2, m_p3, headers = c("2000-2025","2000-2013", "2014-2019", "2020-2025"))

# Differenza % di copertura rispetto a Health, a parita' di piano (con IC 95%)
eff_p1 <- data.frame(periodo = "2000-2013",
                     cluster = sub("cluster8", "", names(coef(m_p1))),
                     effetto = 100 * (exp(coef(m_p1)) - 1),
                     basso   = 100 * (exp(confint(m_p1)[, 1]) - 1),
                     alto    = 100 * (exp(confint(m_p1)[, 2]) - 1),
                     row.names = NULL)

eff_p2 <- data.frame(periodo = "2014-2019",
                     cluster = sub("cluster8", "", names(coef(m_p2))),
                     effetto = 100 * (exp(coef(m_p2)) - 1),
                     basso   = 100 * (exp(confint(m_p2)[, 1]) - 1),
                     alto    = 100 * (exp(confint(m_p2)[, 2]) - 1),
                     row.names = NULL)

eff_p3 <- data.frame(periodo = "2020-2025",
                     cluster = sub("cluster8", "", names(coef(m_p3))),
                     effetto = 100 * (exp(coef(m_p3)) - 1),
                     basso   = 100 * (exp(confint(m_p3)[, 1]) - 1),
                     alto    = 100 * (exp(confint(m_p3)[, 2]) - 1),
                     row.names = NULL)

#oppure potrei fare uno alla volta 
effetti_periodo <- rbind(eff_p1, eff_p2, eff_p3)  
effetti_periodo

ggplot(effetti_periodo,
       aes(x = reorder(cluster, effetto), y = effetto, colour = cluster)) +   
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = basso, ymax = alto)) +
  coord_flip() +
  facet_wrap(~ periodo, ncol = 3) +
  scale_colour_manual(values = colori, guide = "none") +
  scale_y_continuous(breaks = c(-50, 0, 50)) +
  labs(x = NULL, y = "Differenza % di copertura rispetto a Health",
       title = "Copertura dei cluster a parità di crisi, per periodo (IC 95%)")

# I tre modelli separati danno i coefficienti di ogni periodo, ma non dicono
# se le differenze TRA periodi sono significative. Per questo stimo un unico
# modello con le interazioni cluster x periodo.
# - cluster8: scarto di ogni cluster rispetto a Health nel 2000-2013
# - i(periodo, cluster8, ...): di quanto cambia quello scarto nel 2014-2019
#   e nel 2020-2025 rispetto al 2000-2013 (0 = nessun cambiamento)
m_int_p <- feols(log(copertura) ~ cluster8 + i(periodo, cluster8, ref = "2000-2013", ref2 = "Health") | id,
                 data = d, cluster = ~countryCode)
summary(m_int_p)

# Test di Wald: H0 = tutte le interazioni sono zero,
# cioe' gli scarti tra cluster sono gli stessi nei tre periodi.
# p-value piccolo (< 0,05) -> gli scarti cambiano nel tempo
wald(m_int_p, keep = "periodo")

# Mappe per anno
piani_periodo <- piani %>%
  mutate(periodo = case_when(year <= 2013 ~ "2000-2013",year <= 2019 ~ "2014-2019",TRUE         ~ "2020-2025")) %>%
  group_by(countryCode, periodo) %>%                          
  summarise(health = median(health),anni = n(), .groups = "drop")                     

mappa_periodo <- inner_join(mondo, piani_periodo, by = "countryCode", relationship = "many-to-many") %>%
  filter(countryCode != "RUS")                                

ggplot() +
  geom_polygon(data = mondo, aes(x = long, y = lat, group = group), fill = "grey90") +
  geom_polygon(data = mappa_periodo, aes(x = long, y = lat, group = group, fill = health)) +
  facet_wrap(~ periodo, ncol = 3) +
  coord_quickmap(xlim = c(-100, 150), ylim = c(-35, 55)) +
  scale_fill_viridis_c(labels = scales::percent, limits = c(0, 1), name = "exp(α)") +
  labs() +
  theme_void()
ggsave("mappe_alfa_periodo.png", width = 15, height = 6, dpi = 300, bg = "white")

# ---- modello per crisi brevi e lunghe ----

# Crisi lunghe: paesi nel vettore "lunghe" (definito sopra)
# Attenzione: sono solo 16 paesi, quindi gli errori standard clusterizzati
# per paese sono poco precisi (da riportare tra i limiti)
m_lunga <- feols(log(copertura) ~ cluster8 | id,
                 data = filter(d, lunga),
                 cluster = ~countryCode)
summary(m_lunga)

# Crisi brevi: tutti gli altri paesi
m_breve <- feols(log(copertura) ~ cluster8 | id,
                 data = filter(d, !lunga),
                 cluster = ~countryCode)
summary(m_breve)

# Differenza % di copertura rispetto a Health, a parita' di piano (con IC 95%)
effetti_lunga <- data.frame(gruppo = "Crisi lunghe",
                            cluster = sub("cluster8", "", names(coef(m_lunga))),
                            effetto = 100 * (exp(coef(m_lunga)) - 1),
                            basso = 100 * (exp(confint(m_lunga)[, 1]) - 1),
                            alto = 100 * (exp(confint(m_lunga)[, 2]) - 1),
                            row.names = NULL)

effetti_breve <- data.frame(gruppo = "Crisi brevi",
                            cluster = sub("cluster8", "", names(coef(m_breve))),
                            effetto = 100 * (exp(coef(m_breve)) - 1),
                            basso = 100 * (exp(confint(m_breve)[, 1]) - 1),
                            alto = 100 * (exp(confint(m_breve)[, 2]) - 1),
                            row.names = NULL)

effetti_crisi <- rbind(effetti_breve, effetti_lunga)
effetti_crisi

ggplot(effetti_crisi, aes(x = reorder(cluster, effetto), y = effetto, colour = cluster)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = basso, ymax = alto)) +
  coord_flip() +
  facet_wrap(~ gruppo) +
  scale_colour_manual(values = colori, guide = "none") +
  labs(x = NULL, y = "Differenza % di copertura rispetto a Health")

# Come per i periodi: un unico modello con interazione cluster x crisi lunga.
# i(lunga, cluster8, ...) = di quanto cambia lo scarto rispetto a Health
# nelle crisi lunghe rispetto alle brevi (0 = nessuna differenza)
m_int_l <- feols(log(copertura) ~ cluster8 + i(lunga, cluster8, ref = FALSE, ref2 = "Health") | id,
                 data = d, cluster = ~countryCode)
summary(m_int_l)

# Test di Wald: H0 = gli scarti tra cluster sono uguali nelle crisi lunghe e brevi
wald(m_int_l, keep = "lunga")

# Nomi in italiano dei 16 paesi con crisi lunga
nomi_it <- c(AFG = "Afghanistan", BFA = "Burkina Faso", BDI = "Burundi",
             CAF = "Rep. Centrafricana", TCD = "Ciad", COD = "Rep. Dem. del Congo",
             HTI = "Haiti", KEN = "Kenya", NER = "Niger", PSE = "Palestina",
             SOM = "Somalia", SSD = "Sud Sudan", SDN = "Sudan", SYR = "Siria",
             YEM = "Yemen", ZWE = "Zimbabwe")

# Paesi con piani in almeno 15 anni
mediana_anno <- piani %>%
  group_by(year) %>%
  summarise(mediana = median(health))

ggplot(mutate(filter(piani, countryCode %in% lunghe), paese = nomi_it[countryCode]), aes(x = year, y = health)) +
  geom_vline(xintercept = c(2013.5, 2019.5),
             linetype = "dotted", colour = "purple") +     # aggiunta: cambi di periodo (2014 e 2020)
  geom_line(data = mediana_anno, aes(y = mediana),
            colour = "red", linetype = "dashed") +          # aggiunta: mediana di tutte le crisi
  geom_line() +
  geom_point(size = 0.8) +
  facet_wrap(~ paese) +
  scale_x_continuous(breaks = c(2000, 2010, 2020)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Anno", y = "Livello di finanziamento della crisi")
