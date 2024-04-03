# Script APS 1 Comércio Internacional | Grupo 5
# Membros: João Casella, Paloma Ary, Sofia Barbuzza, Valentina Guida,
# Victoria Saraiva

# Questoes 1 a 5
#===============================================================================

# Importando bibliotecas

#install.packages("readr")
library(readr)
#install.packages("haven")
library(haven)
#install.packages("dplyr")
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("plotly")
library(plotly)
#install.packages("knitr")
library(knitr)
#install.packages("tidyr")
library(tidyr)
#install.packages("readxl")
library(readxl)
#install.packages("maps")
library(maps)
#install.packages("ggmap")
library(ggmap)
#install.packages("mapdata")
library(mapdata)
#install.packages("sf")
library(sf)




#VANTAGENS COMPARATIVAS ========================================================

# a)

# Abrindo a base ITPD-E, lendo o arquivo .dta:
itpde <- read_dta("C:/Users/jcase/OneDrive/João/Insper/6º semestre/Comércio Internacional/APS/APS1/com_int_aps1_dados/itpd.dta")

#Filtrar os dados de comercio internacional:
#Apenas incluir exportacoes, ou seja, onde o pais exportador e importador sao diferentes
x_ind <- itpde %>%
  filter(exporter_iso3 != importer_iso3)

# Agrupar e resumir os dados por ano, pais exportador e industria + calcular exportacoes totais em bilhoes

x_ind <- x_ind %>%
  group_by(year, exporter_iso3, industry_id) %>%
  summarize(total_export = sum(trade) / 1e9) # Dividir por 1 bilhão para obter bilhões de dólares

# Criar um dataframe com o mapeamento dos IDs das indústrias para as descrições
mapeamento_industrias <- data.frame(
  ID = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
         21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
         41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
         61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80,
         81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100,
         101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116,
         117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132,
         133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148,
         149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164,
         165, 166, 167, 168, 169, 170),
  Industry_Description = c(
    "Wheat", "Rice (raw)", "Corn", "Other cereals", "Cereal products", "Soybeans",
    "Other oilseeds (excluding peanuts)", "Animal feed ingredients and pet foods",
    "Raw and refined sugar and sugar crops", "Other sweeteners",
    "Pulses and legumes, dried, preserved", "Fresh fruit", "Fresh vegetables",
    "Prepared fruits and fruit juices", "Prepared vegetables", "Nuts",
    "Live Cattle", "Live Swine", "Eggs", "Other meats, livestock products, and live animals",
    "Cocoa and cocoa products", "Beverages, nec", "Cotton",
    "Tobacco leaves and cigarettes", "Spices", "Other agricultural products, nec",
    "Mining of hard coal", "Mining of lignite",
    "Extraction crude petroleum and natural gas", "Mining of iron ores",
    "Other mining and quarring", "Electricity production, collection, and distribution",
    "Gas production and distribution", "Processing/preserving of meat",
    "Processing/preserving of fish", "Processing/preserving of fruit & vegetables",
    "Vegetable and animal oils and fats", "Dairy products", "Grain mill products",
    "Starches and starch products", "Prepared animal feeds", "Bakery products", "Sugar",
    "Cocoa chocolate and sugar confectionery", "Macaroni noodles & similar products",
    "Other food products n.e.c.", "Distilling rectifying & blending of spirits",
    "Wines", "Malt liquors and malt", "Soft drinks; mineral waters", "Tobacco products",
    "Textile fibre preparation; textile weaving", "Made-up textile articles except apparel",
    "Carpets and rugs", "Cordage rope twine and netting", "Other textiles n.e.c.",
    "Knitted and crocheted fabrics and articles", "Wearing apparel except fur apparel",
    "Dressing & dyeing of fur; processing of fur", "Tanning and dressing of leather",
    "Luggage handbags etc.; saddlery & harness", "Footwear", "Sawmilling and planing of wood",
    "Veneer sheets plywood particle board etc.", "Builders' carpentry and joinery",
    "Wooden containers", "Other wood products; articles of cork/straw",
    "Pulp paper and paperboard", "Corrugated paper and paperboard",
    "Other articles of paper and paperboard", "Publishing of books and other publications",
    "Publishing of newspapers journals etc.", "Publishing of recorded media", "Other publishing",
    "Printing", "Service activities related to printing", "Reproduction of recorded media",
    "Coke oven products", "Refined petroleum products", "Processing of nuclear fuel",
    "Basic chemicals except fertilizers", "Fertilizers and nitrogen compounds",
    "Plastics in primary forms; synthetic rubber", "Pesticides and other agro-chemical products",
    "Paints varnishes printing ink and mastics", "Pharmaceuticals medicinal chemicals etc.",
    "Soap cleaning & cosmetic preparations", "Other chemical products n.e.c.",
    "Man-made fibres", "Rubber tyres and tubes", "Other rubber products", "Plastic products",
    "Glass and glass products", "Pottery china and earthenware", "Refractory ceramic products",
    "Struct.non-refractory clay; ceramic products", "Cement lime and plaster",
    "Articles of concrete cement and plaster", "Cutting shaping & finishing of stone",
    "Other non-metallic mineral products n.e.c.", "Basic iron and steel",
    "Basic precious and non-ferrous metals", "Casting of iron and steel",
    "Structural metal products", "Tanks reservoirs and containers of metal", "Steam generators",
    "Cutlery hand tools and general hardware", "Other fabricated metal products n.e.c.",
    "Engines & turbines (not for transport equipment)", "Pumps compressors taps and valves",
    "Bearings gears gearing & driving elements", "Ovens furnaces and furnace burners",
    "Lifting and handling equipment", "Other general purpose machinery",
    "Agricultural and forestry machinery", "Machine tools", "Machinery for metallurgy",
    "Machinery for mining & construction", "Food/beverage/tobacco processing machinery",
    "Machinery for textile apparel and leather", "Weapons and ammunition",
    "Other special purpose machinery", "Domestic appliances n.e.c.",
    "Office accounting and computing machinery", "Electric motors generators and transformers",
    "Electricity distribution & control apparatus", "Insulated wire and cable",
    "Accumulators primary cells and batteries", "Lighting equipment and electric lamps",
    "Other electrical equipment n.e.c.", "Electronic valves tubes etc.",
    "TV/radio transmitters; line comm. apparatus", "TV and radio receivers and associated goods",
    "Medical surgical and orthopaedic equipment",
    "Measuring/testing/navigating appliances etc.",
    "Optical instruments & photographic equipment", "Watches and clocks", "Motor vehicles",
    "Automobile bodies trailers & semi-trailers", "Parts/accessories for automobiles",
    "Building and repairing of ships", "Building/repairing of pleasure/sport. boats",
    "Railway/tramway locomotives & rolling stock", "Aircraft and spacecraft", "Motorcycles",
    "Bicycles and invalid carriages", "Other transport equipment n.e.c.", "Furniture",
    "Jewellery and related articles", "Musical instruments", "Sports goods", "Games and toys",
    "Other manufacturing n.e.c.", "Manufacturing services on physical inputs owned by others",
    "Maintenance and repair services n.i.e.", "Transport", "Travel", "Construction",
    "Insurance and pension services", "Financial services",
    "Charges for the use of intellectual property n.i.e.",
    "Telecommunications, computer, and information services", "Other business services",
    "Heritage and recreational services", "Health services", "Education services",
    "Government goods and services n.i.e.", "Services not allocated", "Trade-related services",
    "Other personal services"
  )
)


# Substituir IDs de indústrias por descrições nas tabelas
x_ind <- x_ind %>%
  left_join(mapeamento_industrias, by = c("industry_id" = "ID")) %>%
  mutate(industry_description = Industry_Description) %>%
  select(-Industry_Description)


# b)

# Agrupando e manipulando dados
dados_agrupados <- x_ind %>%
  group_by(year, exporter_iso3, industry_description) %>% # agrupar dados por ano, pais exportador e industria
  summarize(total_export = sum(total_export)) %>% # calcular total de exportacoes por industria para cada combinacao de ano, pais exportador e industria
  group_by(year, exporter_iso3) %>% # agrupar dados novamente, agora apenas por ano e pais exportador
  summarize(total_export_country = sum(total_export)) # calcular total de exportacoes do pais exportador no ano em questao, somando as exportacoes totais de todas as industrias

# Calculando a vantagem absoluta
vantagem_absoluta <- left_join(dados_agrupados, x_ind, by = c("year", "exporter_iso3")) %>% # unindo resultados das etapas anteriores
  mutate(vantagem_absoluta = total_export / total_export_country) # calculo da medida de vantagem absoluta

# c)

# Ex_ijt: exportacao total do pais i na industria j no ano t
# Ex_it : exportacao total do pais i no ano t
# Ex_jt : exportacao total da industria j no ano t
# Ex_t  : exportacao total no ano t

# RCA_ijt = (Ex_ijt/Ex_it)/(Ex_jt/Ex_t)


# Converter a coluna total_export para numérica nas tabelas originais, se necessário
x_ind$total_export <- as.numeric(as.character(x_ind$total_export))

# Calcular Ex_ijt (exportações por país, indústria e ano)
Ex_ijt <- x_ind %>%
  group_by(exporter_iso3, industry_description, year) %>%
  summarize(Ex_ijt = sum(total_export, na.rm = TRUE))


# Calcular Ex_jt (exportações por indústria e ano)
Ex_jt <- x_ind %>%
  group_by(industry_description, year) %>%
  summarize(Ex_jt = sum(total_export, na.rm = TRUE))

# Calcular Ex_t (exportações totais por ano)
Ex_t <- x_ind %>%
  group_by(year) %>%
  summarize(Ex_t = sum(total_export, na.rm = TRUE))


# Calcular Ex_it (exportações por país e ano)
Ex_it <- x_ind %>%
  group_by(exporter_iso3, year) %>%
  summarize(total_export_country = sum(total_export, na.rm = TRUE))

# Realizar as junções e calcular o RCA_ijt
vantagem_comparativa <- x_ind %>%
  left_join(Ex_ijt, by = c("exporter_iso3", "industry_description", "year")) %>%
  left_join(Ex_jt, by = c("industry_description", "year")) %>%
  left_join(Ex_t, by = "year") %>%
  left_join(Ex_it, by = c("exporter_iso3", "year")) %>%
  mutate(RCA_ijt = (total_export / total_export_country) / (Ex_jt / Ex_t))


# d)

# Filtrar os dados para t = 2016

x_2016 <- vantagem_comparativa %>%
  filter(year == 2016)

# Selecionar os 10 paises (a sua escolha):


# Selecionar os 10 países com maiores volumes de exportação em 2016
top_10_i_2016 <- x_2016 %>%
  filter(year == 2016) %>%
  group_by(exporter_iso3) %>%
  summarize(total_export_2016 = sum(total_export_country)) %>%
  arrange(desc(total_export_2016)) %>%
  head(10)


# Identificar os bens com vantagens comparativas para esses paises:

# Criar tabela com principais industrias que cada pais se destaca
tabela_vantagem_comparativa <- x_2016 %>%
  filter(exporter_iso3 %in% top_10_i_2016$exporter_iso3) %>%
  group_by(exporter_iso3, industry_description) %>%
  summarize(media_RCA = mean(RCA_ijt)) %>%
  arrange(exporter_iso3, desc(media_RCA))

# Visualizar a tabela
View(tabela_vantagem_comparativa)


# Lista de países a serem analisados
paises <- c("CAN", "CHN", "DEU", "FRA", "GBR", "ITA", "JPN", "KOR", "NLD", "USA")

# Número de principais indústrias a serem incluídas no gráfico
top_n_industrias <- 10


# GRÁFICOS:

# Criar uma lista para armazenar os gráficos
graficos <- list()


# Iterar através da lista de países
for (pais in paises) {
  # Filtrar os dados para incluir apenas as 10 principais indústrias para o país atual
  dados_pais <- x_2016 %>%
    filter(exporter_iso3 == pais) %>%
    group_by(industry_description) %>%
    summarize(media_RCA = mean(RCA_ijt)) %>%
    arrange(desc(media_RCA)) %>%
    head(top_n_industrias)
  
  # Criar um gráfico de barras empilhadas para o país atual
  grafico_barras_pais <- ggplot(dados_pais, aes(x = as.factor(industry_description), y = media_RCA)) +
    geom_bar(stat = "identity") +
    labs(x = "Código da Indústria", y = "Índice de RCA") +
    ggtitle(paste("Top", top_n_industrias, "Indústrias com Vantagem Comparativa para", pais)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Adicionar o gráfico à lista
  graficos[[pais]] <- grafico_barras_pais
}

# Exibir os gráficos
for (i in seq_along(graficos)) {
  print(graficos[[i]])
}


# TABELAS:

# Criar uma lista para armazenar os dataframes
dataframes <- list()

# Iterar através da lista de países
for (pais in paises) {
  # Filtrar os dados para incluir apenas as 10 principais indústrias para o país atual
  dados_pais <- x_2016 %>%
    filter(exporter_iso3 == pais) %>%
    group_by(industry_description) %>%
    summarize(media_RCA = mean(RCA_ijt)) %>%
    arrange(desc(media_RCA)) %>%
    head(top_n_industrias)
  
  # Nome do dataframe com base no país
  nome_dataframe <- paste(pais, "_dataframe", sep = "")
  
  # Atribuir o dataframe à lista de dataframes
  dataframes[[nome_dataframe]] <- dados_pais
}

# Colocar os dataframes no ambiente global
for (nome_dataframe in names(dataframes)) {
  assign(nome_dataframe, dataframes[[nome_dataframe]], envir = .GlobalEnv)
}





# Macro-setores



#Adicione as informações sobre os setores no dataframe mapeamento_industrias
mapeamento_industrias <- mapeamento_industrias %>%
  mutate(Broad_Sector = case_when(
    ID %in% 1:26 ~ "Agriculture",
    ID %in% 27:33 ~ "Mining",
    ID %in% 34:153 ~ "Manufacturing",
    ID %in% 154:170 ~ "Services"
  )) %>%
  mutate(ID = as.character(ID))  # Converter a coluna ID para caractere



# Converter a coluna "ID" do dataframe mapeamento_industrias para double
mapeamento_industrias <- mapeamento_industrias %>%
  mutate(ID = as.double(ID))

# Converter a coluna "industry_id" do dataframe x_2016 para double
x_2016 <- x_2016 %>%
  mutate(industry_id = as.double(industry_id))

# Criar uma lista para armazenar os gráficos
graficos_paises <- list()

# Iterar através da lista de países
for (pais in paises) {
  # Filtrar os dados para incluir apenas o país atual
  dados_pais <- x_2016 %>%
    filter(exporter_iso3 == pais)
  
  # Converter a coluna industry_description_id no dataframe dados_pais para caractere
  dados_pais <- dados_pais %>%
    mutate(industry_description = as.character(industry_description))
  
  # Unir os dataframes dados_pais e mapeamento_industrias pelo ID da indústria
  dados_pais <- left_join(dados_pais, mapeamento_industrias, by = c("industry_id" = "ID"))
  
  # Criar um gráfico de barras com os quatro macro-setores
  grafico_barras_pais <- ggplot(dados_pais, aes(x = Broad_Sector, y = RCA_ijt, fill = Broad_Sector)) +
    geom_bar(stat = "identity") +
    labs(x = NULL, y = "Índice de RCA") +
    ggtitle(paste("RCA por Macro-setor para", pais)) +
    scale_fill_manual(values = c("Agriculture" = "blue", "Mining" = "green", "Manufacturing" = "red", "Services" = "purple")) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Adicionar o gráfico à lista
  graficos_paises[[pais]] <- grafico_barras_pais
}

# Exibir os gráficos
for (i in seq_along(graficos_paises)) {
  print(graficos_paises[[i]])
}

# Criar tabela com valores médios de RCA por macro-setor ao longo do tempo, para cada país

# Unir os dataframes x_ind, mapeamento_industrias e vantagem_comparativa
x_ind <- x_ind %>%
  left_join(mapeamento_industrias, by = c("industry_id" = "ID")) %>%
  left_join(vantagem_comparativa, by = c("exporter_iso3", "year", "industry_description"))

# Calcular a média do índice de RCA por macro-setor ao longo do tempo
media_rca_por_macrosetor <- x_ind %>%
  group_by(Broad_Sector, year, exporter_iso3) %>%
  summarize(Media_RCA = mean(RCA_ijt))

# Visualizar a tabela
dfmedia = media_rca_por_macrosetor
View(dfmedia)


# Tendências

# Criar gráficos de linha separados para cada país
graficos_paises <- list()

for (pais in paises) {
  dados_pais <- dfmedia %>%
    filter(exporter_iso3 == pais)
  
  grafico <- ggplot(dados_pais, aes(x = year, y = Media_RCA, color = Broad_Sector)) +
    geom_line() +
    labs(x = "Ano", y = "RCA Médio", color = "Macro-setor") +
    ggtitle(paste("RCA Médio por Macro-setor para", pais)) +
    theme_minimal()
  
  graficos_paises[[pais]] <- grafico
}

# Exibir os gráficos
for (i in seq_along(graficos_paises)) {
  print(graficos_paises[[i]])
}

# e)

# Resposta analítica, sem código


# f)

# Resposta a priori analítica, sem código. Basta comparar os gráficos e, se necessário rodar algum teste.


# DOTAÇÃO DE FATORES E VANTAGEM COMPARATIVA ====================================

# a)

# Feito em "a" na seção anterior

# b)

# Especificar o caminho para o arquivo Excel
caminho_arquivo <- "C:/Users/jcase/OneDrive/João/Insper/6º semestre/Comércio Internacional/APS/APS1/com_int_aps1_dados/pwt1001.xlsx"

# Ler o arquivo Excel
dados_pwt <- read_excel(caminho_arquivo,sheet = "Data")

# Filtrar os dados para o período de 2000 a 2016 e selecionar as variáveis de interesse
dados_filtrados <- dados_pwt %>%
  filter(year >= 2000 & year <= 2016) %>%
  select(countrycode, year, hc, cn, rnna)

# Renomear as colunas
dados_filtrados <- dados_filtrados %>%
  rename(
    "Year" = year,
    "Country_Code" = countrycode,
    "Human_Capital_Index" = hc,
    "Capital_Stock_PPP" = cn,
    "Capital_Stock_NP" = rnna
  )

# c)


# Caminho para o arquivo CSV
caminho_arquivo <- "C:/Users/jcase/OneDrive/João/Insper/6º semestre/Comércio Internacional/APS/APS1/com_int_aps1_dados/wdi.csv"

# Ler os dados do arquivo CSV
dados_wdi <- read_csv(caminho_arquivo)

# Usar a função pivot_longer para transformar as colunas de ano em linhas
dados_wdi_long <- dados_wdi %>%
  pivot_longer(cols = starts_with("v"), names_to = "Year", values_to = "Value")

# Filtrar as informações de "Agricultural land area" (série AG.LND.AGRI.K2) entre 2000 e 2016
dados_agricultura <- dados_wdi_long %>%
  filter(indicatorcode == "AG.LND.AGRI.K2" & Year >= "v2000" & Year <= "v2016") %>%
  select(countrycode, Year, Value)

dados_agricultura <- dados_agricultura %>%
  rename(Agricultural_land_area = Value)


# d)

# Converter as colunas "year" e "Year" para double
x_ind <- x_ind %>%
  mutate(year = as.double(year))

dados_filtrados <- dados_filtrados %>%
  mutate(Year = as.double(Year))

dados_agricultura <- dados_agricultura %>%
  mutate(Year = as.double(sub("v", "", Year)))  # Remover o prefixo "v" e converter para double

# Combinar os dataframes
dados_combinados <- left_join(x_ind, dados_filtrados, by = c("exporter_iso3" = "Country_Code", "year" = "Year"))
dados_combinados <- left_join(dados_combinados, dados_agricultura, by = c("exporter_iso3" = "countrycode", "year" = "Year"))


# Removendo duplicatas
# Selecionar apenas as colunas desejadas e renomear as colunas terminadas em ".x"
dados_combinados <- dados_combinados %>%
  select(-ends_with(".y")) %>%  # Excluir as colunas que terminam com ".y"
  rename_with(~gsub("\\.x$", "", .), ends_with(".x"))  # Remover o ".x" do final das colunas


# e)


# Filtrar os dados para o ano de 2016 e cada setor
dados_2016 <- dados_combinados %>%
  filter(year == 2016)

# Definir os setores de interesse
setores <- c("Agriculture", "Mining", "Manufacturing", "Services")

# Criar uma lista para armazenar os gráficos de dispersão
graficos_dispersao <- list()

# 1º Loop pelos setores - LOG RCA x AREA AGRO
for (setor in setores) {
  # Filtrar os dados para o setor atual
  dados_setor <- dados_2016 %>%
    filter(Broad_Sector == setor)
  
  # Filtrar os dados para excluir valores ausentes e converter RCA_ijt para numeric
  dados_setor <- dados_setor %>%
    filter(!is.na(Agricultural_land_area) &
             !is.na(Capital_Stock_PPP) &
             !is.na(Human_Capital_Index)) %>%
    mutate(RCA_ijt = as.numeric(RCA_ijt))
  
  # Criar bins para agrupar os dados
  num_bins <- 30000
  dados_setor <- dados_setor %>%
    mutate(
      bin_log_RCA = cut(RCA_ijt, breaks = seq(-10, 10, length.out = num_bins), labels = FALSE),
      bin_Agricultural_Land = cut(Agricultural_land_area, breaks = seq(-10, 10, length.out = num_bins), labels = FALSE),
      bin_Capital_Stock = cut(Capital_Stock_PPP, breaks = seq(-10, 10, length.out = num_bins), labels = FALSE),
      bin_Human_Capital_Index = cut(Human_Capital_Index, breaks = seq(-10, 10, length.out = num_bins), labels = FALSE)
    )
  
  # Calcular a média para cada bin
  resumo_bins <- dados_setor %>%
    group_by(bin_log_RCA) %>%
    summarize(
      Media_Agricultural_Land = mean(`Agricultural_land_area`),
      Media_Capital_Stock = mean(Capital_Stock_PPP),
      Media_Human_Capital_Index = mean(Human_Capital_Index)
    )
  
  # Criar o gráfico de dispersão com reta de regressão linear (log RCA x Area Agro)
  grafico <- ggplot(resumo_bins, aes(x = Media_Agricultural_Land, y = bin_log_RCA)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      x = "Área de Terras Agrícolas",
      y = "Log RCA",
      title = paste("Dispersão de Log RCA vs. Área de Terras Agrícolas (", setor, " - 2016)")
    ) +
    theme_minimal()
  
  
  # Adicionar o gráfico à lista
  graficos_dispersao[[setor]] <- grafico
}

# Exibir os gráficos
for (i in seq_along(graficos_dispersao)) {
  print(graficos_dispersao[[i]])
}



# 2º Loop pelos setores - LOG RCA x ESTOQUE DE CAPITAL PPP
for (setor in setores) {
  # Filtrar os dados para o setor atual
  dados_setor <- dados_2016 %>%
    filter(Broad_Sector == setor)
  
  # Filtrar os dados para excluir valores ausentes e converter RCA_ijt para numeric
  dados_setor <- dados_setor %>%
    filter(!is.na(Agricultural_land_area) &
             !is.na(Capital_Stock_PPP) &
             !is.na(Human_Capital_Index)) %>%
    mutate(RCA_ijt = as.numeric(RCA_ijt))
  
  # Criar bins para agrupar os dados
  num_bins <- 30000
  dados_setor <- dados_setor %>%
    mutate(
      bin_log_RCA = cut(RCA_ijt, breaks = seq(-10, 10, length.out = num_bins), labels = FALSE),
      bin_Agricultural_Land = cut(Agricultural_land_area, breaks = seq(-10, 10, length.out = num_bins), labels = FALSE),
      bin_Capital_Stock = cut(Capital_Stock_PPP, breaks = seq(-10, 10, length.out = num_bins), labels = FALSE),
      bin_Human_Capital_Index = cut(Human_Capital_Index, breaks = seq(-10, 10, length.out = num_bins), labels = FALSE)
    )
  
  # Calcular a média para cada bin
  resumo_bins <- dados_setor %>%
    group_by(bin_log_RCA) %>%
    summarize(
      Media_Agricultural_Land = mean(`Agricultural_land_area`),
      Media_Capital_Stock = mean(Capital_Stock_PPP),
      Media_Human_Capital_Index = mean(Human_Capital_Index)
    )

  # Criar o gráfico de dispersão com reta de regressão linear (log RCA x Estoque de Capital)
  grafico <- ggplot(resumo_bins, aes(x = Media_Capital_Stock, y = bin_log_RCA)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      x = "Estoque de Capital PPP",
      y = "Log RCA",
      title = paste("Dispersão de Log RCA vs. Estoque de Capital PPP (", setor, " - 2016)")
    ) +
    theme_minimal()

  # Adicionar o gráfico à lista
  graficos_dispersao[[setor]] <- grafico
}


# Exibir os gráficos
for (i in seq_along(graficos_dispersao)) {
  print(graficos_dispersao[[i]])
}




# 3º Loop pelos setores - LOG RCA x CAPITAL HUMANO
for (setor in setores) {
  # Filtrar os dados para o setor atual
  dados_setor <- dados_2016 %>%
    filter(Broad_Sector == setor)
  
  # Filtrar os dados para excluir valores ausentes e converter RCA_ijt para numeric
  dados_setor <- dados_setor %>%
    filter(!is.na(Agricultural_land_area) &
             !is.na(Capital_Stock_PPP) &
             !is.na(Human_Capital_Index)) %>%
    mutate(RCA_ijt = as.numeric(RCA_ijt))
  
  # Criar bins para agrupar os dados
  num_bins <- 30000
  dados_setor <- dados_setor %>%
    mutate(
      bin_log_RCA = cut(RCA_ijt, breaks = seq(-10, 10, length.out = num_bins), labels = FALSE),
      bin_Agricultural_Land = cut(Agricultural_land_area, breaks = seq(-10, 10, length.out = num_bins), labels = FALSE),
      bin_Capital_Stock = cut(Capital_Stock_PPP, breaks = seq(-10, 10, length.out = num_bins), labels = FALSE),
      bin_Human_Capital_Index = cut(Human_Capital_Index, breaks = seq(-10, 10, length.out = num_bins), labels = FALSE)
    )
  
  # Calcular a média para cada bin
  resumo_bins <- dados_setor %>%
    group_by(bin_log_RCA) %>%
    summarize(
      Media_Agricultural_Land = mean(`Agricultural_land_area`),
      Media_Capital_Stock = mean(Capital_Stock_PPP),
      Media_Human_Capital_Index = mean(Human_Capital_Index)
    )
  
  # Criar o gráfico de dispersão com reta de regressão linear (log RCA x Capital Humano)
  grafico <- ggplot(resumo_bins, aes(x = Media_Human_Capital_Index, y = bin_log_RCA)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      x = "Capital Humano (index)",
      y = "Log RCA",
      title = paste("Dispersão de Log RCA vs. Capital Humano (", setor, " - 2016)")
    ) +
    theme_minimal()
  
  # Adicionar o gráfico à lista
  graficos_dispersao[[setor]] <- grafico
}


# Exibir os gráficos
for (i in seq_along(graficos_dispersao)) {
  print(graficos_dispersao[[i]])
}


# f)

# Resposta analítica, sem código, a priori.


# COMERCIO INTRA-INDUSTRIA =====================================================


# a)

# Filtrar os dados para o ano de 2016
itpd_2016 <- itpde %>%
  filter(year == 2016)

# Filtrar os dados para incluir apenas exportações (exporter_iso3 diferente de importer_iso3)
exports <- itpd_2016 %>%
  filter(exporter_iso3 != importer_iso3)

# Calcular as exportações em US$ bilhões
exports <- exports %>%
  group_by(exporter_iso3, industry_id) %>%
  summarize(total_export_billion = sum(trade) / 1e9) # Dividir por 1 bilhão para obter bilhões de dólares

# Filtrar os dados para incluir apenas importações (exporter_iso3 igual a importer_iso3)
imports <- itpd_2016 %>%
  filter(exporter_iso3 == importer_iso3)

# Calcular as importações em US$ bilhões
imports <- imports %>%
  group_by(exporter_iso3, industry_id) %>%
  summarize(total_import_billion = sum(trade) / 1e9) # Dividir por 1 bilhão para obter bilhões de dólares

#Resultados:
View(exports)
View(imports)
comercio <- left_join(exports,imports)

# b)

# Caminho para o arquivo WDICountry.xlsx
caminho_wdi_country <- "C:/Users/jcase/OneDrive/João/Insper/6º semestre/Comércio Internacional/APS/APS1/com_int_aps1_dados/WDICountry.xlsx"  

# Carregue os dados do arquivo WDICountry.xlsx
wdi_country_data <- read_excel(caminho_wdi_country)

# Filtre os dados para incluir apenas as colunas necessárias (Country Code e Income Group)
wdi_country_data <- wdi_country_data %>%
  select(`Code`, `Income group`)

# Renomeie as colunas para facilitar a junção
colnames(wdi_country_data) <- c("exporter_iso3", "Income Group")

# Junte os dados de classificação de renda à base de comércio
exports_with_income <- left_join(exports, wdi_country_data, by = "exporter_iso3")
imports_with_income <- left_join(imports, wdi_country_data, by = "exporter_iso3")
comercio_with_income <- left_join(exports_with_income,imports_with_income)

# Incluindo a descrição de cada código de indústria

mapeamento_industrias <- rename(mapeamento_industrias, industry_id = ID)
comercio_with_income <- left_join(comercio_with_income, mapeamento_industrias, by = "industry_id")



# c)

# Calcular o índice GLij
comercio_with_income <- comercio_with_income %>%
  mutate(GLij = 1 - abs(total_export_billion - total_import_billion) / (total_export_billion + total_import_billion))

# Criar um histograma do índice GLij
histogram_plot <- ggplot(comercio_with_income, aes(x = GLij)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") + 
  labs(
    x = "Índice GLij",
    y = "Frequência",
    title = "Distribuição do Índice GLij"
  ) +
  geom_text(data = subset(comercio_with_income, cumsum(Industry_Description) > 5), 
            aes(x = GLij, y = cumsum(Industry_Description), label = Industry_Description),
            nudge_x = 0.45, nudge_y = 5, check_overlap = TRUE) +
  theme_minimal()

# Exibir o gráfico
print(histogram_plot)


# Selecionar as maiores indústrias com base no índice GLij
top_industries <- comercio_with_income %>%
  top_n(29, GLij)

# Criar o gráfico de barras para as maiores indústrias
bar_plot <- ggplot(top_industries, aes(x = reorder(Industry_Description, GLij), y = GLij, fill = Industry_Description)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Indústria",
    y = "Índice GLij",
    title = "Índice GLij para as maiores indústrias"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  guides(fill = "none")

# Exibir o gráfico
print(bar_plot)


# d)

# Criando a coluna "categoria" que classifica os setores entre "Services" e "Non-Services"
comercio_with_income <- comercio_with_income %>%
  mutate(
    categoria = ifelse(between(industry_id, 154, 170), "Services", "Non-Services")
  )


# e)

# Criando a coluna "income_cat" que classifica os níveis de renda entre "High income" e "Non-High income"
comercio_with_income <- comercio_with_income %>%
  mutate(
    income_cat = ifelse(`Income Group`=="High income","High income","Non-High income")
  )


# f)

# Filtrar para o setor de serviços e renda alta
services_high_income <- comercio_with_income %>%
  filter(categoria == "Services" & income_cat == "High income")

# Filtrar para o setor de serviços e renda não-alta
services_non_high_income <- comercio_with_income %>%
  filter(categoria == "Services" & income_cat == "Non-High income")

# Filtrar para o setor de não-serviços e renda alta
non_services_high_income <- comercio_with_income %>%
  filter(categoria == "Non-Services" & income_cat == "High income")

# Filtrar para o setor de não-serviços e renda não-alta
non_services_non_high_income <- comercio_with_income %>%
  filter(categoria == "Non-Services" & income_cat == "Non-High income")

# Função para criar histogramas frequência com rótulos
create_histogram <- function(data, title) {
  ggplot(data, aes(x = GLij)) +
    geom_histogram(binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = title, x = "Índice de Grubel-Lloyd", y = "Frequência") +
    theme_minimal()
}

# Criar os histogramas
histogram_services_high_income <- create_histogram(services_high_income, "Serviços - Renda Alta")
histogram_services_non_high_income <- create_histogram(services_non_high_income, "Serviços - Renda Não-Alta")
histogram_non_services_high_income <- create_histogram(non_services_high_income, "Não-Serviços - Renda Alta")
histogram_non_services_non_high_income <- create_histogram(non_services_non_high_income, "Não-Serviços - Renda Não-Alta")

# Exibir os histogramas
histogram_services_high_income
histogram_services_non_high_income
histogram_non_services_high_income
histogram_non_services_non_high_income


# Função para criar histogramas indústrias com rótulos
create_hist <- function(data, title) {
  # Filtrar os dados para incluir apenas as indústrias com barras visíveis
  data_filtered <- data[data$GLij > 0,]
  
  ggplot(data_filtered, aes(x = reorder(Industry_Description, GLij), y = GLij, fill = Industry_Description)) +
    geom_bar(stat = "identity") +
    labs(
      x = "Indústria",
      y = "Índice GLij",
      title = title  # Usar o título passado como argumento da função
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
    guides(fill = "none")
}

# Criar os histogramas
hist_services_high_income <- create_hist(services_high_income, "Serviços - Renda Alta")
hist_services_non_high_income <- create_hist(services_non_high_income, "Serviços - Renda Não-Alta")
hist_non_services_high_income <- create_hist(non_services_high_income, "Não-Serviços - Renda Alta")
hist_non_services_non_high_income <- create_hist(non_services_non_high_income, "Não-Serviços - Renda Não-Alta")

# Exibir os histogramas
hist_services_high_income
hist_services_non_high_income
hist_non_services_high_income
hist_non_services_non_high_income

# g)

# Respota analítica, sem codigo, a priori.


# COMERCIO INTRA-INDUSTRIA NA UE ===============================================

# Resolucao da questao 6 em outro script.
