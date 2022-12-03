#Pacote necessários para implementação da Análise PCA
pacotes <- c("plotly", #plataforma gráfica
             "tidyverse", #carregar outros pacotes do R
             "ggrepel", #geoms de texto e rótulo para 'ggplot2' que ajudam a
             #evitar sobreposição de textos
             "knitr", "kableExtra", #formatação de tabelas
             "reshape2", #função 'melt'
             "PerformanceAnalytics", #função 'chart.Correlation' para plotagem
             "psych", #elaboração da fatorial e estatísticas
             "ltm", #determinação do alpha de Cronbach pela função 'cronbach.alpha'
             "Hmisc", # matriz de correlações com p-valor
             "readxl") # leitura de dados em Excel

#Instalação e carregamento dos pacotes
if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#Carregando a base de dados

auto <- read.csv('imports_85_data.csv')

#Observar o banco de dados
auto %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 15)

#Agora faremos uma análise descritiva do nosso banco de dados

labels(auto)

summary(auto)

#Selecionando as variáveis a serem utilisadas na Analise de componentes principais,
#Removendo os valores faltantes e renomeando as colunas

auto <- auto[,c("column_j", "column_k", "column_l", "column_m", "column_n", "column_q",
                "column_s", "column_t", "column_u", "column_v", "column_w", "column_x",
                "column_y", "column_z")]

auto <- na.omit(auto)

português <- c("tamanho_motor", 'distancia_eixos', 'comprimento', 'largura', 'altura',
               'peso', 'larg_pistao', 'dist_pistao', 'taxa_compressao', 'cavalos', 'pico_rotacao',
               'autonomia_cidade', 'autonomia_estrada', 'preco')

colnames(auto) <- português

#fazendo a nossa matriz de correlação e significância

ma_cor <- rcorr(as.matrix(auto[,1:13], type = 'pearson'))

corr_efic <- ma_cor$r#matriz de correlação
view(corr_efic)
corr_sign <- round(ma_cor$P, 5)#matriz de significância
view(corr_sign)

#Visualização da matris de correlação usando mapas de calor
ggplotly(
  auto[,1:13] %>%
    cor() %>%
    melt() %>%
    rename(Correlação = value) %>%
    ggplot() +
    geom_tile(aes(x = Var1, y = Var2, fill = Correlação)) +
    geom_text(aes(x = Var1, y = Var2, label = format(Correlação, digits = 1)),
              size = 5) +
    scale_fill_viridis_b() +
    labs(x = NULL, y = NULL) +
    theme_bw())

#Teste de esfericidade de bartlett (não usaremos a variável "preço", na análise)
cortest.bartlett(auto[, 1:13])

# Elaboração da análise fatorial por componentes principais
fatorial <- principal(auto[, 1:13],
                      nfactors = length(auto[, 1:13]),
                      rotate = "none",
                      scores = TRUE)

# Eigenvalues (autovalores)
eigenvalues <- round(fatorial$values, 5)
eigenvalues
round(sum(eigenvalues), 2)

# Identificação da variância compartilhada em cada fator
variancia_compartilhada <- as.data.frame(fatorial$Vaccounted) %>% 
  slice(1:3)

rownames(variancia_compartilhada) <- c("Autovalores",
                                       "Prop. da Variância",
                                       "Prop. da Variância Acumulada")
view(variancia_compartilhada)


# Variância compartilhada pelas variáveis originais para a formação de cada fator
round(variancia_compartilhada, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# Cálculo dos scores fatoriais
scores_fatoriais <- as.data.frame(fatorial$weights)
view(scores_fatoriais)

# Cálculo dos fatores propriamente ditos
fatores <- as.data.frame(fatorial$scores)
view(fatores)

# Coeficientes de correlação de Pearson para cada par de fatores (ortogonais)
rho <- rcorr(as.matrix(fatores), type="pearson")
round(rho$r, 4)

# Cálculo das cargas fatoriais
cargas_fatoriais <- as.data.frame(unclass(fatorial$loadings))
view(cargas_fatoriais)

# Cálculo das comunalidades
comunalidades <- as.data.frame(unclass(fatorial$communality)) %>%
  rename(comunalidades = 1)
view(comunalidades)


#######################
#-- APLICANDO OS CRITÉRIOS DE KAIZER, FICAREMOS APENAS COM PC1, PC2, PC3
#######################


# Definição da quantidade de fatores com eigenvalues maiores que 1
k <- sum(eigenvalues > 1)
print(k)
# Elaboração da análise fatorial por componentes principais
# Com quantidade 'k' de fatores com eigenvalues maiores que 1
fatorial1 <- principal(auto[, 1:13],
                       nfactors = k,
                       rotate = "none",
                       scores = TRUE)
fatorial1

# Cálculo das cargas fatoriais
cargas_fatoriais1 <- as.data.frame(unclass(fatorial1$loadings))
view(cargas_fatoriais1)

# Visualização das cargas fatoriais
round(cargas_fatoriais1, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)

# Cálculo das comunalidades com o primeiro fator ('k' = 1)
comunalidades1 <- as.data.frame(unclass(fatorial1$communality)) %>%
  rename(comunalidades = 1)
view(comunalidades1)

# Visualização das comunalidades
round(comunalidades1, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)

# Cálculo dos scores fatoriais
scores_fatoriais1 <- as.data.frame(fatorial1$weights)
view(scores_fatoriais1)

# Visualização dos scores fatoriais
round(scores_fatoriais1, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# Cálculo dos fatores propriamente ditos
fatores1 <- as.data.frame(fatorial1$scores)
view(fatores1)


# Criação de um ranking Critério da soma ponderada e ordenamento)
auto$ranking <- fatores$PC1 * variancia_compartilhada$PC1[2] +
  fatores$PC2 * variancia_compartilhada$PC2[2] +
  fatores$PC3 * variancia_compartilhada$PC3[2]
View(auto)

# O ranking captura o valor das casas?
corr_valor <- rcorr(as.matrix(auto[,14:15]))

valor_corr_coef <- corr_valor$r # Matriz de correlações
print(valor_corr_coef)
valor_corr_sig <- round(corr_valor$P, 5) # Matriz com p-valor dos coeficientes
print(valor_corr_sig)

####FIM!!!