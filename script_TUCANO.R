############################################################
# TUCANO - Técnicas Unificadas de Categorização e Análise de Notícias Online
# Autores: Victor Felipe M. de Lima & Frederico Marianetti Soriani
# Repositório de dados modelo: www.github.com/victorfelipe62vf/TUCANO
############################################################

############################################################
# 1. CARREGAR E INSTALAR PACOTES
############################################################

# Função auxiliar para instalar/carregar pacotes CRAN
usar_pacotes <- function(pkgs){
  novos <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
  if(length(novos)) install.packages(novos, dependencies = TRUE)
  invisible(lapply(pkgs, library, character.only = TRUE))
}

# Pacotes usados
pacotes_cran <- c(
  "tidyverse", "stringr", "stringi", "readtext",
  "quanteda", "quanteda.textstats", "quanteda.textplots",
  "tm", "RColorBrewer", "base64enc"
)

usar_pacotes(pacotes_cran)

############################################################
# 2. CARREGAR DADOS BRUTOS DO MEDIA CLOUD
############################################################

# Defina o seu diretório de trabalho
diretorio_trabalho <- "C:/Users/Victor Felipe/Desktop/Doutorado/Material/" # >>>>> EDITE APENAS ESTA LINHA <<<<<
setwd(diretorio_trabalho)
cat("Diretório de trabalho definido como:\n", normalizePath(getwd()), "\n\n")

# Informe o caminho do arquivo .CSV baixado do Media Cloud
# (ajuste conforme sua máquina – sugere-se colocar na mesma pasta do diretório acima)
caminho_arquivo <- "C:/Users/Victor Felipe/Desktop/Doutorado/Material/dados_modelo.csv"

dados <- read.csv(caminho_arquivo,
                  header = TRUE,
                  sep    = ",",
                  stringsAsFactors = FALSE)

# 2.1 Inspeção inicial
cat("\nDimensões da base original (todas as línguas):\n")
print(dim(dados)) # Deve conter o número total de notícias brutas seguido por 8 (que é o número de colunas no arquivo .CSV)
cat("\nVariáveis disponíveis:\n")
print(names(dados)) # Deve conter 8 itens: "id", "indexed_date", "language", "media_name", "media_url", "publish_date", "title" e "url"
## Se não constar como acima, rever o arquivo .CSV

# 2.2 RESUMO DOS VEICULOS (MEDIA_NAME)

# Esta parte serve para você decidir qual veículo filtrar, se quiser.
{resumo_bases <- dados %>%
  count(media_name, sort = TRUE)

cat("\nResumo dos veículos (media_name) e número de notícias:\n")
print(resumo_bases)} #Vai aparecer o nome do veículo seguido pelo número de notícias

############################################################
# 3. CONFIGURAÇÕES BÁSICAS
############################################################

# 3.1 Escolha do veículo para análise:
# >> Consulte o tópico 2.2 para ver de quais veículos são suas notícias.
#    Decida se quer filtrar por algum veículo ou analisar o conteúdo completo <<
# - Use NA para analisar todo o acervo: filtro_media <- NA
# - Ou informe o nome exato do veículo conforme item 2.2 (media_name), 
# - por exemplo: Para 1 veículo: filtro_media <- "uol.com.br" ou Para 2 ou mais: filtro_media <- c("uol.com.br", "globo.com")
filtro_media <- NA

# 3.2 Parâmetros gerais de análise (ajuste se desejar)
{min_termfreq_unigram <- 20   # Frequência mínima para definir termos mais frequentes
min_termfreq_bigram  <- 2    # Frequência mínima para bigramas
min_termfreq_trigram <- 2    # Frequência mínima para trigramas
top_n_terms_fcm      <- 50   # Número de termos mais frequentes para FCM
window_fcm           <- 5    # Janela de coocorrência para FCM
min_freq_network     <- 25   # Frequência mínima para aparecer na rede (ajuste conforme o acervo)
}

############################################################
# 4. FILTROS BÁSICOS: IDIOMA E BASE DE DADOS
############################################################

# 4.1 Filtrar apenas notícias em português
{cat("\nDistribuição por idioma (language):\n")
print(table(dados$language))} # Aqui vai aparecer a quantidade de notícias brutas por idioma

{dados_pt <- dados %>%
  filter(language == "pt")
cat("\nDimensões após filtrar para português:\n")
print(dim(dados_pt))} # Aqui vai aparecer a quantidade de notícias brutas em português

# 4.2 Filtrar por veículo específico ou acervo geral
{if(!all(is.na(filtro_media))){
  cat("\nAplicando filtro por media_name = ", paste(filtro_media, collapse = ", "), "\n", sep = "")
  
  dados_filtrados <- dados_pt %>%
    filter(media_name %in% filtro_media)
  
  if(nrow(dados_filtrados) == 0){
    stop("Nenhum registro encontrado para o(s) media_name informado(s). Verifique 'resumo_bases'.")
  }
} else {
  cat("\nNenhum filtro por media_name aplicado: analisando todo o acervo em português.\n")
  dados_filtrados <- dados_pt
}


cat("\nDimensões da base utilizada na análise (após filtros iniciais):\n")
print(dim(dados_filtrados))} # Aqui vai aparecer a quantidade de notícias brutas com base no que foi definido como filtro (acervo geral ou veículos específicos)

############################################################
# 5. REMOVER DUPLICATAS E CRIAR VARIÁVEIS TEMPORAIS
############################################################

# 5.1 Remover títulos duplicados
{num_duplicados <- sum(duplicated(dados_filtrados$title))
cat("\nNúmero de títulos duplicados na base filtrada:", num_duplicados, "\n")

dados_unicos <- dados_filtrados %>%
  filter(!duplicated(title))

cat("Dimensões após remoção de duplicatas (corpus final de análise):\n")
print(dim(dados_unicos))} # Aqui vai aparecer a quantidade de notícias filtradas, ou seja: seu corpus de análise final

# 5.2 Criar variável de ano
{dados_unicos$publish_date <- as.Date(dados_unicos$publish_date)
dados_unicos$ano <- format(dados_unicos$publish_date, "%Y")

cat("\nDistribuição de notícias por ano:\n")
print(table(dados_unicos$ano))}

############################################################
# 6. PREPARAÇÃO DOS TÍTULOS (CORPUS E TOKENS)
############################################################

# 6.1 Vetor de títulos
{titulos_raw <- dados_unicos$title

# 6.2 Normalizar acentos (Latin-ASCII)
titulos_ascii <- stringi::stri_trans_general(titulos_raw, "Latin-ASCII")

# 6.3 Criar corpus apenas com títulos
corpus_titulos <- corpus(titulos_ascii)

# 6.4 Tokenização + pré-processamento
tokens_titulos <- tokens(
  corpus_titulos,
  remove_punct   = TRUE,   # Remove pontuação
  remove_numbers = TRUE    # Remove números
)

tokens_titulos <- tokens_tolower(tokens_titulos)                 # Tudo em minúsculas
tokens_titulos <- tokens_remove(tokens_titulos, stopwords("pt")) # Remove stopwords em português

# 6.5 Matriz documento-termo (DFM)
dfm_titulos <- dfm(tokens_titulos)

cat("\nDimensões da matriz documento-termo (dfm_titulos):\n")
print(dim(dfm_titulos))} #Aqui vai aparecer a quantidade de notícias análisadas e a quantidade de termos análisados

# 6.6 Matriz termo-documento (TDM) – apenas transposição
{tdm_titulos <- t(dfm_titulos)
cat("\nDimensões da matriz termo-documento (tdm_titulos):\n")
print(dim(tdm_titulos))}

############################################################
# 7. FREQUÊNCIA DE TERMOS ISOLADOS
############################################################

#Top Termos mais frequentes
{freq <- textstat_frequency(dfm_titulos)
freq$rel_freq <- freq$frequency / sum(freq$frequency)

cat("\nTop 50 termos mais frequentes (frequência absoluta e relativa):\n")
print(head(freq, 50))} ## <<<VOCE PODE AJUSTAR ESTE VALOR PARA AUMENTAR OU DIMINUIR A QUANTIDADE DE TERMOS

# DFM de termos isolados com corte de frequência mínima – valor definido em "min_termfreq_unigram" no tópico 3.2
{dfm_uni <- dfm_trim(dfm_titulos, min_termfreq = min_termfreq_unigram)

cat("\nDimensões de dfm_uni (após dfm_trim para termos isolados):\n")
print(dim(dfm_uni))}

{cat("\nTermos isolados mais frequentes (após corte):\n")
print(textstat_frequency(dfm_uni) %>% head(200))} ## <<<VOCE PODE AJUSTAR ESTE VALOR PARA AUMENTAR OU DIMINUIR A QUANTIDADE DE TERMOS

############################################################
# 8. NUVEM DE PALAVRAS (WORDCLOUD)
############################################################

# Nuvem colorida
textplot_wordcloud(
  dfm_titulos,
  max_words = 200, ## <<<VOCE PODE AJUSTAR ESTE VALOR PARA AUMENTAR OU DIMINUIR A QUANTIDADE DE TERMOS NA NUVEM
  color     = RColorBrewer::brewer.pal(10, "Dark2")
)

# Nuvem em tons de cinza
textplot_wordcloud(
  dfm_titulos,
  max_words = 200, ## <<<VOCE PODE AJUSTAR ESTE VALOR PARA AUMENTAR OU DIMINUIR A QUANTIDADE DE TERMOS NA NUVEM
  color     = gray.colors(10, start = 0, end = 0.9)
)

############################################################
# 9. BIGRAMAS E TRIGRAMAS (FREQUÊNCIAS)
############################################################

# 9.1 Bigramas
{toks_bi <- tokens_ngrams(tokens_titulos, n = 2)
dfm_bi  <- dfm(toks_bi) %>%
  dfm_trim(min_termfreq = min_termfreq_bigram)

cat("\nTop bigramas:\n")
print(textstat_frequency(dfm_bi) %>% head(200))} ## <<<VOCE PODE AJUSTAR ESTE VALOR PARA AUMENTAR OU DIMINUIR A QUANTIDADE DE TERMOS

# 9.2 Trigramas
{toks_tri <- tokens_ngrams(tokens_titulos, n = 3) ## <<<VOCE PODE AJUSTAR ESTE VALOR (n) para pegar mais combinações (4 = tetragrams, 5 = pentagrams, etc)
dfm_tri  <- dfm(toks_tri) %>%
  dfm_trim(min_termfreq = min_termfreq_trigram)

cat("\nTop trigramas:\n")
print(textstat_frequency(dfm_tri) %>% head(200))} ## <<<VOCE PODE AJUSTAR ESTE VALOR PARA AUMENTAR OU DIMINUIR A QUANTIDADE DE TERMOS NA NUVEM

############################################################
# 10. COLLOCATIONS (COM E SEM STOPWORDS) - ngrams com recortes mínimos
############################################################

# 10.1 Collocations “naturais” (com stopwords)
{raw_toks <- tokens(
  titulos_ascii,
  remove_punct   = TRUE,
  remove_numbers = TRUE
) %>%
  tokens_tolower()

#> Bigramas
cat("\nCollocations com stopwords (bigramas, min_count = 3):\n") ## <<<VOCE PODE AJUSTAR ESTE VALOR (min_count) para pegar combinações com valores de frequencia mínima ajustada ao seu acervo - é necessário mudar o valor na linha abaixo também
colloc_bi <- textstat_collocations(raw_toks, size = 2, min_count = 3) ## <<<VOCE PODE AJUSTAR ESTE VALOR (size) para pegar mais combinações (4 = tetragrams, 5 = pentagrams, etc)
print(head(colloc_bi, 100))} ## <<<VOCE PODE AJUSTAR ESTE VALOR PARA AUMENTAR OU DIMINUIR A QUANTIDADE DE TERMOS

#> Trigramas
{cat("\nCollocations com stopwords (trigramas, min_count = 3):\n") ## <<<VOCE PODE AJUSTAR ESTE VALOR (min_count) para pegar combinações com valores de frequencia mínima ajustada ao seu acervo - é necessário mudar o valor na linha abaixo também
colloc_tri <- textstat_collocations(raw_toks, size = 3, min_count = 3) ## <<<VOCE PODE AJUSTAR ESTE VALOR (size) para pegar mais combinações (4 = tetragrams, 5 = pentagrams, etc)
print(head(colloc_tri, 100))} ## <<<VOCE PODE AJUSTAR ESTE VALOR PARA AUMENTAR OU DIMINUIR A QUANTIDADE DE TERMOS

# 10.2 Collocations “filtrados” (sem stopwords, mais técnicos)

#> Bigramas
{cat("\nCollocations sem stopwords (bigramas, min_count = 2):\n")
colloc_bi_clean <- textstat_collocations(tokens_titulos, size = 2, min_count = 2)
print(head(colloc_bi_clean, 100))} ## <<<VOCE PODE AJUSTAR ESTE VALOR PARA AUMENTAR OU DIMINUIR A QUANTIDADE DE TERMOS

#> Trigramas
{cat("\nCollocations sem stopwords (trigramas, min_count = 2):\n")
colloc_tri_clean <- textstat_collocations(tokens_titulos, size = 3, min_count = 2) ## <<<VOCE PODE AJUSTAR ESTE VALOR (size) para pegar mais combinações (4 = tetragrams, 5 = pentagrams, etc)
print(head(colloc_tri_clean, 100))} ## <<<VOCE PODE AJUSTAR ESTE VALOR PARA AUMENTAR OU DIMINUIR A QUANTIDADE DE TERMOS

############################################################
# 11. FCM E REDE DE TERMOS
############################################################

# 11.1 Criar FCM com janela de contexto – tamanho da janela definido em "window_fcm" no tópico 3.2
fcmat <- fcm(tokens_titulos, context = "window", window = window_fcm)

# 11.2 Selecionar top termos por frequência
{top_terms <- freq %>%
  arrange(desc(frequency)) %>%
  slice(1:top_n_terms_fcm) %>%
  pull(feature)

fcm_small <- fcm_select(fcmat, pattern = top_terms)

# 11.3 Converter para matriz e extrair pares de coocorrência
m <- as.matrix(fcm_small)
diag(m) <- 0
inds <- which(upper.tri(m) & m > 0, arr.ind = TRUE)

cooc_pairs <- tibble(
  term1 = rownames(m)[inds[,1]],
  term2 = colnames(m)[inds[,2]],
  cooc  = m[inds]
) %>%
  arrange(desc(cooc))

cat("\nTop pares de coocorrência:\n")
print(head(cooc_pairs, 50))} # Mostra o valor de coocorrencia entre os termos 1 e 2

# Salvar coocorrências em CSV
{nome_contexto <- if (all(is.na(filtro_media))) {
  "geral"
} else {
  gsub("\\W+", "_", paste(filtro_media, collapse = "_"))
}

arquivo_cooc  <- paste0("cooc_pairs_top_", nome_contexto, ".csv")
readr::write_csv(cooc_pairs, arquivo_cooc)
cat("\nArquivo de coocorrências salvo em:", arquivo_cooc, "\n")}

# 11.4 Visualização de rede (janela gráfica)
# Você pode explorar esta parte rodando outras vezes, mudando o tamanho da janela de contexto (window_fcm)
# - e a frequência mínima (min_freq_network) no tópico 3.2.
quanteda.textplots::textplot_network(
  fcm_small,
  min_freq = min_freq_network
) #Pode demorar um pouco, mas este é seu principal resultado

############################################################
# 12. CORRELAÇÃO ENTRE TERMOS (FREQUÊNCIA POR DOCUMENTO)
############################################################

# 12.1 Matriz documento x termo com top_terms
{dfm_top <- dfm_select(dfm_uni, pattern = top_terms)
mat     <- as.matrix(dfm_top)

# Remover colunas sem variação
mat <- mat[, apply(mat, 2, sd) > 0, drop = FALSE]

# 12.2 Correlação entre termos
cor_mat  <- cor(mat, use = "pairwise.complete.obs")
inds_cor <- which(upper.tri(cor_mat), arr.ind = TRUE)

cor_pairs <- tibble::tibble(
  term1 = colnames(cor_mat)[inds_cor[,1]],
  term2 = colnames(cor_mat)[inds_cor[,2]],
  cor   = cor_mat[inds_cor]
) %>%
  arrange(desc(cor))

cat("\nTop pares de termos mais correlacionados:\n")
print(head(cor_pairs, 30))} # Mostra o valor de correlação entre os termos 1 e 2

# Salvar correlações em CSV
{arquivo_cor <- paste0("term_correlations_top_", nome_contexto, ".csv")
readr::write_csv(cor_pairs, arquivo_cor)
cat("\nArquivo de correlações salvo em:", arquivo_cor, "\n")}

############################################################
# 13. FILTRAGEM PÓS-ANÁLISE (POR TERMOS NO TÍTULO) 
############################################################

# Defina aqui os termos que você identificou nas análises de rede, collocations etc.
# Exemplo: palavras_busca <- c("termo1", "termo2")
palavras_busca <- c("morta", "encontrada")   # <-- ALTERE AQUI conforme desejar

# Criar/atualizar uma coluna com o título normalizado (sem acentos e em minúsculas)
{dados_unicos <- dados_unicos %>%
  mutate(
    titulo_ascii = stringi::stri_trans_general(title, "Latin-ASCII"),
    titulo_ascii = tolower(titulo_ascii)
  )

# 13.1 Filtragem com lógica AND – mantém notícias cujo título contém TODAS as palavras
mascara_and <- apply(
  sapply(palavras_busca, function(p){
    stringr::str_detect(dados_unicos$titulo_ascii, fixed(tolower(p)))
  }),
  1,
  all
)

subamostra_and <- dados_unicos[mascara_and, ]

cat("\nFiltragem AND – títulos contendo TODOS os termos:\n")
cat("Termos de busca:", paste(palavras_busca, collapse = ", "), "\n")
cat("Número de notícias selecionadas:", nrow(subamostra_and), "\n\n")}

# Visualizar algumas linhas
print(head(subamostra_and[, c("media_name", "publish_date", "title")], 20))

# Salvar subamostra AND em CSV
{arquivo_and <- paste0(
  "subamostra_titulos_AND_",
  nome_contexto,
  "_",
  paste(palavras_busca, collapse = "_"),
  ".csv"
)
readr::write_csv(subamostra_and, arquivo_and)
cat("Subamostra AND salva em:", arquivo_and, "\n")}

# ----------------------------------------------------------
# 13.2 Filtragem com lógica OR (OPCIONAL, RODAR APENAS SE NECESSÁRIO)
#      Mantém notícias cujo título contém PELO MENOS um dos termos.
#      Pule para o item 14 caso não seja necessário.
# ----------------------------------------------------------

{mascara_or <- apply(
  sapply(palavras_busca, function(p){
    stringr::str_detect(dados_unicos$titulo_ascii, fixed(tolower(p)))
  }),
  1,
  any
)

subamostra_or <- dados_unicos[mascara_or, ]

cat("\nFiltragem OR – títulos contendo PELO MENOS um dos termos:\n")
cat("Termos de busca:", paste(palavras_busca, collapse = ", "), "\n")
cat("Número de notícias selecionadas:", nrow(subamostra_or), "\n\n")}

# Visualizar algumas linhas
print(head(subamostra_or[, c("media_name", "publish_date", "title")], 20))

# (Opcional) Salvar subamostra OR em CSV
{arquivo_or <- paste0(
  "subamostra_titulos_OR_",
  nome_contexto,
  "_",
  paste(palavras_busca, collapse = "_"),
  ".csv"
)
readr::write_csv(subamostra_or, arquivo_or)
cat("Subamostra OR salva em:", arquivo_or, "\n")}

############################################################
# 14. FIM
############################################################
#Referencia dos pacotes usados
lapply(pacotes_cran, citation) ## <<<Não é necessário citar o pacote "base64enc". Lembre de citar o nosso artigo


#Recado final
{.msg_cod <- paste0(
  "8J+OiSBGaW0gZGEgYW7DoWxpc2UhIPCfjokKUGFyYWLDqW5zLCBkZXUgdHVkbyBjZXJ0by4KQWdv",
  "cmEgdm9jw6ogcG9kZSBjb25mZXJpciBvcyByZXN1bHRhZG9zIG5hIHBhc3RhOgpQQVNUQV9BUVVJ",
  "CgpTZSBhaW5kYSB0aXZlciBkw7p2aWRhcywgbWUgZXNjcmV2YToKdmljdG9yZmVsaXBlNjIudmZA",
  "Z21haWwuY29tICDwn5iK"
)
msg_txt <- rawToChar(base64enc::base64decode(.msg_cod))
msg_txt <- sub("PASTA_AQUI", getwd(), msg_txt, fixed = TRUE)
plot.new()
par(mar = c(1,1,1,1))
linhas <- strsplit(msg_txt, "\n")[[1]]
ys <- seq(0.80, 0.20, length.out = length(linhas))

for(i in seq_along(linhas)){
  cex_i <- ifelse(i == 1, 2, 1.3)  # título maior
  text(0.5, ys[i], linhas[i], cex = cex_i)
} }
