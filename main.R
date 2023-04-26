library('tidyverse')
library('readxl')
library('plm')
library('AER')
install.packages('AER')
setwd("C:\\Users\\Dell\\Documents\\GitHub\\emilly_work")


# Coletando os dados:

df2 <- read.csv(file = "microdados_enade_2018econ.csv",fileEncoding = 'UTF-16LE',sep = '\t')
colnames(df2)
dim(df2)

# ------------------

# Filtrando apenas cursos de Economia:

# dudas = df2[df2['Co.Curso']==5691,] # Código do curso de Economia no Enade
df_feat <- df2[df2['Co.Grupo']==13,] # Código 13 para cursos de Economia

# -----------------

# Selecionando variáveis importantes:

df_feat <- df_feat %>% select('Co.Curso','Nt.Ger','Nt.Fg','Co.Modalidade',
                              'Co.Grupo','Co.Uf.Curso','Nu.Idade','Tp.Sexo',
                              'Co.Turno.Graduacao','Tp.Pres','Tp.Pr.Ger',
                              'Qe.I01','Qe.I02','Qe.I03','Qe.I11','Qe.I15',
                              'Qe.I23','Qe.I17','Qe.I18')

str(df_feat)
dim(df_feat)

# Renomeando elas:

df_feat <- df_feat %>% rename('Codigo_curso' = 'Co.Curso', 
                              'Nota_geral' = 'Nt.Ger',
                              'Nota_form_geral' = 'Nt.Fg',
                              'Curso_grupo' = 'Co.Grupo',
                              'Estado_curso' = 'Co.Uf.Curso',
                              'Idade' = 'Nu.Idade',
                              'Sexo' = 'Tp.Sexo',
                              'Turno_graduacao' = 'Co.Turno.Graduacao',
                              'Tipo_presenca_prova' = 'Tp.Pres',
                              'Tipo_presenca_geral' = 'Tp.Pr.Ger',
                              'Estado_civil' ='Qe.I01',
                              'Raça' = 'Qe.I02',
                              'Nacionalidade' = 'Qe.I03',
                              'Programa_governo_facul' = 'Qe.I11',
                              'Politicas_inclusao_facul' = 'Qe.I15',
                              'Esforco_aluno' = 'Qe.I23',
                              'Tipo_ensino_medio' = 'Qe.I17',
                              'Modalidade_ensino_medio' = 'Qe.I18',
                              'EAD' = 'Co.Modalidade')

View(df_feat)

colnames(df_feat)
# --------------------

# Retirando os NA's

df_feat <- df_feat[df_feat$'Nota_geral' != '',]
df_feat <- df_feat[df_feat$Tipo_presenca_prova != 222,]
df_feat$Nota_geral <- str_replace(df_feat$Nota_geral, ",",".") 

str(df_feat)
count(df_feat, Estado_civil, sort = TRUE)

df_feat <- df_feat[df_feat$Estado_civil != '',]
df_feat <- df_feat[df_feat$Raça != '',]
df_feat <- df_feat[df_feat$Nacionalidade != '',]
df_feat <- df_feat[df_feat$Programa_governo_facul != '',]
df_feat <- df_feat[df_feat$Politicas_inclusao_facul != '',]
df_feat <- df_feat[df_feat$Esforco_aluno != '',]
df_feat <- df_feat[df_feat$Tipo_ensino_medio != '',]
df_feat <- df_feat[df_feat$Modalidade_ensino_medio != '',]
count(df_feat, EAD, sort = TRUE)

#df_feat <- df_feat[complete.cases(df_feat),]
row.names(df_feat) <- NULL
summary(df_feat)
# --------------------
# Transformando as variáveis corretamente:

dim(df_feat)
str(df_feat)

df_feat$Nota_geral <- as.numeric(df_feat$Nota_geral)

df_feat$Estado_civil <- as.factor(df_feat$Estado_civil)
df_feat$Raça <- as.factor(df_feat$Raça)
df_feat$Sexo <- as.factor(df_feat$Sexo)
df_feat$Nacionalidade <- as.factor(df_feat$Nacionalidade)
df_feat$Esforco_aluno <- as.factor(df_feat$Esforco_aluno)
df_feat$Tipo_ensino_medio <- as.factor(df_feat$Tipo_ensino_medio)
df_feat$Modalidade_ensino_medio <- as.factor(df_feat$Modalidade_ensino_medio)
df_feat$Programa_governo_facul <- as.factor(df_feat$Programa_governo_facul)
df_feat$Politicas_inclusao_facul <- as.factor(df_feat$Politicas_inclusao_facul)
df_feat$Turno_graduacao <- as.factor(df_feat$Turno_graduacao)
df_feat$EAD <- as.factor(df_feat$EAD)

str(df_feat)
colnames(df_feat)
# --------------------

# Rodando primeiro modelo:

df_model <- df_feat %>% select(-c(Curso_grupo,Codigo_curso,Nota_form_geral))

df_model <- df_model[df_model$Nota_geral > 0,]


library(ggcorrplot)
model.matrix(~0+., data=df_model) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=2)

help(ggcorrplot)

library(lme4)

colnames(df_model)
hist(df_model$Nota_geral)

# Endogenous - Variáveis Causais = EAD. 
# Instruments - Arrumam o vicio das variáveis Endogenas Esforco_aluno, turno_graduacao

# modelo basico lm
# modelo ivreg 
# modelo ivreg 2
# modelok

modelbasic <- lm(Nota_geral ~ EAD, data=df_model) ; summary(modelbasic)


# Utilizamos o ivreg1 porque o teste de Wu-Hausman foi significativo, 
# portanto à diferenças nas estimativas.

# Os instrumentos utilizados no ivreg foram satisfatórios.



ivreg1 <- ivreg(df_model$Nota_geral ~ EAD + Estado_civil 
               + Raça
               + Idade
               + Tipo_ensino_medio
               + Estado_curso
               + Nacionalidade
               + Sexo
               + Modalidade_ensino_medio | Estado_civil 
               + Raça
               + Idade
               + Tipo_ensino_medio
               + Estado_curso
               + Nacionalidade
               + Sexo
               + Modalidade_ensino_medio + Esforco_aluno, data = df_model)

summary(ivreg1, diagnostics = TRUE)
help(summary)

ivreg2 <- ivreg(df_model$Nota_geral ~ EAD + Estado_civil 
                + Raça
                + Idade
                + Tipo_ensino_medio
                + Estado_curso
                + Nacionalidade
                + Sexo
                + Modalidade_ensino_medio | Estado_civil 
                + Raça
                + Idade
                + Tipo_ensino_medio
                + Estado_curso
                + Nacionalidade
                + Sexo
                + Modalidade_ensino_medio + Esforco_aluno + Turno_graduacao, data = df_model)

summary(ivreg2, diagnostics = TRUE)

ivreg

modelk <- lm(df_model$'Nota_geral' ~ Estado_civil + Turno_graduacao 
             + Raça + Estado_civil + Idade + EAD 
             + Esforco_aluno
             + Tipo_ensino_medio
             + Estado_curso
             + Nacionalidade
             + Sexo
             + Modalidade_ensino_medio,data = df_model)

summary(modelk)

