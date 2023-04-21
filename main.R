library('tidyverse')
library('readxl')
library('plm')

setwd("C:\\Users\\vinic\\Documents\\Projetos\\emilly_work")


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

dim(df_feat)

# Renomeando elas:

df_feat <- df_feat %>% rename('Codigo_curso' = 'Co.Curso', 
                              'Nota_geral' = 'Nt.Ger',
                              'Nota_form_geral' = 'Nt.Fg',
                              'EAD' = 'Co.Modalidade',
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
                              'Modalidade_ensino_medio' = 'Qe.I18')

colnames(df_feat)

# --------------------

# Retirando os NA's

df_feat <- df_feat[df_feat$'Nota_geral' != '',]

df_feat <- df_feat[complete.cases(df_feat),]

summary(df_feat)
# --------------------
# Transformando as variáveis corretamente:

dim(df_feat)
str(df_feat)

df_feat$Nota_geral <- as.numeric(df_feat$Nota_geral)
df_feat$Nota_form_geral <- as.numeric(df_feat$Nota_form_geral)

df_feat$Estado_civil <- as.factor(df_feat$Estado_civil)
df_feat$Raça <- as.factor(df_feat$Raça)
df_feat$Sexo <- as.factor(df_feat$Sexo)
df_feat$Nacionalidade <- as.factor(df_feat$Nacionalidade)
df_feat$Esforco_aluno <- as.factor(df_feat$Esforco_aluno)
df_feat$Tipo_ensino_medio <- as.factor(df_feat$Tipo_ensino_medio)
df_feat$Modalidade_ensino_medio <- as.factor(df_feat$Modalidade_ensino_medio)
df_feat$Programa_governo_facul <- as.factor(df_feat$Programa_governo_facul)
df_feat$Politicas_inclusao_facul <- as.factor(df_feat$Politicas_inclusao_facul)

colnames(df_feat)

# --------------------

hist(df_feat$Nota_geral)

# Rodando primeiro modelo:

df_model <- df_feat %>% select(-c(Curso_grupo,Codigo_curso,Nota_form_geral,
                                  Tipo_presenca_prova,Modalidade_ensino_medio,
                                  Tipo_presenca_geral,Tipo_ensino_medio,
                                  Nacionalidade))

str(df_model)


library(ggcorrplot)
model.matrix(~0+., data=df_model) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=2)


library(lme4)

colnames(df_model)
hist(df_model$Nota_geral)
modelm <- glmer(df_model$'Nota_geral' ~ Estado_civil + Turno_graduacao 
             + Raça + Estado_civil + Programa_governo_facul + EAD + Idade 
             + Politicas_inclusao_facul
             + Esforco_aluno,data = df_model,
             family = 'beta')

modelm2 <- glm(formula = Nota_geral ~ Estado_civil + Turno_graduacao 
                 + Raça + Estado_civil + Programa_governo_facul + EAD + Idade 
                 + Politicas_inclusao_facul
                 + Esforco_aluno,data = df_model,family = gaussian)

summary(modelm2)

help(glm)
help(plm)
summary(modelm)
# -----------------------------------------------------------------------------
df_model <- tibble::rowid_to_column(df_model, "ID")
head(df_model)




modelm <- plm(df_model$'Nota_geral' ~ Estado_civil + Turno_graduacao +
               Raça + Estado_civil + Programa_governo_facul + EAD + Idade + Politicas_inclusao_facul
             + Esforco_aluno,data = df_model,model='within',index=Programa_governo_facul)

modelm <- plm(df_model$'Nota_geral' ~ Estado_civil + Turno_graduacao +
                Raça + Estado_civil + Programa_governo_facul + EAD + Idade + Politicas_inclusao_facul
              + Esforco_aluno,data = df_model,model='random',index=)




model0 <- plm(df_model$'Nota_geral' ~ ., data = df_model,na.action = na.omit,model = 'within')

model1 <- plm(df_model$'Nota_geral' ~ ., data = df_model,na.action = na.omit,model = 'random')

phtest(modelm,model1)

summary(model0)
anova(model0)

help(plm)
