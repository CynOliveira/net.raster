#################### Disciplina: Ciência Replicável ######################
##################### e Criação de Pacotes no R  #########################
############## Universidade Estadual de Santa Cruz (UESC) ################
################ Programa de Pós Graduação em Ecologia e #################
################# Conservação da Biodiversidade (PPGECB) #################

## Dr.Neander M. Heming - PPGECB - UESC
## www.researchgate.net/profile/Neander_Heming
## neanderh@yahoo.com.br

### Introdução ao Git e Github no RStudio ---------------------------------

### Carregar pacote "usethis"

options(install.lock = FALSE)
install.packages("usethis")
library(usethis)

### Configurações globais ---------------------------------
### Configurando git
# Só precisa fazer uma única vez no computador
# Aqui usando RStudio, mas pode ser feito por linha de comando
# Use o mesmo email usado no GitHub
usethis::use_git_config(user.name = "Cynthia Valéria Oliveira", # Seu nome
                        user.email = "cynthia.valeria.oliveira@gmail.com") # Seu email
## para checar as configurações
gert::git_config_global()

### Configurando o GitHub + RStudio
# Criar um novo token no GitHub:
usethis::create_github_token()
# A função vai abrir uma página do GitHub para a criação de um novo token (PAT)
# Você pode alterar o nome do token para identificar o computador vinculando
# Crie o novo token.
# Copie o token gerado:

### inserir token (PAT)
# pode ser feito alterando o arquivo .Renviron
usethis::edit_r_environ()
gitcreds::gitcreds_set()
gitcreds::gitcreds_get()


##### Versionando novo projeto/repo/pacote  ---------------------------------
# aqui vamos criar um projeto e adicionar o controle de versão com git e github
# - projeto
# - git
# - github

### Criando um projeto
# Identifique o caminho completo ate a pasta desejada
file.choose()
## confira se esta tudo OK
list.files("C:/Users/Cynthia/Documents/- Estudos/- Pesquisa/- Doutorado/Disciplinas_cursos/8 Cienc_repl_packs")


## atenção ao nome do projeto pois será usado como nome do repositorio no GitHub
# - esse será o nome do pacote!
# - não deve ser o nome de um projeto já existente para evitar conflitos!
# criei o projeto chamado test_package
# Esse será o nome da pasta criada, do repositório no GitHub e do pacote!
# apos rodar o comando abaixo, o RStudio abrirá outra
# janela, com o novo projeto criado
usethis::create_project("C:/Users/Cynthia/Documents/- Estudos/- Pesquisa/- Doutorado/Disciplinas_cursos/8 Cienc_repl_packs/renm")


### Adicionando git
usethis::use_git()
# escolha se quer fazer o commit dos arquivos.
# Se sim: Definitely ou Absolutely ou Yeah

# escolha a opção para reiniciar o RStudio


### Configurando o github
usethis::use_github()
# Repositório criado!

### Algumas precauções
# Get a situation report on your current Git/GitHub status.
# Useful for diagnosing problems.
usethis::git_sitrep()

# Vaccinate your global gitignore file
# Adds .DS_Store, .Rproj.user, .Rdata, .Rhistory, and .httr-oauth to your
# global (a.k.a. user-level) .gitignore. This is good practice as it
# decreases the chance that you will accidentally leak credentials to GitHub
usethis::git_vaccinate()


### Adicionando arquivos ---------------------------------
## Criar um arquivo README.md
usethis::use_readme_md()
# O arquivo será criado e aberto, para ser editado e salvo.


### Ignorar arquivos no git ---------------------------------
usethis::use_git_ignore(c("proj_setup/"))


### branch - merge ---------------------------------
## criar um branch
usethis::pr_init(branch = "teste")

## USAR ISSO PRA CRIAR FUNÇÃO ##
#usethis::pr_init(branch = "fun1")

## para conseguir sincronizar o branch com o GitHub,é preciso fazer o push com o pr_push()
## abrir pull request
usethis::pr_push()
## vai abrir a página o GitHub

## após o aceite o pull request os branches estão fundidos
# apagar o branch criado
usethis::pr_finish()

## RESOLVER ERRO de "commit message did not conform to UTF-8 ##

