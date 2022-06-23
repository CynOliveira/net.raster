#token inicial do projeto: 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

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
## criar um branch para função b (interaction strength)
usethis::pr_init(branch = "fun_b")

## criar outro branch para função AS (interaction strength asymmetry)
usethis::pr_init(branch = "fun_AS")

####FUTURAMENTE, após lidar com os branches

## abrir pull request
usethis::pr_push()

## após o aceite o pull request os branches estão fundidos
# apagar o branch criado
usethis::pr_finish()

