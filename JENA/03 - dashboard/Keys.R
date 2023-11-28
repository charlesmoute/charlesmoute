# Key de connexion a serveur Kobo

# Pour ajouter des variables d'environnement
usethis::edit_r_environ(scope = "project")

# KOBOTOOLBOX_TOKEN="48ef00b7692891eb0c0b7e07989ddabf63d8a693"
# KOBOTOOLBOX_URL="https://kf.kobotoolbox.org/"

#Pour initier l'utilisation de git_hub sur ce projet
# https://www.book.utilitr.org/03_fiches_thematiques/fiche_git_utilisation
usethis::use_git()

#Pour exclure certains fichiers
#.gitignore
usethis::edit_git_ignore(scope = "project")
