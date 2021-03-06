name := "meow"

version := "0.1"

scalaVersion := "2.13.4"

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.1" cross CrossVersion.full)

githubOwner := "linyxus"
githubRepository := "meow"
githubTokenSource := TokenSource.GitConfig("github.token") || TokenSource.Environment("GITHUB_TOKEN")
