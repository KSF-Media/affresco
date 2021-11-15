{-

servers: Apps to be deployed to AppEngine
all: list of all servers

Note: When adding a new server please also update the list of all servers
TODO: Figure out a way to generate all from servers.

-}
{ app-article-server = ./app-servers/app-article.dhall
, mosaico = ./app-servers/mosaico.dhall
}
