% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api.R
\name{baixar}
\alias{baixar}
\title{Baixa um arquivo de audio mp3}
\usage{
baixar(arq = NULL, link = "http://esaj.tjsc.jus.br/cposgtj/open.do")
}
\arguments{
\item{arq}{character vector com um caminho para o arquivo. Se \code{NULL},
salva num arquivo temporario.}
}
\value{
nome do arquivo em que o mp3 foi salvo.
}
\description{
Recebe a mesma coisa que retorna e salva um
arquivo mp3 no caminho especificado
}

