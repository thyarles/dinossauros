# Módulo para leitura das tabelas auxiliares fornecidas pelo SISNAC
#
# CADMUN.DBF  CID10.DBF     TABOCUP.DBF  TABUF.DBF
# CADMUN.xls  CIDCAP10.DBF  TABPAIS.DBF

# Leitura da library
import('foreign')

# Leitura das UFs
export('uf')
uf <- function() {
   return(read.dbf('tabelas/TABUF.DBF'))
}

# Leitura dos países
export('pais')
pais <- function() {
  return(read.dbf('tabelas/TABPAIS.DBF'))
}

# Leitura das ocupações
export('ocupacao')
ocupacao <- function() {
  return(read.dbf('tabelas/TABOCUP.DBF'))
}

# Leitura dos capítulos das CIDs
export('capCid')
capCid <- function() {
  return(read.dbf('tabelas/CIDCAP10.DBF'))
}

# Leitura das CIDs
export('cid')
cid <- function() {
  return(read.dbf('tabelas/CID10.DBF'))
}

# Leitura dos Municípios
export('municipio')
municipio <- function() {
  return(read.dbf('tabelas/CADMUN.DBF'))
}

