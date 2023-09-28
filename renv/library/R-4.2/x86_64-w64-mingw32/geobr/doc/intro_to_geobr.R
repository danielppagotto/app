## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = identical(tolower(Sys.getenv("NOT_CRAN")), "true"),
  out.width = "100%"
)

## ----eval=FALSE, message=FALSE, warning=FALSE---------------------------------
#  # From CRAN
#  install.packages("geobr")
#  
#  # Development version
#  utils::remove.packages('geobr')
#  devtools::install_github("ipeaGIT/geobr", subdir = "r-package")
#  

## ----eval=TRUE, message=FALSE, warning=FALSE, results='hide'------------------
library(geobr)
library(ggplot2)
library(sf)
library(dplyr)

## ----eval=TRUE, message=FALSE, warning=FALSE----------------------------------
# Available data sets
datasets <- list_geobr()

head(datasets)


## ----eval=TRUE, message=FALSE, warning=FALSE----------------------------------
# State of Sergige
state <- read_state(
  code_state="SE",
  year=2018,
  showProgress = FALSE
  )

# Municipality of Sao Paulo
muni <- read_municipality(
  code_muni = 3550308, 
  year=2010, 
  showProgress = FALSE
  )

plot(muni['name_muni'])

## ----eval=TRUE, message=FALSE, warning=FALSE, results='hide'------------------
# All municipalities in the state of Minas Gerais
muni <- read_municipality(code_muni= "MG", 
                          year=2007,
                          showProgress = FALSE)

# All census tracts in the state of Rio de Janeiro
cntr <- read_census_tract(
  code_tract = "RJ", 
  year = 2010,
  showProgress = FALSE
  )

head(muni)

## ----eval=TRUE, message=FALSE, warning=FALSE----------------------------------
# read all intermediate regions
inter <- read_intermediate_region(
  year=2017,
  showProgress = FALSE
  )

# read all states
states <- read_state(
  year=2019, 
  showProgress = FALSE
  )

head(states)

## ----eval=TRUE, message=FALSE, warning=FALSE, fig.height = 8, fig.width = 8, fig.align = "center"----
# Remove plot axis
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

# Plot all Brazilian states
ggplot() +
  geom_sf(data=states, fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
  labs(subtitle="States", size=8) +
  theme_minimal() +
  no_axis


## ----states br, eval=FALSE, echo=FALSE, message=FALSE, out.width='100%'-------
#  knitr::include_graphics("https://github.com/ipeaGIT/geobr/blob/master/r-package/inst/img/states_br.png?raw=true")

## ----eval=TRUE, message=FALSE, warning=FALSE, fig.height = 8, fig.width = 8, fig.align = "center"----

# Download all municipalities of Rio
all_muni <- read_municipality(
  code_muni = "RJ", 
  year= 2010,
  showProgress = FALSE
  )

# plot
ggplot() +
  geom_sf(data=all_muni, fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
  labs(subtitle="Municipalities of Rio de Janeiro, 2000", size=8) +
  theme_minimal() +
  no_axis


## ----munis rio, eval=FALSE, echo=FALSE, message=FALSE, out.width='100%'-------
#  knitr::include_graphics("https://github.com/ipeaGIT/geobr/blob/master/r-package/inst/img/munis_rj.png?raw=true")

## ----eval=TRUE, message=FALSE, warning=FALSE, results='hide'------------------
# Read data.frame with life expectancy data
df <- utils::read.csv(system.file("extdata/br_states_lifexpect2017.csv", package = "geobr"), encoding = "UTF-8")

states$name_state <- tolower(states$name_state)
df$uf <- tolower(df$uf)

# join the databases
states <- dplyr::left_join(states, df, by = c("name_state" = "uf"))


## ----eval=TRUE, message=FALSE, warning=FALSE, fig.height = 8, fig.width = 8, fig.align = "center"----
ggplot() +
  geom_sf(data=states, aes(fill=ESPVIDA2017), color= NA, size=.15) +
    labs(subtitle="Life Expectancy at birth, Brazilian States, 2014", size=8) +
    scale_fill_distiller(palette = "Blues", name="Life Expectancy", limits = c(65,80)) +
    theme_minimal() +
    no_axis


## ----life expectancy states, eval=FALSE, echo=FALSE, message=FALSE, out.width='100%'----
#  knitr::include_graphics("https://github.com/ipeaGIT/geobr/blob/master/r-package/inst/img/life_expect_states.png?raw=true")

