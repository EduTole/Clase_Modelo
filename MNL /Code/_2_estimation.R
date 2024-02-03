rm(list = ls())
setwd("C://Users//et396//Dropbox//Docencia//Educate//Econometria//Clase_Modelo//Aplicacion")

# Paquetes para instalar y poder usarlos
paquetes_set <- c("readstata13", "dplyr", "tidyverse","sjlabelled",
                  "survey", "stargazer", "caret", "foreign","readr",
                  "mfx","texreg")
# install.packages(paquetes_set)
lapply(paquetes_set, library, character.only=TRUE)

# Carga de Data     ------------------------------------------------------
base <- read.dta13("base_empleo_2021.dta", nonint.factors = FALSE )

base %>% dim()
base %>% names()
base %>% str()

# Base de datos -------------------------------
# Regresion MPL ---------------------
#formula <- rpublica ~ rmujer+rinfo+lnr6+redad
table(base$rpublica)

formula <- rpublica ~ rmujer+rinfo+lnr6+redad +factor(reduca_niv) 
m1 <- lm(formula , data = base)
summary(m1)

# Regresion Probit -----------------------
m2 <- glm(formula, data = base, family =binomial(link="probit"))
summary(m2)

# Modelo Logit --------------------------------
m3 <- glm(formula , data = base, family =binomial(link="logit"))
summary(m3)

# Presentacion de resultados --------------------------------
stargazer(m1, m2,m3,
          title="Modelos No lineales", type="text", 
          df=FALSE, digits=4)

# Efectos marginales ----------
mprobit <- probitmfx( formula ,
                 data=base, atmean=FALSE)
mlogit <- logitmfx(formula ,
               data=base, atmean=FALSE)

# Generar table en latex
# Preparar la tabla
modelss = list(mprobit$fit, mlogit$fit)
coefs   = list(c(0, mprobit$mfxest[, 1]), c(0, mlogit$mfxest[, 1])  )
ses     = list(c(0, mprobit$mfxest[, 2]), c(0, mlogit$mfxest[, 2])  )
pvals   = list(c(0, mprobit$mfxest[, 4]), c(0, mlogit$mfxest [, 4]) )

tr1 = texreg(modelss,
             override.coef = coefs,
             override.se = ses,
             override.pval = pvals,
             omit.coef = "(Intercept)",
             caption.above = TRUE,
             caption = "Models Explaining Poverty Participation. Marginal
             Effects",
             digits = 4, 
             #dcolumn = TRUE,
             #custom.note = "\%stars.",
             stars = c(0.01, 0.05, 0.1),
             custom.model.names = c("Probit (mean)", "Logit (mean)"),
             return.string = TRUE)
tr1



