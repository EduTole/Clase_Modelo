cls 
clear all

*--------------------------------------------------
*Paso 1: Direccion de carpeta
*--------------------------------------------------
cd "C:\Users\et396\Dropbox\Docencia\Educate\Econometria\Clase_Modelo\Aplicacion" // Clase 	

*--------------------------------------------------
*Paso 2: Carga de data
*--------------------------------------------------

u "base_empleo_2021.dta",clear

gl Zs "rmujer rinfo lnr6 redad reduca_niv1-reduca_niv4"
gl Qs "rmujer rinfo lnr6 redad"
gl Xs "rmujer rinfo lnr6 redad i.reduca_niv"
sum rpublica $Xs
*Comando de tabla de distribucion de la variable y 
tab rpublica 

*Pregunta 1
*--------------------------------------------------------------
probit rpublica $Qs 
probit rpublica $Xs 

*Pregunta 2
*--------------------------------------------------------------
*MCO --> Modelo de probabilidad Lineal
reg rpublica $Xs , r
predict y_predict , xb

*PROBIT
probit rpublica $Xs 
*efecto marginal
margins, dydx(*) post


*LOGIT
logit rpublica $Xs
*efecto marginal
margins, dydx(*) post


*Pregunta 3-4
*--------------------------------------------------------------
*Efecto Marginal
*-----------------------------------------------------
*Efectos marginales
quietly probit rpublica $Xs
quietly margins, dydx(*) post

*Pregunta 5-6
*--------------------------------------------------------------
*Modelo probit y efectos marginales
quietly probit rpublica $Xs
quietly margins, dydx(*) post
outreg2 using "Tabla_1.xls", append ctitle(Marginal Effects Probit) keep(rmujer lnr6 redad reduca_niv rinfo ) addtext(Region FE, No)  addnote(Fuente: ENAHO -2021)

quietly logit rpublica $Xs
quietly margins, dydx(*) post
outreg2 using "Tabla_1.xls", append ctitle(Marginal Effects Logit) keep(rmujer lnr6 redad reduca_niv rinfo ) addtext(Region FE, No)  addnote(Fuente: ENAHO -2021)




