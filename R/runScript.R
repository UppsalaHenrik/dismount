

createPlotlyPlot <- function(){

install.packages("plotly")

library(plotly)

py <- plotly(username='uppsalahenrik', key='ja9b5copw1')


# Params at saddle nr 2

V1 <- 10.3597319
Q <- 4.280909


# V1 <- 6.77991978954211
# Q <- 18.8992833837428

paramValue1 <- V1
paramValue2 <- Q

paramName1 <- "V1"
paramName2 <- "Q"

modFileName <- "run111_FOCEI_FixedParams_saddle_maxev0_9.mod"


# Fixed params at saddle nr 1
# fixedParams <- c(8.34296436272427,  2.88271289190858,  
#                  0.0192783230476275,	0.0268572730251153, 
#                  0.166234293524786,  0.0533595674510603,	
#                  0.0322201530323241,	0.238275122057642,	1)

# Fixed params at saddle nr 2
fixedParams <- c(5.633234,  2.893139,  
                 0.0191713979,  0.014252238, 
                 0.2215548,  0.05484621,	
                 0.0210258327,	0.008060689,	1)


# Fixed params at "global minimum"
# fixedParams <- c(7.1703494460480321E+00,  2.9140037479895606E+00,  
#                  1.8694905666959707E-02,  1.9399078120883668E-02, 
#                  1.6503501173785581E-01,  5.4335207568558072E-02,	
#                  2.5513704446167820E-02,	1.6214241582692102E-01,	1)

names(fixedParams) <- c("V2", "TVCL", "CLCR_CLR", "WT_V1", "prop", "OMEGA(1,1)", 
                        "OMEGA(2,1)", "OMEGA(2,2)", "SIGMA(1,1)")



rawresInputList <- createRawresInput(paramValue1, paramValue2,
                                     paramName1, paramName2, 
                                     0.5, 2, 80, 2, fixedParams)

dirName <- runParaRetries(modFileName, rawresInputList[[1]], local = TRUE)

rawresPath <- findRawres(dirName)

ofvVector <- parseOfvs(rawresPath)

url <- createPlotlyObj(ofvVector, rawresInputList[[2]], rawresInputList[[3]])

print(url)
}