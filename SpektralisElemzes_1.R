library( lattice )

## Elmélet és egy (nem-közgazdasági) motiváló példa ##

HomTmpDir <- tempdir()
HomTmpFile <- tempfile( tmpdir = HomTmpDir )
download.file( paste0( "https://www.met.hu/zip_downloads.php?fn=/eghajlat/magyarorszag_eghajlata/",
                       "eghajlati_adatsorok/Budapest/adatok/eghajlati_adatsor_Budapest1901-2010.zip" ), HomTmpFile )
unzip( HomTmpFile, exdir = HomTmpDir )

RawData <- read.delim( paste0( HomTmpDir, "\\havi_adatok\\BP_M_ta.txt" ), sep = ";", dec = ".", stringsAsFactors = FALSE )
#idojaras adatok
unlink( HomTmpFile )

RawData$Datum <- as.Date( paste0( RawData$X.datum, "-15" ) )
RawData <- RawData[ -nrow( RawData ), ] # csak hogy páratlan legyen

# m_ta	havi középhőmérséklet
# m_tax	a legmagasabb napi középhőmérséklet a hónapban
# m_taxd	a legmagasabb napi középhőmérséklet napja
# m_tan	a legalacsonyabb napi középhőmérséklet a hónapban
# m_tand	a legalacsonyabb napi középhőmérséklet napja

xyplot( m_ta ~ Datum, data = RawData, type = "l" )

spectrum( RawData$m_ta ) #periodogramm, fuggoleges tengely logaritmikus, vizsintes tengely 0tol 0.5ig megy, alul nullatol piig megy
#12 honapnal van eves periodocitas, eyert ott kiugro a szinuszoid erteke
spectrum( RawData$m_ta, spans = 7 )

BpHomSpec <- spectrum( RawData$m_ta )
str( BpHomSpec )
xyplot( spec ~ freq, data = BpHomSpec, type = "l" ) #nem logaritmikusan
xyplot( spec ~ freq, data = BpHomSpec, type = "l", scales = list( y = list ( log = 10 ) ) )
xyplot( spec ~ (1/freq), data = BpHomSpec, type = "l", scales = list( y = list ( log = 10 ) ) )#1/freq, ami a periodiusido 
xyplot( spec ~ (1/freq), data = BpHomSpec, type = "l", scales = list( y = list ( log = 10 ) ), xlim = c( 0, 20 ) ) #havi kozephomerseklet periodikus, alul periodusido

curve( cos( pi/2*x ), xlim = c( 0, 9 ), ylab = "", xlab = "t" )
curve( cos( 3*pi/2*x ), xlim = c( 0, 9 ), add = TRUE, col = "blue" )

N <- nrow( RawData )
M <- ( N-1 )/2
omegas <- ( 1:M )*2*pi/N
predCos <- outer( ( 0:(N-1) ), omegas, function( x, y ) cos( x*y ) ) #diadikus szorzat, de azert nem matrixszorzast irtunk, mert outer engedi hogy ne csak szorzas hanem pl cos legyen
dim( predCos )
predSin <- outer( (0:(N-1) ), omegas, function( x, y ) sin( x*y ) )
pred <- data.frame( predCos, predSin  )
dim( pred )
colnames( pred ) <- c( paste0( "cos", 1:M ), paste0( "sin", 1:M ) )
pred$m_ta <- RawData$m_ta

SpecReg <- lm( m_ta ~ ., data = pred )
summary( SpecReg ) #nincsenek sztenderd hibak, es az r negyzet egy
coef( SpecReg )
mean( RawData$m_ta )
SpecRegCoef <- coef( SpecReg )
names( SpecRegCoef )[ 1 ] <- "mu"
SpecRegCoef

# Proposition 6.2 (a)

mean( RawData$m_ta )
SpecRegCoef[ "mu" ]

dim( predCos )
cbind( as.numeric( (2/N)*RawData$m_ta %*% predCos ),
       SpecRegCoef[ grep( "cos", names( SpecRegCoef ) ) ] )

sum( ( (2/N)*RawData$m_ta %*% predSin - SpecRegCoef[ grep( "sin", names( SpecRegCoef ) ) ] )^2 )

# Proposition 6.2 (b)

(1/N)*( sum( ( RawData$m_ta - mean( RawData$m_ta ) )^2 ) )
var( RawData$m_ta )*( (N-1)/N )
(1/2)*sum( SpecRegCoef[ grep( "cos", names( SpecRegCoef ) ) ]^2 +
             SpecRegCoef[ grep( "sin", names( SpecRegCoef ) ) ]^2 )

# Proposition 6.2 (c)

cbind( (1/2)*( SpecRegCoef[ grep( "cos", names( SpecRegCoef ) ) ]^2 + SpecRegCoef[ grep( "sin", names( SpecRegCoef ) ) ]^2 ),
       (4*pi/N)*approx( BpHomSpec$freq, BpHomSpec$spec, (1:M)/N )$y )

(1/2)*( SpecRegCoef[ grep( "cos", names( SpecRegCoef ) ) ]^2 +
          SpecRegCoef[ grep( "sin", names( SpecRegCoef ) ) ]^2 ) /
  (4*pi/N)*approx( BpHomSpec$freq, BpHomSpec$spec, (1:M)/N )$y

SpecSajat <- N/(4*pi)*(1/2)*( SpecRegCoef[ grep( "cos", names( SpecRegCoef ) ) ]^2 +
                                SpecRegCoef[ grep( "sin", names( SpecRegCoef ) ) ]^2 )
xyplot( spec ~ freq, data = BpHomSpec, type = "l", scales = list( y = list ( log = 10 ) ) ) +
  latticeExtra::as.layer( xyplot( SpecSajat ~ ( (1:M)/N ), type = "l", col = "red", scales = list( y = list ( log = 10 ) ) ) )

## Hamilton példája (ipari termelés, gyártás) ##

EStoc <- eurostat::get_eurostat_toc()
EStoc[ EStoc$code=="sts_inpr_m", ]

# ManufProd <- eurostat::get_eurostat( "sts_inpr_m" )
ManufProd <- eurostat::get_eurostat( "sts_inpr_m", filters = list( geo = "HU", s_adj = "NSA", nace_r2 = "C", unit = "I10" ) )
# HU: magyar, NSA: nem szezonálisan kiigazított, C: manufacturing, I10: 2010-es bázison
# http://ec.europa.eu/eurostat/ramon/nomenclatures/index.cfm?TargetUrl=ACT_OTH_CLS_DLD&StrNom=CL_NACE2&StrFormat=HTML&StrLanguageCode=EN
# http://ec.europa.eu/eurostat/ramon/nomenclatures/index.cfm?TargetUrl=LST_NOM_DTL&StrNom=NACE_REV2&StrLanguageCode=EN&IntPcKey=&StrLayoutCode=HIERARCHIC
ManufProd <- ManufProd[ !is.na( ManufProd$values ), ]

str( ManufProd )

xyplot( values ~ time, data = ManufProd, type = "l" )
ManufProdSpec <- spectrum( ManufProd$values )
spectrum( ManufProd$values, span = 5 )

xyplot( spec ~ freq, data = ManufProdSpec, type = "l" )
xyplot( spec ~ freq, data = ManufProdSpec, type = "l", scales = list( y = list ( log = 10 ) ) )
xyplot( spec ~ (1/freq), data = ManufProdSpec, type = "l", xlim = c( 0, 20 ) )
xyplot( spec ~ (1/freq), data = ManufProdSpec, type = "l", xlim = c( 0, 50 ) )
xyplot( spec ~ (1/freq), data = ManufProdSpec, type = "l", scales = list( y = list ( log = 10 ) ), xlim = c( 0, 20 ) )
xyplot( spec ~ (1/freq), data = ManufProdSpec, type = "l", scales = list( y = list ( log = 10 ) ), xlim = c( 0, 50 ) )

ManufProd$ldvalues <- c( NA, diff( log( ManufProd$values ) ) )
xyplot( ldvalues ~ time, data = ManufProd, type = "l" )
LDManufProdSpec <- spectrum( ManufProd$ldvalues[ -1 ] )
xyplot( spec ~ (1/freq), data = LDManufProdSpec, type = "l", xlim = c( 0, 50 ) )
xyplot( spec ~ (1/freq), data = LDManufProdSpec, type = "l", xlim = c( 0, 20 ) )

ManufProd$ld2values <- c( rep( NA, 12 ), diff( log( ManufProd$values ), 12 ) )
xyplot( ld2values ~ time, data = ManufProd, type = "l" )
LD2ManufProdSpec <- spectrum( ManufProd$ld2values[ -(1:12) ] )
xyplot( spec ~ freq, data = LD2ManufProdSpec, type = "l" )
xyplot( spec ~ (1/freq), data = LD2ManufProdSpec, type = "l", xlim = c( 0, 20 ) )
xyplot( spec ~ (1/freq), data = LD2ManufProdSpec, type = "l", xlim = c( 0, 50 ) )

xyplot( spec ~ (1/freq), data = LDManufProdSpec, type = "l",
        scales = list( y = list ( log = 10 ) ), xlim = c( 0, 20 ),
        panel = function( x, y, ... ) {
          panel.xyplot( x, y, ... )
          maxs <- zoo::rollapply( y, 15, function(x) which.max(x)==(15+1)/2,
                             align = "center", fill = NA )
          panel.abline( v = x[ maxs ], col = "gray" )
          panel.points( x[ maxs ], y[ maxs ] )
          panel.text( x[ maxs ], y[ maxs ], round( x[ maxs ], 1 ), pos = 4 )
        } )

specpanel <- function( x, y, span, ... ) {
  if( span%%2==0 )
    span <- span+1
  panel.xyplot( x, y, ... )
  maxs <- zoo::rollapply( y, span, function(x) which.max(x)==(span+1)/2, align = "center", fill = NA )
  panel.abline( v = x[ maxs ], col = "gray" )
  panel.points( x[ maxs ], y[ maxs ] )
  panel.text( x[ maxs ], y[ maxs ], round( x[ maxs ], 1 ), pos = 4 )
}

xyplot( spec ~ (1/freq), data = LDManufProdSpec, type = "l",
        scales = list( y = list ( log = 10 ) ), xlim = c( 0, 20 ),
        panel = specpanel, span = 13 )

## EKG-k elemzése frekvenciatartományon ##

# require( tuneR )
# require( pastecs )
# devtools::install_github( "mkfs/r-physionet-ptb" )
library( r.physionet.ptb )

# https://www.physionet.org/physiobank/database/ptbdb/

system2( system.file( 'exec', 'ptb_patient_to_json.rb', package = 'r.physionet.ptb' ), args = "patient001" )

ptb <- ptb.from.file( "patient001.json" )
ptbecg <- ptb.extract.lead( ptb, "i" )
xyplot( ptbecg$`1-10010`[1:5000]~1:5000,type="l" )
ECGSpec <- spectrum( ptbecg$`1-10010` )
ECGSpec <- spectrum( ptbecg$`1-10010`, span = rep( 201, 3 ) )
xyplot( spec ~ freq*1000, data = ECGSpec, type = "l", scales = list( y = list ( log = 10 ) ) )
xyplot( spec ~ freq*1000, data = ECGSpec, type = "l", scales = list( y = list ( log = 10 ) ), xlim = c( 0, 70 ),
        panel = specpanel, span = 7 )