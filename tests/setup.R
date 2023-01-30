# family structure
# questions cuz radar fails at
# storks with bassinets
library(SAScii)
library(readr)

dat_url <-
	"https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2017_2019_FemRespData.dat"

sas_url <-
	file.path(
		dirname( dat_url ) , 
		"sas" , 
		gsub( "Data" , "Setup" , basename( dat_url ) )
	)
	
sas_positions <-
	parse.SAScii( sas_url )

sas_positions[ , 'varname' ] <-
	tolower( sas_positions[ , 'varname' ] )

sas_positions[ , 'column_types' ] <-
		ifelse( sas_positions[ , 'char' ] , "c" , "d" )

nsfg_tbl <-
	read_fwf(
		dat_url ,
		fwf_widths( 
			abs( sas_positions[ , 'width' ] ) , 
			col_names = sas_positions[ , 'varname' ] 
		) ,
		col_types = paste0( sas_positions[ , 'column_types' ] , collapse = "" ) ,
		na = c( "" , "." )
	)
	
nsfg_df <- data.frame( nsfg_tbl )

options( survey.lonely.psu = "adjust" )

library(survey)

nsfg_design <- 
	svydesign( 
		id = ~ secu , 
		strata = ~ sest , 
		data = nsfg_df , 
		weights = ~ wgt2017_2019 , 
		nest = TRUE 
	)
nsfg_design <- 
	update( 
		nsfg_design , 

		one = 1 ,
		
		birth_control_pill = as.numeric( constat1 == 6 ) ,
		
		age_categories = 
			factor( findInterval( ager , c( 15 , 20 , 25 , 30 , 35 , 40 ) ) ,
				labels = c( '15-19' , '20-24' , '25-29' , '30-34' , '35-39' , '40-49' ) ) ,
		
		marstat =
			factor( marstat , levels = c( 1:6 , 8:9 ) ,
				labels = c(
					"Married to a person of the opposite sex" ,
					"Not married but living together with a partner of the opposite sex" ,
					"Widowed" ,
					"Divorced or annulled" ,
					"Separated, because you and your spouse are not getting along" ,
					"Never been married" ,
					"Refused" ,
					"Don't know" )
			)
	)
sum( weights( nsfg_design , "sampling" ) != 0 )

svyby( ~ one , ~ age_categories , nsfg_design , unwtd.count )
svytotal( ~ one , nsfg_design )

svyby( ~ one , ~ age_categories , nsfg_design , svytotal )
svymean( ~ pregnum , nsfg_design , na.rm = TRUE )

svyby( ~ pregnum , ~ age_categories , nsfg_design , svymean , na.rm = TRUE )
svymean( ~ marstat , nsfg_design )

svyby( ~ marstat , ~ age_categories , nsfg_design , svymean )
svytotal( ~ pregnum , nsfg_design , na.rm = TRUE )

svyby( ~ pregnum , ~ age_categories , nsfg_design , svytotal , na.rm = TRUE )
svytotal( ~ marstat , nsfg_design )

svyby( ~ marstat , ~ age_categories , nsfg_design , svytotal )
svyquantile( ~ pregnum , nsfg_design , 0.5 , na.rm = TRUE )

svyby( 
	~ pregnum , 
	~ age_categories , 
	nsfg_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE , na.rm = TRUE
)
svyratio( 
	numerator = ~ pregnum , 
	denominator = ~ lbpregs , 
	nsfg_design ,
	na.rm = TRUE
)
sub_nsfg_design <- subset( nsfg_design , timescoh > 0 )
svymean( ~ pregnum , sub_nsfg_design , na.rm = TRUE )
this_result <- svymean( ~ pregnum , nsfg_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ pregnum , 
		~ age_categories , 
		nsfg_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( nsfg_design )
svyvar( ~ pregnum , nsfg_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ pregnum , nsfg_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ pregnum , nsfg_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ birth_control_pill , nsfg_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( pregnum ~ birth_control_pill , nsfg_design )
svychisq( 
	~ birth_control_pill + marstat , 
	nsfg_design 
)
glm_result <- 
	svyglm( 
		pregnum ~ birth_control_pill + marstat , 
		nsfg_design 
	)

summary( glm_result )
library(srvyr)
nsfg_srvyr_design <- as_survey( nsfg_design )
nsfg_srvyr_design %>%
	summarize( mean = survey_mean( pregnum , na.rm = TRUE ) )

nsfg_srvyr_design %>%
	group_by( age_categories ) %>%
	summarize( mean = survey_mean( pregnum , na.rm = TRUE ) )
result <- svytotal( ~ one , nsfg_design )

stopifnot( round( coef( result ) , 0 ) == 72671926 )

stopifnot( round( SE( result ) , 0 ) == 3521465 )
row_percents <- c( 19.5112 , 23.7833 , 19.6916 , 15.2800 , 6.4965 , 6.5215 )

std_err_row_percents <- c( 1.8670 , 2.1713 , 2.2773 , 1.7551 , 0.9895 , 1.0029 )

results <- svyby( ~ birth_control_pill , ~ age_categories , nsfg_design , svymean )

stopifnot( all( round( coef( results ) * 100 , 4 ) == row_percents ) )

stopifnot( all( round( SE( results ) * 100 , 4 ) == std_err_row_percents ) )
