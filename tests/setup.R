# family structure
# questions cuz radar fails at
# storks with bassinets
library(haven)

sas_url <-
	"https://ftp.cdc.gov/pub/Health_Statistics/NCHS/NSFG/NSFG-2022-2023-FemRespPUFData.sas7bdat"
	
nsfg_tbl <- read_sas( sas_url )
	
nsfg_df <- data.frame( nsfg_tbl )

names( nsfg_df ) <- tolower( names( nsfg_df ) )
# nsfg_fn <- file.path( path.expand( "~" ) , "NSFG" , "this_file.rds" )
# saveRDS( nsfg_df , file = nsfg_fn , compress = FALSE )
# nsfg_df <- readRDS( nsfg_fn )
library(survey)

options( survey.lonely.psu = "adjust" )

nsfg_design <- 
	svydesign( 
		id = ~ vecl , 
		strata = ~ vest , 
		data = nsfg_df , 
		weights = ~ wgt2022_2023 , 
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
sub_nsfg_design <- subset( nsfg_design , evrcohab == 1 )
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
result <- svytotal( ~ one , nsfg_design )

stopifnot( round( coef( result ) , 0 ) == 74936918 )

stopifnot( round( SE( result ) , 0 ) == 2910451 )
row_percents <- c( 14.2348 , 18.9586 , 14.6057 , 10.1973 , 7.8114 , 6.8632 )

std_err_row_percents <- c( 1.6792 , 2.0226 , 1.8889 , 1.3836 , 1.1050 , 0.7961 )

results <- svyby( ~ birth_control_pill , ~ age_categories , nsfg_design , svymean )

stopifnot( all( round( coef( results ) * 100 , 4 ) == row_percents ) )

stopifnot( all( round( SE( results ) * 100 , 4 ) == std_err_row_percents ) )
library(srvyr)
nsfg_srvyr_design <- as_survey( nsfg_design )
nsfg_srvyr_design %>%
	summarize( mean = survey_mean( pregnum , na.rm = TRUE ) )

nsfg_srvyr_design %>%
	group_by( age_categories ) %>%
	summarize( mean = survey_mean( pregnum , na.rm = TRUE ) )
