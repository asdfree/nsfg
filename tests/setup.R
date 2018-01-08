if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)
lodown( "nsfg" , output_dir = file.path( getwd() ) )
library(lodown)
# examine all available NSFG microdata files
nsfg_cat <-
	get_catalog( "nsfg" ,
		output_dir = file.path( getwd() ) )

# 2013-2015 only
nsfg_cat <- subset( nsfg_cat , grepl( "2013_2015" , full_url ) )
# download the microdata to your local computer


options( survey.lonely.psu = "adjust" )

library(survey)

nsfg_df <- readRDS( file.path( getwd() , "2013_2015_FemRespData.rds" ) )

nsfg_design <- 
	svydesign( 
		id = ~ secu , 
		strata = ~ sest , 
		data = nsfg_df , 
		weights = ~ wgt2013_2015 , 
		nest = TRUE 
	)
nsfg_design <- 
	update( 
		nsfg_design , 

		one = 1 ,
		
		birth_control_pill = as.numeric( constat1 == 6 ) ,
		
		age_categories = 
			factor( findInterval( ager , c( 15 , 20 , 25 , 30 , 35 , 40 ) ) ,
				labels = c( '15-19' , '20-24' , '25-29' , '30-34' , '35-39' , '40-44' ) ) ,
		
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
svymean( ~ npregs_s , nsfg_design , na.rm = TRUE )

svyby( ~ npregs_s , ~ age_categories , nsfg_design , svymean , na.rm = TRUE )
svymean( ~ marstat , nsfg_design )

svyby( ~ marstat , ~ age_categories , nsfg_design , svymean )
svytotal( ~ npregs_s , nsfg_design , na.rm = TRUE )

svyby( ~ npregs_s , ~ age_categories , nsfg_design , svytotal , na.rm = TRUE )
svytotal( ~ marstat , nsfg_design )

svyby( ~ marstat , ~ age_categories , nsfg_design , svytotal )
svyquantile( ~ npregs_s , nsfg_design , 0.5 , na.rm = TRUE )

svyby( 
	~ npregs_s , 
	~ age_categories , 
	nsfg_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE ,
	na.rm = TRUE
)
svyratio( 
	numerator = ~ npregs_s , 
	denominator = ~ nbabes_s , 
	nsfg_design ,
	na.rm = TRUE
)
sub_nsfg_design <- subset( nsfg_design , timescoh > 0 )
svymean( ~ npregs_s , sub_nsfg_design , na.rm = TRUE )
this_result <- svymean( ~ npregs_s , nsfg_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ npregs_s , 
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
svyvar( ~ npregs_s , nsfg_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ npregs_s , nsfg_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ npregs_s , nsfg_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ birth_control_pill , nsfg_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( npregs_s ~ birth_control_pill , nsfg_design )
svychisq( 
	~ birth_control_pill + marstat , 
	nsfg_design 
)
glm_result <- 
	svyglm( 
		npregs_s ~ birth_control_pill + marstat , 
		nsfg_design 
	)

summary( glm_result )
library(srvyr)
nsfg_srvyr_design <- as_survey( nsfg_design )
nsfg_srvyr_design %>%
	summarize( mean = survey_mean( npregs_s , na.rm = TRUE ) )

nsfg_srvyr_design %>%
	group_by( age_categories ) %>%
	summarize( mean = survey_mean( npregs_s , na.rm = TRUE ) )

