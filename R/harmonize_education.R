#' Harmonize a set of education variables all at once
#' @param data.frame
#' @value data.frame
#' @export

harmonize_education <- function(CensusData,
                                year,
                                delete_originals = T){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }

        if(!is.numeric(year)){
                stop("'year' must be a numeric value")
        }

        # Building age for being used in the further harmonizations
        check_vars <- check_var_existence(CensusData, c("age"))
        age_just_created <- FALSE
        if(length(check_vars) > 0) {
                call <- paste0("build_demographics_age_",year,"(CensusData)")
                CensusData <- eval(parse(text = call))
                age_just_created <- TRUE
        }
        gc()


        call <- paste0("build_education_literacy_",year,"(CensusData)")
        CensusData <- eval(parse(text = call))
        gc(); Sys.sleep(.5); gc()


        call <- paste0("build_education_levelattnd_",year,"(CensusData)")
        CensusData <- eval(parse(text = call))
        gc(); Sys.sleep(.5); gc()


        call <- paste0("build_education_schoolattnd_",year,"(CensusData)")
        CensusData <- eval(parse(text = call))
        gc(); Sys.sleep(.5); gc()


        call <- paste0("build_education_attainment_",year,"(CensusData)")
        CensusData <- eval(parse(text = call))
        gc(); Sys.sleep(.5); gc()


        if(delete_originals == T){

                if(year == 1960){
                        var_to_exclude <- c("v204","v211","v212","v213","v214")
                }

                if(year == 1970){
                        var_to_exclude <- c("v035","v036","v037","v038","v039")
                }

                if(year == 1980){
                        var_to_exclude <- c("v519", "v520", "v521", "v522", "v523", "v524", "v525")
                }

                if(year == 1991){
                        var_to_exclude <- c("v0323", "v0324","v0325","v0326","v0327","v0328","v0329")
                }

                if(year == 2000){
                        var_to_exclude <- c("v0428", "v0429", "v0430", "v0431", "v0432", "v0433", "v0434", "v4752")
                }

                if(year == 2010){
                        var_to_exclude <- c("v0627", "v0628", "v0629", "v0630", "v0631", "v0632", "v0633", "v0634", "v0636")
                }

                CensusData[, (var_to_exclude) := NULL]

                gc();Sys.sleep(.5);gc()

        }

        # Deleting age auxiliary variable (if it were created on-the-fly just for the purpose
        # of harmonizing the education variables.
        if(age_just_created == TRUE){
                CensusData[ , age := NULL]
        }


        gc()

        # education
        # 1 "None"
        # 2 "Primary, incomplete"
        # 3 "Primary, complete"
        # 4 "Middle school, incomplete"
        # 5 "Middle school, complete"
        # 6 "High School, incomplete"
        # 7 "High School, complete"
        # 8 "Higher Education, incomplete"
        # 9 "Higher Education, complete"
        # 999 "Unknown", replace

        #levelattnd
        # 1 "Regular Primary/Middle school"
        # 2 "Regular High chool"
        # 3 "Higher education"
        # 9 "Other", replace

        CensusData

}
