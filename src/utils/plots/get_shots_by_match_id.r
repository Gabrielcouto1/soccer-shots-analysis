get_shots_by_match_id <- function(match_id, dataset){
    shots <- dataset[which(dataset$match_id==match_id),]

    return(shots)
}