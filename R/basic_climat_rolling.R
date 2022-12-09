#' @title rolling_mod_climatology
#'
#' @param q Quantile of interest.
#' @param ERA_NWP Data of ERA observations and quantiles of NWP forecasts.
#' @param window Max number of forecast days from initialization.
#'
#' @return Model output with predictions, loss and model coefficients.
#' @export
#' @import quantreg
#' @import data.table
#' @import stats
#'
#' @examples
#'
#' @name rolling_mod_climatology

#mod_climate = rolling_mod_climatology(q = 0.9, ERA_NWP,window = 60)

rolling_mod_climatology = function(q, ERA_NWP,window = 60){
    detailed_results = list()

    init_date = NWP1 = NWP2 = . = NULL

    #Fix dates
    start =as.Date('2007-01-01')
    end = as.Date('2022-05-01')
    all_days = seq(start, end,  by = '1 days')
    init_days = all_days[mday(all_days)==1]
    init_m = start
    k = 0
    incl_vars = c('date', 'PC1', 'hour', 'init_date', 'lead_time')
    ERA_NWP_vars = ERA_NWP[,.SD, .SDcols =incl_vars]
    ERA_NWP_vars[,yday:=yday(date)]

    #Forecast iteration
    for (i in seq_along(init_days)){
        init_day = init_days[i]
        target_days = seq(init_day, length.out = window,  by = '1 days')

        print(paste('Forecast made on:', init_day))

        ERA_NWP_time = ERA_NWP_vars[date <= target_days[length(target_days)],]
        ERA_NWP_final= na.omit(ERA_NWP_time)

        ## 5b) Split train-test
        train = ERA_NWP_final[ yday %in% yday(target_days) & date < init_day, .SD, keyby = .(date,hour)]
        test = ERA_NWP_final[init_date == init_day, .SD, keyby = .(date, hour)]

        ## 5c) Run qr-reg
        #train_pred = train[,pred :=quantile(PC1,probs = q), keyby = .(yday,hour)]
        test_pred = train[,quantile(PC1,probs = q), keyby = .(yday,hour)]

        test_df = merge(test_pred, test, by.x = c('yday', 'hour'), by.y =c('yday', 'hour'))
        #train_l = pinball_loss(q, train_pred[,pred], train$PC1)
        test_l = pinball_loss(q, test_df[,V1], test_df$PC1)

        ## 5d) Register loss
        results = test
        results[,'pred' :=test_df[,V1]]
        results[,'test_loss' := test_l]

        k = k+1
        detailed_results[[k]] = results
    }

    Results = rbindlist(detailed_results,use.names=FALSE)
    out = c()
    out$Results = Results
    return(out)
}
#ERA_NWP =get_ERA_NWP()
