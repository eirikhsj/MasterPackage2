#' @title Rolling model reweighting
#'
#' @param q Quantile of interest.
#' @param ERA_NWP Data of ERA observations and quantiles of NWP forecasts.
#' @param predictors Number of NWP predictors.
#' @param model Specify type of model, e.g. qreg, qgam, etc.
#' @param window Max number of forecast days from initialization.
#' @param hour_v Include hour covariate.
#' @param week_v Include week covariate.
#' @param month_v Include month covariate.
#' @param year_v Include year covariate.
#'
#' @return Model output with predictions, loss and model coefficients.
#' @export
#' @import quantreg
#' @import data.table
#' @import stats
#'
#' @examples
#' @name rolling_mod_re


# q = 0.9
# predictors = 1
# model = 'reg'
# window = 60
# hour_v=TRUE
# week_v = TRUE
# month_v = TRUE
# year_v = TRUE


rolling_mod_re = function(q, ERA_NWP_re = load('input_data/ERA_NWP.Rda'), predictors, model='reg', window = 45,
                          hour_v=FALSE, week_v=FALSE, month_v = FALSE, year_v=FALSE){
    detailed_results = list()

    init_date = NWP1 = NWP2 = . = NULL

    #1) Fix dates
    start =as.Date('2007-01-01')
    end = as.Date('2022-05-01')
    all_days = seq(start, end,  by = '1 days')
    init_days = all_days[mday(all_days)==16]
    init_m = start
    k = 0

    #2) Build regression formula and fetch variables
    incl_vars = c('date','PC1', 'hour', 'init_date', 'lead_time')
    formula = 'PC1 ~ 1'

    if (hour_v == TRUE){
        formula = paste0(formula, ' + as.factor(hour)')}
    if (week_v == TRUE){
        incl_vars = c(incl_vars, 'week')
        formula = paste0(formula, ' + cos(2*pi * week/52.5) + sin(2*pi * week/52.5)')} #works a lot better
    if (month_v == TRUE){
        incl_vars = c(incl_vars, 'month')
        formula = paste0(formula, ' + cos(2*pi * month/12) + sin(2*pi * month/12)')}
    if (year_v == TRUE){
        incl_vars = c(incl_vars, 'year')
        formula = paste0(formula, ' + year')}
    if (predictors > 0){
        for (l in 1:predictors){
            incl_vars = c(incl_vars, c('NWP1_re')[l])
            formula = paste0(formula, ' + ', c('NWP1_re')[l])}
    }

    ERA_NWP_vars = ERA_NWP_re[,.SD, .SDcols =incl_vars]

    #3) Forecast iteration
    for (i in seq_along(init_days)){

        ##3a) Time keeping
        init_day = init_days[i]
        target_days = seq(init_day, length.out = window,  by = '1 days')
        print(paste('Forecast made on:', init_day))
        ERA_NWP_time = ERA_NWP_vars[date <= target_days[length(target_days)],]
        ERA_NWP_final= na.omit(ERA_NWP_time)

        ## 3b) Split train-test
        train = ERA_NWP_final[date<init_day, .SD, keyby = .(date,hour)]
        test = ERA_NWP_final[date %in%target_days, .SD, keyby = .(date,hour)]

        ## 3c) Run qr-reg
        if (model == 'reg'){
            suppressWarnings(qreg <- rq(formula, data = train, tau = c(q)))
            train_l = pinball_loss(q, predict(qreg), train$PC1)
            test_l = pinball_loss(q, predict(qreg, newdata = test), test$PC1)
        }
        ## 3d) Register loss
        results = test
        results[,'pred' := predict(qreg, newdata = test)]
        results[,'test_loss' := test_l]

        ## 3e) Register Beta coefficients
        betas = data.table(t(coef(qreg)))
        if(year_v==TRUE){
            betas[,'year_coef' := year]
            betas[,'year':= NULL]
        }
        if(predictors > 0){
            betas[,'NWP1_coef' := NWP1_re]
            betas[,'NWP1_re' :=NULL]
            if (predictors ==2){
                betas[,'NWP2_coef' := NWP1_re]
                betas[,'NWP1_re' :=NULL]}
        }
        betas = betas[rep(1,dim(test)[1]),] #unnecessary storage use, expand later
        results = cbind(results, betas)

        k = k+1
        detailed_results[[k]] = results
    }

    #4) Store and return
    Results = rbindlist(detailed_results,use.names=FALSE)
    out = c()
    out$Results = Results
    return(out)
}
#ERA_NWP =get_ERA_NWP()
