#' get_ERA_NWP
#' @name get_ERA_NWP
#'
#' @param ERA_path Path to ERA file
#' @param NWP_path Path to NWP file
#' @param NWP_quant NWP quantile of interest
#' @param PC Principle component
#' @param NWP_preds Number of NWP predictors
#'
#' @return Data.table with data ready for input in rolling_mod function
#' @export
#' @import data.table
#'
#' @examples
#' ERA_NWP = get_ERA_NWP(NWP_preds = 2)
#'
#'



#
get_ERA_NWP = function(ERA_path = '~/Desktop/MasterNR/PC_ERA_79_92.Rda',
                    NWP_path = '~/Desktop/MasterNR/NWP_quant_1993_2022.Rda',
                    NWP_quant = 'NWP_q_90_PC1',
                    PC = 'PC1',
                    NWP_preds = 1){
    # Set vars to null for package reasons
    init_date = init_date_1 = NWP_1 = monthdiff = . = NULL

    #Load specified data
    load_ERA_name = load(file = ERA_path)
    ERA = get(load_ERA_name)
    F_ERA = ERA$dt_test

    load_NWP_name = load(file = NWP_path)
    NWP = get(load_NWP_name)

    # Merge NWP, ERA
    ERA_NWP_merge = merge(F_ERA, NWP)

    #Set name of quantile and PC of interest
    setnames(ERA_NWP_merge, NWP_quant, 'NWP')
    setnames(ERA_NWP_merge, PC, 'PC')

    #Select variables including quantile of interest
    ERA_NWP_vars = ERA_NWP_merge[,.(date, hour, init_date, NWP, PC)]

    #Prepare to cast on init_date for
    inits = c(1,2,3,4,5)
    ERA_NWP_vars[,'monthdiff' :=  inits[(month(date)+ year(date)*12) -(month(init_date)+ year(init_date)*12)+1]]
    ERA_NWP_vars[,`:=` (monthdiff2 = monthdiff-1,
                        monthdiff3 = monthdiff-2,
                        monthdiff4 = monthdiff-3,
                        monthdiff5 = monthdiff-4)]
    #Cast
    Cast1 = dcast(ERA_NWP_vars, date +hour +PC ~ monthdiff , value.var = c('NWP', 'init_date'))
    Cast2 = dcast(ERA_NWP_vars, date +hour +PC ~ monthdiff2, value.var = c('NWP', 'init_date'))
    Cast3 = dcast(ERA_NWP_vars, date +hour +PC ~ monthdiff3, value.var = c('NWP', 'init_date'))
    Cast4 = dcast(ERA_NWP_vars, date +hour +PC ~ monthdiff4, value.var = c('NWP', 'init_date'))
    Cast5 = dcast(ERA_NWP_vars, date +hour +PC ~ monthdiff5, value.var = c('NWP', 'init_date'))


    #Merge tables
    vars = c('date', 'hour', 'PC', 'init_date_1', 'NWP_1', 'NWP_2', 'NWP_3', 'NWP_4', 'NWP_5')
    ERA_NWP_comb =rbindlist(list(
                         Cast1[,.SD, .SDcols = vars],
                         Cast2[,.SD, .SDcols = vars[1:8] ],
                         Cast3[,.SD, .SDcols = vars[1:7] ],
                         Cast4[,.SD, .SDcols = vars[1:6] ],
                         Cast5[,.SD, .SDcols = vars[1:5] ]), fill = TRUE)

    ERA_NEW_keep = ERA_NWP_comb[NWP_1!='NA']

    #Create time variables from date
    ERA_NEW_keep[,'year'  := (year(date) - 1993)]
    ERA_NEW_keep[,'month' := month(date)]
    ERA_NEW_keep[,'week'  := week(date)]
    ERA_NEW_keep[,'lead_time':= ((as.integer(difftime(date, init_date_1, units = 'days')))*4)+ hour/6]

    NWPs = c('NWP1', 'NWP2', 'NWP3', 'NWP4', 'NWP5')
    #Set new names and select
    setnames(ERA_NEW_keep, c('NWP_1', 'NWP_2', 'NWP_3', 'NWP_4', 'NWP_5','init_date_1', 'PC'),
             c(NWPs,'init_date',PC ))

    last_vars = c(c('date','hour','week', 'month','year','init_date', 'lead_time'),
                    NWPs[1:NWP_preds], PC)

    ERA_NWP_select = ERA_NEW_keep[,.SD, .SDcols = last_vars]

    return(ERA_NWP_select)
}








