#' Get Reweighted Forecast Data
#' This is a cover function where one inputs the range of forecasts months.
#' The function then iterates over the specified range calling get_quantiles_reweighted,
#' and stores the results of these iterated outputs in a data.table.
#' Output can then be merged with ERA-data using a get_ERA_NWP function to be used in prediction tasks.
#'
#' @param start_month
#' @param start_year
#' @param forc_months
#'
#' @return Returns reweighted quantiles of interest
#' @export
#'
#' @examples

get_forc_data_re = function(start_month, start_year, forc_months){
    #Forc_months gives total forecast months (including start month) .

    add_months = forc_months - 1

    #Get dates for data.table
    first_start_date = as.Date(paste0(start_year, '-', start_month, '-', '01'))
    add = paste(add_months, 'month')
    last_start_date = seq(first_start_date, length=2, by=add)[2]
    init_dates = as.character(seq(first_start_date, length=forc_months, by='1 month'))

    target_hour = rep(c(6,12,18,24), 125)

    #Set data.table to store results
    NWP_res = list()

    #Get indexes for files
    year_bool = as.numeric(mapply(substring,files, 51, 54)) == start_year
    month_bool = as.numeric(mapply(substring,files, 56, 57)) == as.numeric(start_month)
    start_files_ix = which((year_bool) & (month_bool))[[1]]
    stop_files_ix = start_files_ix + forc_months -1

    #Loop over monthly forecast files
    col_nr = 0
    l = 0
    for (file_ix in c(start_files_ix:stop_files_ix)){
        out = get_quantiles_reweighted(file_ix)   #Using get_quantiles function
        NWP_reweight_q_90_PC1 = out$Results

        ix = file_ix - start_files_ix + 1
        print(ix)
        date_seq = seq(as.Date(init_dates[ix]), length=125, by='1 days')
        target_date = as.character(rep(date_seq, each=4))
        init_date = rep(init_dates[ix], 500)

        # Store results
        l = l+1
        NWP_res[[l]] = data.table(date =as.Date(target_date),
                                  init_date,
                                  hour = target_hour,
                                  NWP_reweight_q_90_PC1   )

    }
    #Store and return
    NWP = rbindlist(NWP_res)
    out = c()
    out$NWP = NWP
    print('Get quantiles completed')
    return(out)
}


get_quantiles_reweighted = function(i){

    #Open nc4, assign file and close nc4
    nc = nc_open(files[i])
    forec = ncvar_get(nc, attributes(nc$var)$names[1])
    year_nwp = substring(files[i], 51, 54) #year
    month_nwp = substring(files[i], 56, 57) #month
    forc_name = paste0('forc', year_nwp, '_', month_nwp)
    assign(forc_name, forec)
    print(c(year_nwp,month_nwp))
    nc_close(nc)

    #Get forecast and run pca- delete dt when done and store pca-results
    forecast = get(forc_name)
    print(dim(forecast))
    pc_data = get_pca_exp(X_mat, forecast[,,,1:dim(forecast)[4]], I_train, I_test, 2,
                          U = PC_ERA_79_92$U,
                          mu = PC_ERA_79_92$mu)
    rm(forecast)
    rm(forc_name)

    #1 get weights
    #1a find the correct date for ERA
    d= as.Date(paste0(year_nwp,'-',month_nwp,'-15'))
    ERA_PC1_15 = PC_ERA_79_92$dt_test[(date == d & hour ==12), PC1]

    #1b find the matching NWP
    results = list()
    #Find weight
    sq = exp(seq(log(0.000001), log(0.01), length.out = 25))
    for (k in 1:length(sq)){
        w = rep(0,dim(pc_data$NWP_PC_mat)[3])
        for (m in c(57:60)){  # time 57:60 is day 15 hours 6,12,18,24
            NWP_15 = pc_data$NWP_PC_mat[m,1,]
            w = w + -0.5*sq[k]*(ERA_PC1_15 - NWP_15)^2
        }

        mx = max(w)
        s = sum( exp(w - mx) )
        w_n = exp(w - mx) / s

        #Apply weight to remaining obs
        #When applying weights we are not dealing with actual values,
        #so we must use a quant est function to get an actual values back
        rw = rep(0,500)
        for (j in 1:500){
            rw[j] = whdquantile(pc_data$NWP_PC_mat[j,1,], p = 0.9, weights = w_n)
        }

        #Store
        out$NWP_reweight_q_90_PC1 = rw
        results[[k]] = data.table(t(rw))
    }

    Results = t(rbindlist(results,use.names=FALSE))
    out = c()
    out$Results = Results


    return(out)
}

wquantile.generic <- function(x, probs, cdf.gen, weights = NA) {
    n <- length(x)
    if (any(is.na(weights)))
        weights <- rep(1 / n, n)
    nw <- sum(weights)^2 / sum(weights^2) # Kish's effective sample size

    indexes <- order(x)
    x <- x[indexes]
    weights <- weights[indexes]

    weights <- weights / sum(weights)
    cdf.probs <- cumsum(c(0, weights))

    sapply(probs, function(p) {
        cdf <- cdf.gen(nw, p)
        q <- cdf(cdf.probs)
        w <- tail(q, -1) - head(q, -1)
        sum(w * x)
    })
}

# whdquantile = function(x, probs, weights = NA) {
#         cdf.gen <- function(n, p){
#                 func = function(cdf.probs) {
#                         pbeta(cdf.probs, (n + 1) * p, (n + 1) * (1 - p))
#                 }
#                 return(func)
#         }
#         wquantile.generic(x, probs, cdf.gen, weights)
# }

whdquantile = function(x, probs, weights = NA) {
    cdf.gen <- function(n, p){
        func = function(cdf.probs) {
            pbeta(cdf.probs, (n + 1) * p, (n + 1) * (1 - p))
        }
        return(func)
    }
    wquantile.generic(x, probs, cdf.gen, weights)
}
