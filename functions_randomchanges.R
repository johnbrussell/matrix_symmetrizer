# Here use the ratio of percentage decrease 
# in entropy to percentage of the way to the minimum known change can be 
# calculated for each step... so maybe you should order the ones you look at, 
# choosing first the ones that have the highest ratio... 
adjust_df_ratio <- function(df, term, percent, pbt, repeats)
{
    # Generate all the null elements if they are not meant to be null
    dfbest = diagonaldemand(df)
    dforig = df
    n = nrow(df)
    
    best_change = calc_change(dfbest, dforig)
    
    # Determine the search order
    timesince = 0
    timeterm = 1/pbt
    
    # Timesince is the number of iterations of the loop that have happened since improving the matrix
    while(timesince < timeterm)
    {
        df = dforig
        cent = calc_entropy(df)
        #bad = 0
        
        while(calc_entropy(df) >= term)
        {
            # Randomly make a change
            i = round(runif(1,0,1)*n + .5)
            j = round(runif(1,0,1)*n + .5)
            
            dft = make_change(df, i, j, percent)
            
            # Bad seems to be a variable that increases when you make bad changes and decreases 
            #  when you make good ones.  
            if(calc_entropy(dft) >= cent)
            {
                #bad = bad + 1
                next
            }
            
            #bad = max(0, bad - 1/2)
            
            
            df = dft
            if(repeats > 1)
            {
                min = ceiling(repeats - repeats_plusorminus)
                max = floor(repeats + repeats_plusorminus)
                lrepeats = sample(min:max, 1) - 1
                if(lrepeats > 0)
                {
                    for(k in 1:lrepeats)
                    {
                        df = make_change(df, i, j, percent)
                    }
                }
            }
            
            cent = calc_entropy(df)
        }
        if(calc_change(df, dforig) < best_change)
        {
            dfbest = df
            timesince = 0
            print("new best")
        }
        else 
            timesince = timesince + 1
        
        best_change = calc_change(dfbest, dforig)
    }
    
    return(dfbest)
}

get_dm <- function(size)
{
    if(!is.numeric(size))
        return(get_dm_route(size))
    if(size == 4)
    {
        r1 = c(0, 0.1, 0.2, .3)
        r2 = c(0.2, 0, 0.4, .2)
        r3 = c(0.3, 0.25, 0, .1)
        r4 = c(.2, .1, .5, 0)
        return(stochastize(rbind(r1, rbind(r2, rbind(r3, r4)))))
    }
    
    if(size == 3)
    {
        r1 = c(0, 0.1, 0.2)
        r2 = c(0.2, 0, 0.4)
        r3 = c(0.3, 0.25, 0)
        return(stochastize(rbind(r1, rbind(r2,r3))))
    }
    
    if(size == 5)
    {
        r1 = c(0, .2, .3, .2, .25)
        r2 = c(0.1, 0, .2, .5, .3)
        r3 = c(0.2, 0.25, 0, 0.05, 0.4)
        r4 = c(0.3, 0.35, 0.1, 0, 0.2)
        r5 = c(.5, 0.4, 0.3, 0.1, 0)
        return(stochastize(rbind(r1, rbind(r2, rbind(r3, rbind(r4, r5))))))
    }
}

get_entries <- function(size)
{
    if(!is.numeric(size))
        return(get_entries_route(size))
    if(size == 4)
        return(c(2, 4, 3, 1))
    if(size == 3)
        return(c(2, 4, 3))
    if(size == 5)
        return(c(100, 200, 150, 125, 125))
}

stochastize <- function(matrix)
{
    tempsum = 0
    for (i in 1:nrow(matrix))
    {
        tempsum = sum(matrix[i,])
        matrix[i,] = matrix[i,] / tempsum
    }
    return(matrix)
}

make_df <- function(dm, entries)
{
    for(i in 1:nrow(dm))
    {
        dm[i,] = dm[i,] * entries[i]
    }
    return(dm)
}

# Takes a demand flow matrix and returns a matrix where every passenger 
#  exits the system at the same station where they got on
diagonaldemand <- function(df)
{
    for(i in 1:nrow(df))
    {
        df[i,i] = sum(df[i,])
        for(j in 1:ncol(df))
        {
            if(j != i)
                df[i,j] = 0
        }
    }
    return(df)
}

# This takes a demand flow matrix (df) and a comparison demand flow matrix (orig)
#  and calculates the sum of squared change required to go from the comparison 
#  to the new matrix
calc_change <- function(df, orig)
{
    value = df - orig
    value = value * value
    return(sum(value))
}

# This is a similar function that calculates the difference in demand flows
#  from A to B and from B to A.
calc_entropy <- function(df)
{
    dft = t(df)
    value = df - dft
    value = value * value
    return(sum(value)/2)
}

# Make change -- given an i, j, and demand flow matrix return a modified demand 
#  flow matrix with passengers moved away from the stated i and j
make_change <- function(df, i, j, per)
{
    if(df[i,j] <= df[j,i])
        return(df)
    
    dforigl = df
    
    return(normalize(change_percentage(df, i, j, per), dforigl, i, j))
}

change_percentage <- function(df, i, j, per)
{
    change = (df[i,j] - df[j,i]) * per / 200
    df[i,j] = df[i,j] - change
    df[j,i] = df[j,i] + change
    return(df)
}

change_absolute <- function(df, i, j, abs)
{
    if(abs > (df[i,j] - df[j, i])/2)
        return(change_percentage(df, i, j, 100))
    df[i,j] = df[i,j] - abs
    df[j,i] = df[j,i] + abs
    return(df)
}

change_abs_per <- function(df, i, j, absper)
{
    dforig = df
    change = df[i,j] - df[j,i] * absper / 100
    df[i,j] = df[i,j] - change
    df[j,i] = df[j,i] + change
    df = adjust_row_except(df, i, j, dforig)
    return(adjust_row_except(df, j, i, dforig))
}

adjust_row_except <- function(df, row, ignorecol, orig)
{
    sum = sum(orig[row,]) - df[row, ignorecol]
    dfsum = sum(df[row,]) - df[row, ignorecol]
    change = sum / dfsum
    for(i in 1:ncol(df))
    {
        if(i == ignorecol)
            next
        df[row,i] = df[row,i] * change
    }
    return(df)
}

normalize <- function(df, dforig, i, j)
{
    df[i,] = df[i,] * sum(dforig[i,]) / sum(df[i,])
    df[j,] = df[j,] * sum(dforig[j,]) / sum(df[j,])
    return(df)
}

get_dm_route <- function(route)
{
    if(route == "Ethan Allen")
        return(stochastize(read.csv("C://users/John/Google Drive/Mind Games/transportation/MBTA/MassTrack/EA Demand.csv", stringsAsFactors = FALSE)))
    if(route == "Vermonter")
        return(stochastize(read.csv("C://users/John/Google Drive/Mind Games/transportation/MBTA/MassTrack/VT demand.csv", stringsAsFactors = FALSE)))
    return(stochastize(read.csv(str_c("C://users/John/Google Drive/Mind Games/transportation/MBTA/MassTrack/", 
                                      route, " demand.csv"), stringsAsFactors = FALSE)))
}

get_entries_route <- function(route)
{
    if(route == "Ethan Allen")
        return(c(8243, 2202, 2606, 9413, 4227, 687, 256, 211, 716, 632, 499, 22039)/365)
    if(route == "Vermonter")
        return(c(4401, 21400, 6106, 8504, 2302, 15983, 1197, 2117, 5399, 18963, 13780,
                 4136, 384, 2500, 316, 564, 371, 4105, 1419, 2686, 33666, 1989, 404, 1520,
                 7555, 1729, 1749, 1044, 641, 8370) / 365 / 2)
    if(route == "Vermonter - Knowledge Corridor")
        return(c(4663, 22751, 6510, 9067, 2439, 16608, 1243, 2210, 5524, 19183, 6592, 15155, 820,
                 4153, 391, 2565, 324, 578, 379, 4332, 1485, 2850, 36063, 2133, 416, 1615, 8109, 
                 1811, 1837, 1093, 663, 8847) / 365 / 2)
    if(route == "Downeaster")
        return(c(33327, 14997, 171431, 16817, 47813, 50507, 55144, 58893, 79908, 38481, 22754, 
                 426776)/ 365/2)
                 
}

finish <- function(route, df, load_profile)
{
    if(is.numeric(route))
        return
    write.csv(df, str_c("C://users/John/Google Drive/Mind Games/Transportation/MBTA/MassTrack/", route, 
                        " Flow.csv"))
    write.csv(load_profile, str_c("C://users/John/Google Drive/Mind Games/Transportation/MBTA/MassTrack/", route, 
                        " Load profile.csv"))
}

create_load_profile <- function(df)
{
    stations = colnames(df)
    lp = data.frame("station" = rep(NA, 0), "ons.down.offs.up" = rep(NA, 0), "offs.down.ons.up" = rep(NA, 0), 
                                         "load.leaving.down.arriving.up" = rep(NA, 0), "load.arriving.down.leaving.up" = rep(NA, 0))
    
    for(i in 1:nrow(df))
    {
        ons = 0
        offs = 0
        for(j in 1:nrow(df))
        {
            if(i > j)
                offs = offs + df[i,j]
            else 
                ons = ons + df[i,j]
        }
        lp[nrow(lp) + 1,] = c(stations[i], ons, offs, 0, 0)
    }

    j = nrow(lp)
    lpd = as.numeric(lp$load.arriving.down.leaving.up)
    lpu = as.numeric(lp$load.leaving.down.arriving.up)
    down = as.numeric(lp$ons.down.offs.up)
    up = as.numeric(lp$offs.down.ons.up)
    diff = (sum(down) - sum(up))/2
    up = up * (((sum(up) - diff) / sum(up)) ^ sgn(diff))
    down = down * ((sum(down) / (sum(down) - diff)) ^ sgn(diff))

    lpu = c(lpu, 0)
    lpd = c(0, lpd)
    
    for(i in 1:nrow(lp))
    {
        lpd[i+1] = lpd[i] + down[i] - up[i]
        lpu[j] = lpu[j+1] - down[j] + up[j]
        j = j - 1
    }
    lpd = lpd[-1]
    lpu = lpu[-length(lpu)]
    lp$load.arriving.down.leaving.up = lpu
    lp$load.leaving.down.arriving.up = lpd
    return(lp)
}

sgn <- function(num)
{
    if(num == 0)
        return(num)
    if(num < 0)
        return(-1)
    return(1)
}