#+eval=FALSE
options(scipen = 999)
# Loading packages --------------------------------------------------------

#openxlsx::write.xlsx(last, "E:\\SG.xlsx")
{
    library('RMySQL') #connect to mysql(not in Restricted.r version)
    library('tidyr')
    library('openxlsx') #writing into xls
    library('dplyr') #data wrangling
    library('purrr') #functional programing
    library('stringr') #working with strings
    library('tibble')
    library('car') #tests referenced to regression
    library('gvlma') #general testing of regression assumptions
    library('plm') #panel regression
    library('Hmisc')
    library('Rfast') #find dist
    library('ggplot2') #plotting
    library('stringi') #translation
    library('lubridate')
    library("quantreg") #quantile regression
    library('plotly') #plotting
    library('rvest')
    library('corrplot')
}


# Creating deflator indexes table -----------------------------------------

#Table with year indexes for deflation
deflator <-
    tibble(
        year = c(2004:2016),
        index = c(34.4380651, 42.76957124, 49.06898173, 60.40500034, 77.66787512,
                  87.7791759, 100, 114.200791, 123.102655, 128.4416393, 148.8661831,
                  206.7478252, 242.0957496)
    )

# Compose data into total and per row format ------------------------------

#Compose table with full information
base <- 
    read.csv("correct_id_2.txt", sep="", stringsAsFactors=FALSE) %>% 
    distinct() %>% 
    as_tibble() %>% 
    mutate_if(is.character, ~stri_trans_general(., "ukrainian-latin/bgn")) %>%
    select(matches('^[^X].*'), everything())

# Correcting column names(deleting "X" from the start)
for(i in select(base, matches('X.')) %>% names){
    colnames(base)[colnames(base) == i] = str_sub(i, 2)
}
remove(base2)
#Data frame with reference information
refernce_df = 
    base %>%
    select(-matches('^0.'))

#Data frame based only on farm code, year and indexes
compute_df = 
    base %>%
    select(pre, year, matches('^0.')) %>%
    {
        n = 
            select(., -pre, -year) %>%
            names()
        gather(., 'index', 'data', n)
    } %>%
    separate(index, c('row', 'col'), 'G', remove = F) %>%
    mutate_at(vars(row, col), ~as.integer(.)) %>%
    filter(year != 2016)
    
remove(base)

# Composed table with total production and factors
compute_total = 
    compute_df %>%
    {
        spends_total = 
            filter(compute_df, (row %in% c(280, 285, 295, 310, 315, 320, 325, 330, 335, 340, 345, 451)) & col == 1) %>%
            select(-col, -index) %>%
            spread(row, data) %>%
            rename(
                labor = `280`,
                social_costs = `285`,
                seeds = `295`,
                fert = `310`,
                oil_prod = `315`,
                electricity = `320`,
                oil = `325`,
                repairs = `330`,
                serv_costs = `335`,
                deprec = `340`,
                other_costs = `345`,
                land = `451`
            ) %>%
            left_join(deflator, by = 'year') %>%
            mutate_at(vars(labor:other_costs), ~./(index/100)) %>%
            select(-index)
        # 21,25,26,30,31,23,33,40,41,42,43,44,50,100
        # 21:45, 50:100
        prod = 
            filter(compute_df, row %in% c(21:45, 50:100) & col %in% c(2, 4, 7)) %>%
            select(-index) %>%
            spread(col, data) %>%
            mutate(nominal_price = `7`/`4`) %>% 
            left_join(deflator, by = c('year')) %>%
            mutate(real_price = if_else(`4` == 0 | is.na(`4`), 0, nominal_price / (index/100))) %>%
            {
                join_ref = 
                    refernce_df %>%
                    select(pre, year, obl, ter)
                
                left_join(., join_ref, by = c('pre', 'year'))
            } %>%
            group_by(year, row, obl, ter) %>%
            #filter(., !real_price == 0) %>%
            mutate(real_price_rayon = median(real_price[!real_price == 0])) %>%
            ungroup() %>%
            group_by(year, row, obl) %>%
            mutate(real_price_obl = median(real_price[!real_price == 0])) %>%
            replace(is.na(.), 0) %>%
            mutate(
                sales = real_price * `4`,
                sales_ray = real_price_rayon * `4`,
                sales_obl = real_price_obl * `4`,
                prod = real_price * `2`,
                prod_ray = real_price_rayon * `2`,
                prod_obl = real_price_obl * `2`
            ) %>% 
            ungroup() %>%
            group_by(year, pre) %>%
            summarise_at(vars(sales:prod_obl), ~sum(.)) %>% 
            left_join(., spends_total, by = c('pre', 'year'))
    } %>%
    ungroup() %>%
    # mutate(row_name = str_c(year, pre, sep = "_")) %>%
    # mutate_at(vars(prod:other_costs), ~if_else(land == 0, 0, ./land)) %>%
    # column_to_rownames('row_name')
    
    mutate(
        labor = labor,
        seed = seeds,
        fert = fert,
        machinery = repairs + deprec,
        fuel_energy = oil_prod + oil + electricity,
        other = other_costs + serv_costs,
        constant = social_costs,
        land = land
    ) %>%
    select(year:prod_obl, labor, seed, fert, machinery, fuel_energy, other, constant, land) %>%
    # mutate_at(vars(prod:prod_obl), ~if_else(land == 0, 0, ./land)) %>%
    # mutate(row_name = str_c(year, pre, sep = "_")) %>%
    # column_to_rownames('row_name') %>%
    mutate_at(vars(prod:prod_obl, labor:constant), ~if_else(land == 0, 0, ./land)) %>%
    mutate_at(vars(sales:constant), ~./6) %>% 
    filter(land >= 3000)

compute_total_ray = 
    compute_df %>%
    {
        spends_total = 
            filter(compute_df, (row %in% c(280, 285, 295, 310, 315, 320, 325, 330, 335, 340, 345, 451)) & col == 1) %>%
            select(-col, -index) %>%
            spread(row, data) %>%
            rename(
                labor = `280`,
                social_costs = `285`,
                seeds = `295`,
                fert = `310`,
                oil_prod = `315`,
                electricity = `320`,
                oil = `325`,
                repairs = `330`,
                serv_costs = `335`,
                deprec = `340`,
                other_costs = `345`,
                land = `451`
            ) %>%
            left_join(deflator, by = 'year') %>%
            mutate_at(vars(labor:other_costs), ~./(index/100)) %>%
            select(-index) %>%
            {
                join_ref = 
                    refernce_df %>%
                    select(pre, year, ter)
                
                left_join(., join_ref, by = c('pre', 'year'))
            } %>%
            group_by(year,ter) %>%
            summarise_at(vars(labor:land), ~sum(.))
            
        # 21,25,26,30,31,23,33,40,41,42,43,44,50,100
        # 21:45, 50:100
        prod = 
            filter(compute_df, row %in% c(21:45, 50:100) & col %in% c(2, 4, 7)) %>%
            select(-index) %>%
            spread(col, data) %>%
            mutate(nominal_price = `7`/`4`) %>% 
            left_join(deflator, by = c('year')) %>%
            mutate(real_price = if_else(`4` == 0 | is.na(`4`), 0, nominal_price / (index/100))) %>%
            {
                join_ref = 
                    refernce_df %>%
                    select(pre, year, obl, ter)
                
                left_join(., join_ref, by = c('pre', 'year'))
            } %>%
            group_by(year, row, obl, ter) %>%
            #filter(., !real_price == 0) %>%
            mutate(real_price_rayon = median(real_price[!real_price == 0])) %>%
            ungroup() %>%
            group_by(year, row, obl) %>%
            mutate(real_price_obl = median(real_price[!real_price == 0])) %>%
            replace(is.na(.), 0) %>%
            mutate(
                sales = real_price * `4`,
                sales_ray = real_price_rayon * `4`,
                sales_obl = real_price_obl * `4`,
                prod = real_price * `2`,
                prod_ray = real_price_rayon * `2`,
                prod_obl = real_price_obl * `2`
            ) %>% 
            group_by(year, ter) %>%
            summarise_at(vars(sales:prod_obl), ~sum(.)) %>% 
            left_join(., spends_total, by = c('ter', 'year'))
    } %>%
    ungroup() %>%
    # mutate(row_name = str_c(year, pre, sep = "_")) %>%
    # mutate_at(vars(prod:other_costs), ~if_else(land == 0, 0, ./land)) %>%
    # column_to_rownames('row_name')
    
    mutate(
        labor = labor,
        seed = seeds,
        fert = fert,
        machinery = repairs + deprec,
        fuel_energy = oil_prod + oil + electricity,
        other = other_costs + serv_costs,
        constant = social_costs,
        land = land
    ) %>%
    select(year:prod_obl, labor, seed, fert, machinery, fuel_energy, other, constant, land) %>%
    # mutate_at(vars(prod:prod_obl), ~if_else(land == 0, 0, ./land)) %>%
    # mutate(row_name = str_c(year, pre, sep = "_")) %>%
    # column_to_rownames('row_name') %>%
    mutate_at(vars(prod:prod_obl, labor:constant), ~if_else(land == 0, 0, ./land)) %>%
    mutate_at(vars(sales:constant), ~./10000) %>% 
    filter(land >= 3000)

# Pooled OLS --------------------------------------------------------------
# Pooled OLS total

pooled_ols_total = 
    compute_total %>%
    select(sales:prod_obl)  %>%
    names() %>%
    {
        names(.) <- .
        (.)
    } %>%
    {
        purrr::map(., ~lm(compute_total %>% select(.x) %>% pull ~ 
                       ., data = compute_total %>% select(labor:land)))
    }

par(mfrow=c(2,2))
pooled_ols_total$prod_ray %>% plot
pooled_ols_total$prod_ray %>% summary
durbinWatsonTest(pooled_ols_total$prod)
crPlots(pooled_ols_total$prod)
ncvTest(pooled_ols_total$prod)
vif(pooled_ols_total$prod)
gvlma(pooled_ols_total$prod) 
# CDPF  -------------------------------------------------------------------

# log values in total production
compute_total_logged = 
    compute_total %>%
    filter(land > 3500) %>%
    mutate_at(vars(sales:constant), ~if_else(. == 0, log(0.000000001), log(.)))


# CDPF applied on total values
CDPF_total = 
    compute_total_logged %>%
    select(sales:prod_obl)  %>%
    names() %>%
    {
        names(.) <- .
        (.)
    } %>%
    {
        map(., ~lm(compute_total_logged %>% select(.x) %>% pull ~ 
                       ., data = compute_total_logged %>% select(labor:land)))
    }

CDPF_total$prod_ray$coefficients[-1] %>% sum
CDPF_total$prod_ray %>% summary()
CDPF_total$prod_ray %>% plot
# Fixed effects per  -----------------------------------------------------------

compute_pre_log = 
    compute_total %>%
    mutate_at(vars(sales:constant), ~if_else(. == 0, log(0.000000001), log(.)))

compute_ray_log = 
    compute_total_ray %>%
    mutate_at(vars(sales:constant), ~if_else(. == 0, log(0.000000001), log(.)))




frml = str_c("prod_ray", '~', str_c(select(compute_pre_log, labor:land) %>% names, collapse = '+'))
fixed_pre = plm(frml, data = compute_pre_log, model = 'within', index = c('pre'))
summary(fixed_pre)

frml = str_c("prod_ray", '~', str_c(select(compute_ray_log, labor:land) %>% names, collapse = '+'))
fixed_ray = plm(frml, data = compute_ray_log, model = 'within', index = c('ter'))
summary(fixed_ray)

ef = fixed_pre %>% fixef() %>% as.data.frame() %>% rownames_to_column('pre') %>% select(pre) %>% distinct() %>% pull %>% as.numeric()
ef2 = compute_pre_log %>% select(pre) %>% distinct() %>% pull 
ef2[!(ef2 %in% ef)]

ef_ray = 
    fixed_ray %>% 
    fixef() %>% 
    as.data.frame(row.names = T) %>% 
    rownames_to_column('ter') %>%
    rename(effect = ".") %>%
    mutate(ter =as.numeric(ter)) %>%
    {
        ref = refernce_df %>% select(ter, rayon_name, obl, obl_name, zona_name)
        left_join(., ref, by = 'ter')
    } %>%
    distinct() %>% 
    mutate(effect = ifelse(zona_name == 'Lisostep', effect * 1.5, effect)) %>%
    mutate(effect = ifelse(zona_name == 'Step', effect * 1.05, effect)) %>%
    filter(!row.names(.) == 1) %>%
    mutate(effect = ifelse(effect < 0, 0.5, effect))