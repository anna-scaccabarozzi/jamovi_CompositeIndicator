
# This file is a generated template, your changes will not be overwritten

compareovertimeClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "compareovertimeClass",
    inherit = compareovertimeBase,
    private = list(
        .run = function() {
          
          
          ###### 1) Correlation matrix (print): ################################
          #self$results$cor_mat$setContent(cor(self$data[self$options$indicators]))  
          
          ##### 1) Getting started  ############################################
          self$results$getting_started$setContent("<h2>Getting started</h2>
                                                    <div>
                                                    This module allows you to compute several composite indicators for one or more time periods<br>.
                                                    You can compute one or more of the following composite indicators:<br>
                                                    - generalized mean (or power mean) of exponent p, with p taking value in {-1,0,1,2}<br>
                                                    - Mazziotta-Pareto Index (MPI)<br>
                                                    - Adjusted Mazziotta-Pareto Index (AMPI)<br>
                                                    
                                                    <br>
                                                    <b>Input data</b><br>
                                                    Your input data can be in either wide or long format. Data are automatically reshaped to the most suitable format for each
                                                    composite indicator.<br>
                                                    Data should contain the columns of the individual indicators that you want to combine, <b>Indicators names</b>, the column
                                                    that identifies your statistical units, <b>Statistical unit</b>, and the column which specifies the time period for each observation, 
                                                    <b>Time column</b>, if data are in long format.<br>
                                                    No missing data should be included in your dataset.<br>
                                                    
                                                    <br>
                                                    <b>Output</b><br>
                                                    The results (scores and ranks) are shown in a table. They are exported in a .csv file, and they can be added in the current dataset in jamovi, too, if desired.<br>
                                                    
                                                    <br>
                                                    You can customize several options:<br>
                                                    <br>
                                                    <b>Data structure options</b><br>
                                                    You should specify <b>whether your current dataset is in long format</b> (TRUE or FALSE).
                                                    Then, you should input:<br>
                                                    - the <b>name of your time column</b>, if your data is in long format, or any new name otherwise (e.g. period)<br>
                                                    - the <b>periods</b> in your dataset, comma separated, for instance 2014, 2015<br>
                                                    - the <b>number of periods</b> in your dataset<br>
                                                    - the <b>Polarity</b> of each indicator, which can be either positive (POS) or negative (NEG). For instance, for two indicators with positive polarity you should input: POS, POS.<br>

                                                    
                                                    <br>
                                                    <b>AMPI/MPI options</b><br>
                                                    First, <b>select which indicators</b> you want to compute: AMPI and/or MPI.
                                                    Then you should input:<br>
                                                    - the <b>Reference values</b> of each indicator, required to compute the AMPI. Suppose you have two indicators, empl and inc, with reference values 60 and 50, respectively.
                                                    Your input should have the following structure: list(empl=50, inc=60).<br>
                                                    Then you can select the <b>Penalty</b> of your composite indicator: Positive or Negative.<br>
                                                    
                                                    <br>
                                                    <b>Generalize mean options</b>:<br>
                                                    You can decide to compute one or more of the following composite indicators:<br>
                                                    - harmonic mean<br>
                                                    - geometric mean<br>
                                                    - arithmetic mean<br>
                                                    - quadratic mean<br>
                                                    For each of them, you can specify a different normalization method that we will be used to normalize the individual indicators. It can be one of the following:<br>
                                                    - None: individual indicators are not normalized<br>
                                                    - 1: standardization<br>
                                                    - 2: min-max<br>
                                                    - 3: ranking<br>
                                                    
                                                    <br>
                                                    Finally, you can specifiy the <b>Saving path</b> where the output dataset will be saved. The path should end with the filename you choose
                                                    for the final dataset, and it should end with the .csv extension.<br>
                                                  </div")
          
          
          
          ###### -----DEFINE FUNCTIONS #########################################
          # NORMALIZATION (for generalized mean)  
          apply_normalization <- function(norm_method,
                                          mydf, 
                                          indic_col, 
                                          polarity_vec) 
          {
            if (norm_method == 'None') {
              data_norm <- mydf
            } else {
              data_norm <- Compind::normalise_ci(x=mydf,
                                                 indic_col=indic_col,
                                                 polarity = polarity_vec, 
                                                 method = stringr::str_sub(norm_method, 1, 1))$ci_norm 
            }
            # output: df with normalized individual indicators (colnames are colnames of the individual indicators)
            return(data_norm)
          }
          
          
          # ADD COLUMNS 
          add_scores_ranks_to_df <- function(df_name, df,
                                             results_compind,
                                             name_col_scores,
                                             name_col_rank,
                                             column_stat_unit) {
            
            if (exists(df_name) && is.data.frame(get(df_name))) { 
              # Add cols to existing df 
              df[[name_col_scores]] = results_compind
              df[[name_col_rank]] = rank(-results_compind, na.last=T, ties.method = "average")
            } else {
              # Create df
              df = data.frame(column_stat_unit)  
              df[[name_col_scores]] = results_compind
              df[[name_col_rank]] = rank(-results_compind, na.last=T, ties.method = "average") 
            }
            return(df)
          }
          
          ##### MPI function ###################################################
          # start from Compind::ci_mpi
          # but modify: 
          # -take standardization out of the function (because it does not differentiate between indicators with positive vs negative polarity)
          # -so, only perform the aggregation 
          
          ci_mpi_MODIFIED1 <<- function(x,
                                        indic_col,
                                        penalty)
          {
            x_num   <- x[,indic_col]  
            n_indic <- dim(as.matrix(x_num))[2]
            n_unit  <- dim(as.matrix(x_num))[1]
            
            # Numeric check
            if (n_indic<2)
            {
              stop(paste("There must be at least two simple indicators!"))
            }
            
            for (i in seq(1,n_indic))
            {
              if (!is.numeric(x_num[,i]))
              {
                stop(paste("Data set not numeric at column:",i))
              }
            }
            
            
            for (i in seq(1,n_unit))
            {
              for (j in seq(1,n_indic))
              {
                if (is.na(x_num[i,j]))
                {
                  message(paste("Pay attention: NA values at column:",i,", row",j,". Composite indicator has been computed, but results may be misleading, Please refer to OECD handbook, pg. 26."))
                  #       options(warn=-2)
                }
              }
            }
            
            
            
            #punto A
            # Ma <- colMeans(x_num) 
            # Sqm <- (apply(x_num,2,var))^0.5
            # S=10
            # M=100
            # z = ((x_num-Ma)/Sqm)*S + M
            
            #z <- x_num
            
            #punto B
            Ma_z <- apply(x_num,1,base::mean)
            Sqm_z <- (apply(x_num,1,stats::var))^0.5
            cv <- Sqm_z / Ma_z
            
            #punto C
            if (penalty=="POS") {
              ci_mpi_est <- Ma_z*(1-cv^2)
            } else {
              ci_mpi_est <- Ma_z*(1+cv^2)
            }
            
            r<<-list(ci_mpi_est=ci_mpi_est, ci_method="mpi")
            r$call<-match.call()
            class(r)<-"CI"
            return(r)
            
            
          }
          
          
          ###### -----SET PARAMS WHICH ARE VALID FOR ALL CI: ###################
          id_column = self$options$stat_unit 
          separator = "."
          separator_colname = '_'
          # polarity vec: in two steps:
          polarity_vec = unlist(strsplit(self$options$polarity, ","))
          polarity_vec = gsub('\\s', "", polarity_vec)  # here strip spaces
          
          vector_ci_names_computed_t = c()
          
          # Parameters depending on input data format:
          if (self$options$is_long_format == FALSE) {
            indicators_names = unique(gsub(paste0('\\',separator,"[0-9]+$"), "", self$options$indicators)) #remove the period from the colnames
            time_column = self$options$time_column  # name
            
          } else if (self$options$is_long_format == TRUE) { 
            indicators_names = self$options$indicators
            time_column = self$options$time_column_long # colname
          }
          
          ###### 2) AMPI #########################################################################
          if (self$options$compute_ampi == TRUE) {
            
            # - If not in long format: reshape to long
            if (self$options$is_long_format == FALSE) {
              mydata <-reshape(self$data, 
                               varying= self$options$indicators, 
                               direction="long", 
                               timevar = time_column,
                               idvar=id_column, 
                               sep=separator)
              rownames(mydata) <- 1:nrow(mydata)
              
            } else if (self$options$is_long_format == TRUE) { 
              mydata <- self$data 
            }
            
            
            # - period: from numeric to factor
            mydata[[time_column]] <- as.factor(mydata[[time_column]]) # note: levels are ordered, unique values not ordered
            
            # we consider, for each indicator, the mean of the base year (base period selected by the user)
            # -'goalposts' (actually: reference values):
            
            # method 1: colmeans of base year as default:
            #df_base_year = mydata %>% filter(mydata[[time_column]] == self$options$base_period)   
            #goals = as.vector(colMeans(df_base_year[indicators_names]))
            # method 2: 
            # reference values user defined, form string to vector:
            #goals = unlist(strsplit(self$options$ref, ","))  
            #goals = as.numeric(gsub('\\s', "", goals))
            #self$results$data_scores$setContent(goals)
            
            # from string "list(ind1 = ref1, ind2=ref2, ...)"
            # to corresponding  named list --> list(ind1 = ref1, ind2=ref2, ...):
            goals_list = eval(parse(text=self$options$ref)) 
            
            #goals = unlist(strsplit(self$options$ref, ","))
            #goals = unlist(strsplit(goals, ":"))
            #goals[seq(1, length(mylist), 2)]) # select all indexes in odd positions which should be indicators names
            
            
            # - compute scores
            #(note: normalization already included)
            # Reorder and unlist goalposts:
            goals = unlist(goals_list[c(indicators_names)], use.names=FALSE)
            #self$results$data_scores$setContent(list('goals_list' =goals_list, 
            #                                         'goals_vec' = goals,
            #                                         'indicators_names' = indicators_names,
            #                                         'indicators' = match(indicators_names, colnames(mydata)),
            #                                         'mydata' = mydata) )
            # note:
            # if data are wide: 
            #if order of the indicators is reversed (1°->2°, 2°->1°): positions numbers are still 3,4
            # because we select them when we do reshape from wide to long format
            # if data are long:
            # if order of indicators is reversed, positions numbers are reversed: from 3,4 to 4,3
            # ----> so, in any case, it is correct to adapt the order of goals based on the order of indicator_names
            
            # in long format, I also verified that, if insert as reference values the indicators value of a specific
            # region in 2010, the AMPI_2010 is 100
            
            results <- Compind::ci_ampi(x=mydata,
                                        indic_col=match(indicators_names, colnames(mydata)),
                                        gp = goals, 
                                        time = mydata[[time_column]], 
                                        polarity = polarity_vec,        
                                        penalty = self$options$penalty)
            
            for(i in 1:length(unique(mydata[[time_column]]))) { 
              colnames(results$ci_ampi_est)[i] = paste0('ampi', separator, levels(mydata[[time_column]])[i])
            }
            
            
            # - compute ranks:
            for(col in colnames(results$ci_ampi_est)) {
              results$ci_ampi_est[,paste0('rank', separator_colname, col)] <- rank(-results$ci_ampi_est[, col], na.last=T, ties.method = "average")
            }
            
            # - create complete dataset (in wide format) with region, individual indicators, scores and rankings, reorder columns by year (year: suffix as numbers)
            mydata <- cbind(reshape(mydata, idvar = id_column, timevar = time_column, direction = "wide", sep = separator),
                            results$ci_ampi_est) %>%
              #select(id_column, contains('ampi')) %>% 
              select(id_column, order(gsub(".*?([0-9]+)$", "\\1", colnames(.))))  #\\1 extract the group in ()
            
            # Append col names:
            vector_ci_names_computed_t = append(vector_ci_names_computed_t, 
                                                names(results$ci_ampi_est)[order(gsub(".*?([0-9]+)$", "\\1", names(results$ci_ampi_est)))])
            
          }
          
          
          
          
          ##### 3) OTHER METHODS (MPI, GENERALIZED MEAN) ###########################################################################
          
          ####### a) Get the correct data format ( we will add the scores to it, but we need it also to get the vec of periods:
          # we need dataset in WIDE format to compute the MPI and the generalized means
          if (exists("mydata") && is.data.frame(get("mydata"))) {
            # - it exists if we computed AMPI first, in this case we already reshaped to wide before, so we go on:
            mydata <- mydata   
          } else if (!(exists("mydata") && is.data.frame(get("mydata")))) {
            # - if it doesn't exists yet, if long we reshape self$data to wide, else mydata are self$data:
            if (self$options$is_long_format == TRUE) {
              mydata <- reshape(self$data, idvar = id_column, timevar = time_column, direction = "wide", sep=separator)
            } else if (self$options$is_long_format == FALSE) {
              mydata <- self$data
            }
          } 
          
          ####### b) Get composite indicators 
          periods = unlist(strsplit(x = self$options$periods, split= "\\s*,\\s*")) #separated by comma, preceeded or followed by any space
          
          # - proceed separately by period:
          for (t in periods) { 
            
            # Select only the individual indicators at time t:
            mydata_sel = mydata %>% 
              select(!grep(paste0('^ampi', separator, '[0-9]+$', '|', '^rank', separator_colname, 'ampi', separator, '[0-9]+$'),
                           colnames(mydata), value = TRUE) & contains(t))  
            
            # Vec of individual indicators indexes:
            indic_indexes =  1:ncol(mydata_sel)       
            
            #### MPI ####
            if (self$options$compute_mpi == TRUE) { 
              type_aggr = 'mpi'
              
   
              # normalize data with standardization
              data_norm = normalise_ci(mydata_sel,
                                       indic_indexes,
                                       polarity_vec,
                                       method=1,z.mean=100, z.std=10) 
              # compute scores (use the new mpi function, defined at the beginning,
              # because the package function ci_mpi does not distinguish between
              # pos and neg polarity. It is the same function
              # also defined and used in the mpi analysis of
              # this same module compositeindicators)
              results <- ci_mpi_MODIFIED1(x=data_norm$ci_norm,
                                          indic_col =  indic_indexes, 
                                          penalty= self$options$penalty
              )
              
              
              
              
              # Create df/add cols of scores and ranks to df:
              mydata <- add_scores_ranks_to_df(df_name = "mydata", df = mydata,
                                               results_compind = results$ci_mpi_est,
                                               name_col_scores = paste0(type_aggr, separator, t),
                                               name_col_rank = paste0(type_aggr, separator_colname, 'rank', separator, t),
                                               column_stat_unit = mydata[[self$options$stat_unit]]) %>% 
                select(self$options$stat_unit, order(gsub(".*?([0-9]+)$", "\\1", colnames(.))))
              
              # Append col names of scores and ranks to the vector of ci names:
              vector_ci_names_computed_t = append(vector_ci_names_computed_t, c(paste0(type_aggr, separator, t),
                                                                                paste0(type_aggr, separator_colname, 'rank', separator, t)))
            }
            
            
            #### HARMONIC MEAN ####
            #if (self$options$compute_harmonic_mean == TRUE |
            #    self$options$compute_mean == TRUE |
            #    self$options$compute_quadratic_mean == TRUE) {
            
            #  if (self$options$compute_harmonic_mean == TRUE) {
            #    type_aggr = 'harmonic_mean'
            
            #}
            
            
            if (self$options$compute_harmonic_mean == TRUE) {
              type_aggr = 'harmonic_mean'
              
              # Normalize data with the chosen method:
              # df with normalized invidual indicators:
              data_norm <- apply_normalization(norm_method = self$options$norm_harmonic_mean,
                                               mydf = mydata_sel,
                                               indic_col = indic_indexes,  
                                               polarity_vec = polarity_vec) 
              
              # Compute scores
              results <- Compind::ci_generalized_mean(x = data_norm,
                                                      indic_col = indic_indexes, 
                                                      p = -1,
                                                      na.rm = FALSE)
              
              # Create df/add cols of scores and ranks to df:
              mydata <- add_scores_ranks_to_df(df_name = 'mydata',df = mydata,
                                               results_compind = results$ci_generalized_mean_est,
                                               name_col_scores = paste0(type_aggr, separator, t),
                                               name_col_rank = paste0(type_aggr, separator_colname, 'rank', separator, t),
                                               column_stat_unit = mydata[[self$options$stat_unit]]) %>% 
                select(self$options$stat_unit, order(gsub(".*?([0-9]+)$", "\\1", colnames(.))))
              
              # Append col names of scores and ranks to the vector of ci names:
              vector_ci_names_computed_t = append(vector_ci_names_computed_t, 
                                                  c(paste0(type_aggr, separator, t), paste0(type_aggr, separator_colname, 'rank', separator, t)))
              
            }
            
            #### GEOMETRIC MEAN #####
            if (self$options$compute_geomean == TRUE) {
              type_aggr = 'geomean'
              
              # Geomean-specific controls:
              # - if want to standardize: error -> Stop
              if (stringr::str_sub(self$options$norm_geomean, 1, 1) == '1') {
                stop(paste('Please choose a different normalization method! You cannot apply geometric mean on standardized data'))
              }
              # - if there are negative values in the numeric columns: error -> Stop
              if (any(mydata_sel[indic_indexes] <0)) {   
                stop(paste('Please choose a different aggregation method! Your data include negative values'))
              }
              
              
              # Normalize data with the chosen method:
              data_norm <- apply_normalization(norm_method = self$options$norm_geomean,
                                               mydf = mydata_sel,
                                               indic_col = indic_indexes, 
                                               polarity_vec = polarity_vec) 
              
              # Compute scores
              results <- Compind::ci_geom_gen(x=data_norm,
                                              indic_col= indic_indexes, 
                                              meth='EQUAL')
              
              # Create df/ add cols of scores and ranks to the df:
              mydata <- add_scores_ranks_to_df(df_name = 'mydata', df = mydata,
                                               results_compind = results$ci_mean_geom_est,
                                               name_col_scores = paste0(type_aggr, separator, t),
                                               name_col_rank = paste0(type_aggr, separator_colname, 'rank', separator, t),
                                               column_stat_unit = mydata[[self$options$stat_unit]]) %>% 
                select(self$options$stat_unit, order(gsub(".*?([0-9]+)$", "\\1", colnames(.))))
              
              # Append col names of scores and ranks to the vector of ci names:
              vector_ci_names_computed_t = append(vector_ci_names_computed_t, 
                                                  c(paste0(type_aggr, separator, t), paste0(type_aggr, separator_colname, 'rank', separator, t)))
              
            }
            
            #### ARITHMETIC MEAN ####
            if (self$options$compute_mean == TRUE) {
              type_aggr = 'arithmetic_mean'
              
              # Normalize data with the chosen method:
              data_norm <- apply_normalization(norm_method = self$options$norm_mean,
                                               mydf = mydata_sel,
                                               indic_col = indic_indexes,  
                                               polarity_vec = polarity_vec) 
              
              # Compute scores
              results <- Compind::ci_generalized_mean(x = data_norm,
                                                      indic_col = indic_indexes, 
                                                      p = 1,
                                                      na.rm = FALSE)  
              
              # Create df/ add cols of scores and ranks to the df:
              mydata <- add_scores_ranks_to_df(df_name = 'mydata',df = mydata,
                                               results_compind = results$ci_generalized_mean_est,
                                               name_col_scores = paste0(type_aggr, separator, t),
                                               name_col_rank = paste0(type_aggr, separator_colname, 'rank', separator, t),
                                               column_stat_unit = mydata[[self$options$stat_unit]]) %>% 
                select(self$options$stat_unit, order(gsub(".*?([0-9]+)$", "\\1", colnames(.))))
              
              # Append col names of scores and ranks to the vector of ci names:
              vector_ci_names_computed_t = append(vector_ci_names_computed_t, 
                                                  c(paste0(type_aggr, separator, t), paste0(type_aggr, separator_colname, 'rank', separator, t)))
            }
            
            
            #### QUADRATIC MEAN ####
            if (self$options$compute_quadratic_mean == TRUE) {
              type_aggr = 'quadratic_mean'
              
              # Normalize data with the chosen method:
              data_norm <- apply_normalization(norm_method = self$options$norm_quadratic_mean,
                                               mydf = mydata_sel,
                                               indic_col = indic_indexes,  
                                               polarity_vec = polarity_vec) 
              
              # Compute scores
              results <- Compind::ci_generalized_mean(x = data_norm,
                                                      indic_col = indic_indexes, 
                                                      p = 2,
                                                      na.rm = FALSE)  
              
              # Create df/ add cols of scores and ranks to the df:
              mydata <- add_scores_ranks_to_df(df_name = 'mydata',df = mydata,
                                               results_compind = results$ci_generalized_mean_est,
                                               name_col_scores = paste0(type_aggr, separator, t),
                                               name_col_rank = paste0(type_aggr, separator_colname, 'rank', separator, t),
                                               column_stat_unit = mydata[[self$options$stat_unit]]) %>% 
                select(self$options$stat_unit, order(gsub(".*?([0-9]+)$", "\\1", colnames(.))))
              
              # Append col names of scores and ranks to the vector of ci names:
              vector_ci_names_computed_t = append(vector_ci_names_computed_t, 
                                                  c(paste0(type_aggr, separator, t), paste0(type_aggr, separator_colname, 'rank', separator, t)))
              
            }
          } 
          
          # Print df 
          #if (nrow(mydata) >= 50){          
          #  self$results$data_scores$setContent(head(mydata))
          #} else {
          #  self$results$data_scores$setContent(mydata)
          #  
          #}
          
          ##### 4) TABLE ####
          
          # Add the columns of the dataset (excluding individual indicators and rank columns):
          cols_to_add_table = grep(paste0('^.*rank', '.*', '[0-9]+$'), vector_ci_names_computed_t, value=TRUE, invert = TRUE)
          cols_to_add_table = cols_to_add_table[order(gsub(".*?([0-9]+)$", "\\1", cols_to_add_table))]
          cols_to_add_table = append(c(self$options$stat_unit ),cols_to_add_table)
          
          
          # Initialize the table
          table <- self$results$CI_table
          
          for (i in 1:length(cols_to_add_table)) {
            table$addColumn(cols_to_add_table[i])
          }
          
          #for (i in 1:ncol(mydata)) {
          #  table$addColumn(names(mydata[i]))
          #}
          
          # Set row content for each row:
          
          for (i in 1:nrow(mydata)) {
            named_list = list()
            for (j in cols_to_add_table) {  #names(mydata)
              named_list[['ID']] = i
              named_list[[j]] =  mydata[i,j] 
              if (j == id_column) {
                named_list[[j]] = stringr::str_sub(mydata[i,j], 1, 21)}
            }
            table$addRow(rowKey=i, values=named_list)
          }
          
          
          ##### SAVE CSV  #############################################################################
          # note: no need to convert slash
          utils::write.csv(mydata, self$options$saving_path, row.names = FALSE)
          
          
          ##### 5) FILL OUTPUT #######################################################################################
          # note:
          # wide: ncol = n_possible_ci * 2 * n_periods)
          # long: ncol = n_possible_ci * 2
          
          # - Vec of cols of scores and ranks for each composite indicator, with no time info:
          vector_ci_names = c('ampi', 'rank_ampi', 'mpi', 'mpi_rank', 'harmonic_mean', 'harmonic_mean_rank', 'geomean', 'geomean_rank', 'arithmetic_mean', 'arithmetic_mean_rank', 'quadratic_mean' , 'quadratic_mean_rank' )
          # basically, vector_ci_names are all the colnames for the df in long format (if all ci are computed)
          
          # - Vec of bool: for each ci, if we compute it or not:
          vector_bool <- c(self$options$compute_ampi, self$options$compute_mpi, self$options$compute_harmonic_mean, self$options$compute_geomean, self$options$compute_mean, self$options$compute_quadratic_mean)
          
          # - Create vector with column names of scores and rank for the methods that were not computed:
          vector_not_computed <- c()
          j=1
          for (i in 1:length(vector_bool)) {  #1:6
            if (vector_bool[i]==FALSE) {
              vector_not_computed <- append(x=vector_not_computed, values = vector_ci_names[j:(j+1)]) 
            }
            j = j +2
          }
          
          
          # - Vector of all col names, computed and not computed, adding info of periods:
          vector_ci_names_t = c() 
          for (t in periods) {
            t = as.character(t)
            for (i in vector_ci_names) {
              vector_ci_names_t = append(vector_ci_names_t, paste0(i,separator,t))
            }
          }
          #vector_ci_names_t = vector_ci_names_t[order(gsub(".*?([0-9]+)$", "\\1", vector_ci_names_t))]
          #self$results$data_scores$setContent(vector_ci_names_t)
          
          
          
          # -Vector of not computed methods, adding period to cols to have all the missing cols
          vector_not_computed_t = c()
          for (t in periods) {
            t = as.character(t)
            for (i in vector_not_computed) {
              vector_not_computed_t = append(vector_not_computed_t, paste0(i,separator,t))
            }
          }
          
          # For columns of scores and ranks of methods not computed, fill with 0s (we cannot put a string because type column is continuous, we will get all 1s,
          # so better fill with 0s)
          for (i in 1:length(vector_not_computed_t)) {
            mydata[vector_not_computed_t[i]] <- 0
          }
          mydata <- mydata %>%  select(self$options$stat_unit, order(gsub(".*?([0-9]+)$", "\\1", colnames(.))))
          
          # Filling with output depends on data format:
          
          # --If opened dataset is in wide format:
          if (self$options$is_long_format == FALSE) {
            
            # Fill the new columns with scores and ranks: rownumbers should be the same for  mydata and self$data, fill with values:
            if (self$options$composite_indicators && self$results$composite_indicators$isNotFilled()) {
              # Set row numbers (may be useful if in future we will use na.omit). For the moment, we assume we do not use na.omit
              self$results$composite_indicators$setRowNums(rownames(self$data))
              # Set values:
              self$results$composite_indicators$setValues(mydata %>% select(vector_ci_names_t))
              
              #self$results$data_scores$setContent(mydata)
            }
            # Set column titles
            for (i in 1:length(vector_ci_names_t)) {
              self$results$composite_indicators$setTitle(title = vector_ci_names_t[i], index = i)  
            }
          }
          
          
          # --If opened dataset is in long format:
          if (self$options$is_long_format == TRUE) {
            
            
            # Fill the new columns with scores and ranks: rownumbers should be self$data (which is in long format; hence different from mydata which is in whide format), fill with values:
            if (self$options$composite_indicators && self$results$composite_indicators$isNotFilled()) {
              # Set row numbers (may be useful if in future we will use na.omit). For the momemt, we assume we do not use na.omit
              self$results$composite_indicators$setRowNums(rownames(self$data))
              # reshape long then set values:
              self$results$composite_indicators$setValues(reshape(mydata, 
                                                                  varying=  colnames(mydata)[! colnames(mydata) %in% self$options$stat_unit],    
                                                                  direction="long", 
                                                                  timevar = time_column,
                                                                  idvar=self$options$stat_unit, 
                                                                  sep=separator,
                                                                  new.row.names = 1:nrow(self$data)) %>%
                                                            select(vector_ci_names))
            } 
            
            
            # Set column titles
            for (i in 1:length(vector_ci_names)) {
              self$results$composite_indicators$setTitle(title = vector_ci_names[i], index = i)
            }
            
          }
          
          
        }
        #,
        
        
    )
)
