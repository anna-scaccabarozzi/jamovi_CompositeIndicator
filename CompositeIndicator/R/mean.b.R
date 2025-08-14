
# This file is a generated template, your changes will not be overwritten

meanClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "meanClass",
    inherit = meanBase,
    private = list(
        .run = function() {
          
          ###### PARAMS  #######################################################
          # polarity vec: in two steps:
          polarity_vec = unlist(strsplit(self$options$polarity, ","))
          polarity_vec = gsub('\\s', "", polarity_vec)  # here strip spaces
          
          
          ###### 1) Correlation matrix (print): #############################################################
          #self$results$cor_mat$setContent(cor(self$data[which(names(self$data)%in%names(self$data)[sapply(self$data, is.numeric)])]))
          
          ##### 1) Getting started #############################################
          self$results$getting_started$setContent("<h2>Getting started</h2>
                                                    <div>
                                                    This module allows you to compute the <b>generalized mean</b> (or power mean) for one time period.<br>
                                                    Your input data should be in wide format. They should contain the columns of the individual indicators
                                                    that you want to combine, <b>Indicators names</b>, and the column that identifies your statistical units, <b>Statistical unit</b>.<br>
                                                    If your data are in long format, I advise you to load the module jReshape from the jamovi library and to use it to reshape
                                                    your dataset.<br>
                                                    No missing data should be included in your dataset.<br>
                                                    <br>
                                                    After selecting the individual indicators and the statistical unit columns, you should input the <b>Polarity</b> of each indicator,
                                                    which can be either positive (POS) or negative (NEG). For instance, for two indicators with positive polarity you should input: POS, POS.<br>
                                                    <br>
                                                    Then you select the <b>Normalization</b> technique that we will be used to normalize the individual indicators. It can be one of the following:<br>
                                                    - None: individual indicators are not normalized<br>
                                                    - 1: standardization<br>
                                                    - 2: min-max<br>
                                                    - 3: ranking<br>
                                                    <br>  
                                                    You select the aggregation method selecting one value of <b>Exponent p</b>:<br>
                                                    - -1: harmonic mean<br>
                                                    -  0: geometric mean<br>
                                                    -  1: arithmetic mean<br>
                                                    -  2: quadratic mean<br>
                                                    <br>
                                                    Finally, you can specifiy the <b>Saving path</b> where the output dataset will be saved. The path should end with the filename you choose
                                                    for the final dataset, and it should end with the .csv extension.<br>
                                                    </div")
          
          
          ###### 2) Compute scores ###########################################################################
          
          # Some specific options are allowed with geometric mean if some conditions are met:
          if (as.numeric(self$options$p)==0 ) {
            if (stringr::str_sub(self$options$normalization, 1, 1) == '1') {
              stop(paste('Please choose a different normalization method! You cannot apply geometric mean on standardized data'))
            } else if (any(self$data[self$options$indicators] <0)) {
              stop(paste('Please choose a different aggregation method! Your data include negative values'))
            }
          }
          
          
          # Compute scores and ranks:
          if (self$options$normalization == 'None') {
            data_norm <- self$data
          } else {
            data_norm1 <- Compind::normalise_ci(x=self$data,
                                                indic_col = which(names(self$data)%in%self$options$indicators),   #which(names(self$data)%in%names(self$data)[sapply(self$data, is.numeric)]),
                                                polarity = polarity_vec,  #self$data[1:length(which(names(self$data)%in%names(self$data)[sapply(self$data, is.numeric)])), ncol(self$data)],
                                                method = stringr::str_sub(self$options$normalization, 1, 1))$ci_norm
            data_norm <- data.frame(self$data[self$options$stat_unit],
                                    data_norm1)
            
          }
          
          if (as.numeric(self$options$p)==0) {
            results <- Compind::ci_geom_gen(x=data_norm,
                                            indic_col=which(names(self$data)%in%self$options$indicators),
                                            meth='EQUAL')
            
            mydata <- data_norm %>%
              mutate(mean_score = results$ci_mean_geom_est,
                     mean_rank = rank(-results$ci_mean_geom_est, na.last=T, ties.method = "average")
              )
          } else {
            
            results <- Compind::ci_generalized_mean(x=data_norm,
                                                    indic_col=which(names(self$data)%in%self$options$indicators),
                                                    p = as.numeric(self$options$p),
                                                    na.rm = FALSE)
            
            mydata <- data_norm %>%
              mutate(mean_score = results$ci_generalized_mean_est,
                     mean_rank = rank(-results$ci_generalized_mean_est, na.last=T, ties.method = "average"))
            
            
          }
          
          
          
          ###### 3) Head dataset (print) ########################################
          # Print df 
          #if (nrow(mydata) >= 50){          
          #  self$results$data_scores$setContent(head(mydata))
          #} else {
          #  self$results$data_scores$setContent(mydata)
          
          #}
          
          
          ##### 4) TABLE #######################################################
          # Initialize the table
          table <- self$results$CI_table
          
          cols_to_add_table = c(self$options$stat_unit, 'mean_score', 'mean_rank')
          
          for (i in 1:length(cols_to_add_table)) {
            table$addColumn(cols_to_add_table[i])
          }
          
          
          
          # Set row content for each row:
          
          for (i in 1:nrow(mydata)) {
            named_list = list()
            for (j in cols_to_add_table) {  #names(mydata)
              named_list[['ID']] = i
              named_list[[j]] =  mydata[i,j] 
              if (j == self$options$stat_unit) {
                named_list[[j]] = stringr::str_sub(mydata[i,j], 1, 21)}
            }
            table$addRow(rowKey=i, values=named_list)
          }
          
          
          ##### SAVE CSV  ######################################################
          utils::write.csv(mydata, self$options$saving_path, row.names = FALSE)
          
          
          
        }
    )
)
