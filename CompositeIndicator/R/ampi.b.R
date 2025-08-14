
# This file is a generated template, your changes will not be overwritten

ampiClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "ampiClass",
    inherit = ampiBase,
    private = list(
        .run = function() {
          
          ###### PARAMS  #######################################################
          # polarity vec: in two steps:
          polarity_vec = unlist(strsplit(self$options$polarity, ","))
          polarity_vec = gsub('\\s', "", polarity_vec)  # here strip spaces
          
          # separator: of AMPI scores, separate 'ampi' from period
          separator = '.'
          separator_colname = '_'
          
          
          
          ##### 1) Correlation matrix (print): #################################
          #self$results$cor_mat$setContent(cor(self$data[which(names(self$data)%in%names(self$data)[sapply(self$data, is.numeric)])]))
          
          
          ##### 1) Getting started #############################################
          self$results$getting_started$setContent("<h2>Getting started</h2>
                                                    <div>
                                                    This module allows you to compute the <b>Adjusted Mazziotta-Pareto Index (AMPI)</b> composite indicator for one time period.<br>
                                                    Your input data should be in long format. They should contain the columns of the individual indicators
                                                    that you want to combine, <b>Indicators names</b>, the column that identifies your statistical units, <b>Statistical unit</b>,
                                                    and the column which specifies the time period for each observation: <b>Time column</b>.<br>
                                                    If your data are in wide format, I advise you to load the module jReshape from the jamovi library and to use it to reshape
                                                    your dataset.<br>
                                                    No missing data should be included in your dataset.<br>
                                                    <br>
                                                    After selecting the individual indicators, the statistical unit and the time columns, you should input the <b>Polarity</b> of each indicator,
                                                    which can be either positive (POS) or negative (NEG). For instance, for two indicators with positive polarity you should input: POS, POS.<br>
                                                    <br>
                                                    In the next step, you input the <b>Reference values</b> of each indicator. Suppose you have two indicators, empl and inc, with reference values 60 and 50, respectively.
                                                    Your input should have the following structure: list(empl=50, inc=60).<br>
                                                    <br>
                                                    Then you select the <b>Penalty</b> of your composite indicator: Positive or Negative.<br>
                                                    <br>
                                                    Finally, you can specifiy the <b>Saving path</b> where the output dataset will be saved. The path should end with the filename you choose
                                                    for the final dataset, and it should end with the .csv extension.<br>
                                                    </div")
          
          ##### 2) parameters ###########################################
          mydata <- self$data 
          indicators_names = self$options$indicators
          time_column = self$options$time_column_long
          
          
          
          ##### 3) Prepare data: time column, reference values ###################
          
          # - reference values
          # from string "list(ind1 = ref1, ind2=ref2, ...)"
          # to corresponding  named list --> list(ind1 = ref1, ind2=ref2, ...):
          goals_list = eval(parse(text=self$options$ref)) 
          # goals list is named list of reference values.
          # why we input reference values, instead of goalposts?
          # --> note that, in the ci_ampi R function, 'goals'  
          # are the reference value/values used to compute goalposts,
          # one for each individual indicator.
          
          # Reorder and unlist reference values:
          # we adapt the order of goals based on the order of indicator_names
          goals = unlist(goals_list[c(indicators_names)], use.names=FALSE)
          
          
          
          ##### 4) Compute scores and rank  ######################################
          #(note: normalization already included in the command ci_ampi)
          results <- Compind::ci_ampi(x=mydata,
                                      indic_col=match(indicators_names, colnames(mydata)), 
                                      gp = goals, 
                                      time = mydata[[time_column]], 
                                      polarity = polarity_vec,        
                                      penalty = self$options$penalty)
          #self$results$cor_mat$setContent(results)
          
          
          
          for(col in colnames(results$ci_ampi_est)) {  #col are the periods, e.g. years
            i = which(colnames(results$ci_ampi_est) == col)
            colnames(results$ci_ampi_est)[i] = paste0('ampi', separator, col)
          }          
          
          
          # - compute ranks:
          for(col in colnames(results$ci_ampi_est)) {
            results$ci_ampi_est[,paste0('rank', separator_colname, col)] <- rank(-results$ci_ampi_est[, col], na.last=T, ties.method = "average")
          }     
          
          
          
          
          ##### 5) Prepare dataset  #####################################
          # - create complete dataset (in wide format) with region, individual indicators, scores and rankings, reorder columns by year (year: suffix as numbers)
          mydata <- cbind(reshape(mydata,
                                  idvar = self$options$stat_unit,
                                  timevar = time_column,
                                  direction = "wide",
                                  sep = separator),
                          results$ci_ampi_est) %>%
            #select(id_column, contains('ampi')) %>% 
            select(self$options$stat_unit, order(gsub(".*?([0-9]+)$", "\\1", colnames(.))))  #\\1 extract the group in ()
          
          
          
          # Print df 
          #if (nrow(mydata) >= 50){          
          #  self$results$data_scores$setContent(head(mydata))
          #} else {
          #  self$results$data_scores$setContent(mydata)
          #  
          #}
          
          ##### 4) TABLE #######################################################
          
          # Add the columns of the dataset (excluding individual indicators and rank columns):
          cols_to_add_table = grep(paste0('^.*rank', '.*', '[0-9]+$'), colnames(results$ci_ampi_est), value=TRUE, invert = TRUE)
          # Add the columns of the dataset (excluding individual indicators and rank columns):
          #cols_to_add_table = colnames(results$ci_ampi_est)
          cols_to_add_table = cols_to_add_table[order(gsub(".*?([0-9]+)$", "\\1", cols_to_add_table))]
          cols_to_add_table = append(c(self$options$stat_unit ),cols_to_add_table)
          #self$results$data_scores$setContent(cols_to_add_table) #3 4
          
          # Initialize the table
          table <- self$results$CI_table
          
          for (i in 1:length(cols_to_add_table)) {
            table$addColumn(cols_to_add_table[i])
          }
          
          # Set row content for each row:
          for (i in 1:nrow(mydata)) {
            named_list = list()
            for (j in cols_to_add_table) {  
              named_list[['ID']] = i
              named_list[[j]] =  mydata[i,j] 
              if (j == self$options$stat_unit) {
                named_list[[j]] = stringr::str_sub(mydata[i,j], 1, 21)}
            }
            table$addRow(rowKey=i, values=named_list)
          }
          
          
          ##### 5) export csv:  ################################################
          utils::write.csv(mydata, self$options$saving_path)
          #self$results$mydata_head$setContent(rownames(mydata)) 
          
          
          
          
          
        } 
    )
)
