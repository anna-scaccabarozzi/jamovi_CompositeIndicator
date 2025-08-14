
# This file is a generated template, your changes will not be overwritten

mpiClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mpiClass",
    inherit = mpiBase,
    private = list(
        .run = function() {

          
          ###### PARAMS ########################################################
          
          # polarity vec: in two steps:
          polarity_vec = unlist(strsplit(self$options$polarity, ","))
          polarity_vec = gsub('\\s', "", polarity_vec)  # here strip spaces
          
          ######################################################################
          
          
          ##### 1) Getting started
          self$results$getting_started$setContent("<h2>Getting started</h2>
                                                    <div>
                                                    This module allows you to compute the <b>Mazziotta Pareto index (MPI)</b><br>
                                                    Your input data should be in wide format. They should contain the columns of the individual indicators
                                                    that you want to combine, <b>Indicators names</b>, and the column that identifies your statistical units, <b>Statistical unit</b>.<br>
                                                    If your data are in long format, I advise you to load the module jReshape from the jamovi library and to use it to reshape
                                                    your dataset.<br>
                                                    No missing data should be included in your dataset.<br>
                                                    <br>  
                                                    To compute the MPI, you should input the <b>Polarity</b> of each indicator, which can be either positive (POS) or negative (NEG). For instance, 
                                                    for two indicators with positive polarity you should input: POS, POS.<br>
                                                    Then you select the <b>Penalty</b> of your composite indicator: Positive or Negative.<br>
                                                    Finally, you can specifiy the <b>Saving path</b> where the output dataset will be saved. The path should end with the filename you choose
                                                    for the final dataset, and it should end with the .csv extension.<br>
                                                  </div")
          
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
          
    
          
          ###### 2) Compute mpi scores #########################################
          
          # normalize data with standardization
          data_norm = normalise_ci(self$data,
                                   which(names(self$data)%in%names(self$data)[sapply(self$data, is.numeric)]),  #individual_indicators_indexes, the numeric columns of self$data
                                   polarity_vec,
                                   method=1,z.mean=100, z.std=10) 
          # compute scores
          results <- ci_mpi_MODIFIED1(x=data_norm$ci_norm,
                                      indic_col =  match(self$options$indicators, colnames(data_norm$ci_norm)), penalty= self$options$penalty
          )

          
          mydata <- self$data %>%
            mutate(mpi_score = results$ci_mpi_est,
                   mpi_rank = rank(-results$ci_mpi_est, na.last=T, ties.method = "average"))

          
          ##### 4) TABLE ####
          
          
          # Select the colnames of the columns in the table
          cols_to_add_table = append(c(self$options$stat_unit ),c('mpi_score', "mpi_rank"))
          
          
          # Initialize the table
          table <- self$results$CI_table
          
          for (i in 1:length(cols_to_add_table)) {
            table$addColumn(cols_to_add_table[i])
          }
          
          
          # Set row content for each row:
          
          for (i in 1:nrow(mydata)) {
            named_list = list()
            for (j in cols_to_add_table) {  #names(mydata)
              named_list[['ID']] = i
              named_list[[j]] =  mydata[i,j] 
              if (j == self$options$stat_unit ) {
                named_list[[j]] = stringr::str_sub(mydata[i,j], 1, 21)}
            }
            table$addRow(rowKey=i, values=named_list)
          }
          
          ##### SAVE CSV  ######################################################
          utils::write.csv(mydata,
                           self$options$saving_path,  # note: no need to convert slash
                           row.names = FALSE)
          
          
        }
    )
)

