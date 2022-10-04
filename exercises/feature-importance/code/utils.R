library(R6)
library(Matrix)
library(pracma)
library(MASS)
library(matrixcalc)

GaussianConditionalEstimator <-
  R6Class(
    "GaussianConditionalEstimator",
    public = list(
      inp_ind = NA,
      RegrCoeff = NA,
      Sigma = NA,
      mu_part = NA,
      fit = function(train_inputs, train_context) {
        X_train = cbind(train_inputs, train_context)
        
        mean = colMeans(X_train)
        
        cov = cov(X_train)
        
        n_in = ncol(train_inputs)
        n_co = ncol(train_context)
        
        # In R, the index should start from 1.
        inp_ind_from_0 = seq(0, n_in, 1)
        inp_ind <- tail(inp_ind_from_0, -1)
        
        cont_ind_from_0 = seq(n_in, n_in + n_co, 1)
        cont_ind <- tail(cont_ind_from_0, -1)
        
        return(self$fit_mean_cov(mean, cov, inp_ind, cont_ind))
        
      },
      
      fit_mean_cov = function(joint_mean, joint_cov, inp_ind, cont_ind) {
        self$inp_ind = inp_ind
        
        if (length(cont_ind) == 0) {
          self$RegrCoeff = matrix(0, length(inp_ind), 0)
        }
        else{
          if (length(cont_ind) == 1) {
            Sigma_GG_inv = matrix(1 / joint_cov[cont_ind, cont_ind])
          }
          else{
            cov_context = joint_cov[cont_ind, cont_ind]
            Sigma_GG_inv = ginv(cov_context)
          }
          cov_ip_con = joint_cov[inp_ind, cont_ind]
          tmp = cov_ip_con %*% Sigma_GG_inv
          self$RegrCoeff = tmp
          
        }
        cov_inp = joint_cov[inp_ind, inp_ind]
        
        cov_cont_inp = joint_cov[cont_ind, inp_ind]
        
        self$Sigma = cov_inp - (self$RegrCoeff %*% cov_cont_inp)
        
        mean_inp = joint_mean[inp_ind]
        mean_cont = joint_mean[cont_ind]
        
        self$mu_part = mean_inp - self$RegrCoeff %*% mean_cont
        
      },
      
      sample = function(context, num_samples, col_Name) {
        mu_part2 = self$RegrCoeff %*% t(context)
        mu = c()
        
        for (i in 1:nrow(mu_part2)) {
          mu <-
            as.matrix(rbind(mu, array(mu_part2[i, ] + self$mu_part[i, 1])))
          
        }
        muT = t(mu)
        sample_df = c()
        for (i in 1:nrow(muT)) {
          mvrnorm_result <- mvrnorm(num_samples, muT[i, ], self$Sigma)
          sample_df <- rbind(sample_df, mvrnorm_result)
        }
        sample_df <-
          as.data.frame(sample_df)
        
        colnames(sample_df) <- col_Name
        return(sample_df)
        
      }
    )
  )