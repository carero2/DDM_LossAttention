########
######## This code is provided to perform all the DDM analysis from a Windows pc. If you want to run this from a mac
######## you won't be able to run the models calling the fast-dm.exe from this code, but you will be able to create
######## participant files, models files, read models results and plot models results.
########


library(dplyr)
library(rlang)


##### READ AND CLEAN DATA from csv data
# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

read_and_clean <- function(file,                                # csv file to read
                           remove_basedOnRT = TRUE,             # whether to remove trials with too quick or too slow responses
                           remove_outliers_rt = FALSE,          # Whether to remove outliers trials based on RT
                           column_rt = "duration",              # specify rt column
                           remove_outliers_performance = TRUE,  # whether to remove participants with an outlier performance
                           column_performance = "maximized_option",   # Specify performance column
                           column_participants = "subject_id",  # specify participants column
                           remove_participants = c(),           # specific participants to remove
                           var_block = "block",                 # specify block variable (where practice is equal to 0)
                           transform_log=FALSE) {               # whether to transform RT to log
  # Read
  df <- read.csv(file)
  
  # Remove practice block
  df <- df[df[[var_block]] != 0,]
  
  
  # Remove outliers in response time
  if (remove_outliers_rt){
    df <- remove_rt_outliers(df, column_outiers = column_rt)
  }
  
  # Remove outliers in performance
  if (remove_outliers_performance){
    df <- remove_performance_outliers(df, column_participants, column_performance)
  }
  
  # Remove based on RT, keep only responses between 250ms and 3500ms
  if (remove_basedOnRT){
    # Save sizes and remove trials
    original_size <<- nrow(df)
    removed_trials <<-
      nrow(df) - nrow(df[(df["duration"] > 250) &
                           (df["duration"] < 3500), ])
    
    df <- df[(df["duration"] > 250) & (df["duration"] < 3500), ]
    
    print(
      paste0(
        removed_trials,
        " trials have been removed based on RT. This is a %",
        round(removed_trials / original_size * 100, 3)
      )
    )
  } else{
    print("No trials have been removed based on RT.")
  }
  
  # Remove specific participants specified in remove_participants parameter
  if (length(remove_participants) != 0){
    to_keep <- !pull(df, column_participants) %in% remove_participants
    df <- df[to_keep, ]
    print(paste0("Removed ", length(remove_participants), " participants: ", paste0(remove_participants, collapse=",")))
  }
  
  # Convert response time from ms to seconds
  df$duration_secs <- round((df$duration/1000), 5)
  
  # reset index
  rownames(df) <- NULL
  
  # Log transform response time variable
  if (transform_log){
    df["RT_transformed"] <- log10(df["duration"])
  }
  
  
  return(df)
}

# Remove trials outliers based on 1.5 IQR in response time
remove_rt_outliers <- function(df, column_outliers="duration"){
  
  # Check and remove outliers (1.5 IQR)
  column <- as.numeric(unlist(df1[column_name]))
  if (length(boxplot.stats(column)$out) != 0) {
    out <- boxplot.stats(column)$out
    out_ind <- which(column %in% c(out))
    df <- df[!rownames(df) %in% out_ind, ]
    removed_trials <- removed_trials + length(out)
    rownames(df) <- NULL
    print(paste0(length(out), 
                 " outliers have been removed from dataset. Thus ",
                 removed_trials, " trials have been eliminated. This is a %",
                 round(removed_trials/original_size*100, 3)))
  } else{
    print("There are no outliers in dataset.")
  }
  
  return(df)
}

remove_performance_outliers <- function(df,
                                        column_participants,
                                        column_performance){
  aggregated <- as.data.frame(df %>%
                                 group_by((!!sym(column_participants))) %>%
                                 summarize(prop_correct = mean((!!sym(column_performance))))
                                 )
  
  boxplot(aggregated$prop_correct,
          ylab="Proportion",
          main = "Proportion of correct"
  )
  out <- boxplot.stats(aggregated$prop_correct)$out
  
  
  
  if (length(out)>0) {
    out_ind <- which(aggregated$prop_correct %in% c(out))
    participants_outlier <- aggregated[out_ind, column_participants]
    mtext(paste("Participant outliers: ", paste(participants_outlier, collapse = ", ")))
    
    print(paste0(length(out), 
                 " participants have been removed for being outliers in performance from dataset. ",
                 "Thus, our final sample is composed of ", 
                 length(unique(df[!(df[column_participants] %in% participants_outlier), column_participants])),
                 " participants. "))
  } else{
    print("There are no outliers in dataset.")
  }
  
  return(df[!(df[column_participants] %in% participants_outlier), ])
}



# Apply to Study 1
df1 <- read_and_clean(file = "Study1cleaned_df.csv",
                      remove_basedOnRT= TRUE,
                      remove_outliers_rt = FALSE,
                      column_participants = "subject_id",
                      remove_participants = c(),
                      var_block = "block",
                      transform_log=FALSE)


# Study 2
df2 <- read_and_clean(file = "Study2cleaned_df.csv",
                      remove_basedOnRT = TRUE, 
                      remove_outliers_rt = FALSE,
                      column_rt = "duration",
                      remove_outliers_performance = TRUE, 
                      column_performance = "maximized_option",
                      column_participants = "subject_id", 
                      remove_participants = c(), 
                      var_block = "block_order", 
                      transform_log=FALSE)



# Study 3
df3 <- read_and_clean(file = "Study3cleaned_df.csv",
                      column_rt = "duration",
                      remove_basedOnRT= TRUE,
                      remove_outliers_rt = FALSE,
                      remove_outliers_performance = FALSE, 
                      column_performance = "correct",
                      var_block = "block_order",
                      column_participants = "participant_id", 
                      transform_log=FALSE)

# For study 3 we want to remove outlier participants only based on the pure domains (where no risk preferences are involved)
# Thats why we remove the participants manually:

df3$pure_domain <- ifelse(substr(df3$domain, 0, 4) == "pure", 1, 0)

aggregated3 <- as.data.frame(df3 %>%
                              filter(pure_domain == 1)  %>%
                              group_by((!!sym("participant_id"))) %>%
                              summarize(prop_correct = round(mean((!!sym("correct"))), 2)
                              ))



boxplot(aggregated3$prop_correct,
        ylab="Proportion",
        main = "Proportion of correct in pure domains"
)
out <- boxplot.stats(aggregated3$prop_correct)$out
out_ind <- which(aggregated3$prop_correct %in% c(out))
participants_outlier <- aggregated3[out_ind, "participant_id"]
mtext(paste("Participant outliers: ", paste(participants_outlier, collapse = ", ")))


df3 <- df3[!(df3$participant_id %in% participants_outlier), ]
print(paste0(length(out), 
             " participants have been removed for being outliers in performance from dataset. ",
             "Thus, our final sample is composed of ", 
             length(unique(df3[!(df3$participant_id %in% participants_outlier), "participant_id"])),
             " participants. "))






##### CREATE PARTICIPANT FILES

# This function creates al the participants files used by fast-dm.exe to fit the models
create_participants_DDM <- function(df,
                                    participant_column="subject_id",
                                    columns_keep){
  
  for (participant in unique(df[, participant_column])) {
    print(participant)
    write.table(
      df[df[, participant_column] == participant, columns_keep],
      file = paste0(participant, ".txt"),
      sep = "\t",
      row.names = FALSE,
      col.names = FALSE
    )
  }
}

# Move to Study 1 where we will create the files
setwd("Study 1/")
create_participants_DDM(df1, participant_column="subject_id",
                        c("maximized_option",
                          "duration_secs",
                          "domain",
                          "diff_net_value"))



# Return to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Move to Study 2
setwd("Study 2/")
create_participants_DDM(df2,
                        participant_column="subject_id",
                        c("maximized_option",
                               "duration_secs",
                               "domain",
                               "diff_net_value"))


# Return to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Move to Study 3 pure domains
setwd("Study 3/pure_domains")
create_participants_DDM(df3[df3$domain %in% c("pure_mixed", "pure_gain", "pure_loss"),],
                        participant_column = "participant_id",
                        c("correct",
                          "duration_secs",
                          "domain"))



# Return to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Move to Study 3 mixed domains
setwd("Study 3/mixed_domains")
create_participants_DDM(df3[df3$domain %in% c("mixed_loss", "mixed_gain"),],
                        participant_column = "participant_id",
                        c("correct",
                          "duration_secs",
                          "domain"))



##### CREATE THE MODELS (you should have created a models_StudyX.csv to specify which models you want to create)
# This function will create all the models.ctl files that are used by fast-dm.exe to specify all the parameters of the model to be trained
create_models <- function(models_csv="models_Study1.csv",
                          format="RESPONSE TIME condition difficulty"){
  
  df <- read.csv(models_csv, row.names=NULL, sep=";")
  df <- subset(df, select = -c(model, p))
  
  for (row in 1:nrow(df)){
    sink(paste0("./model", row, ".ctl"))
    cat(paste0("# Model ", row), "\n")
    cat(paste0("method ml", "\n", "precision 3", "\n"))
    for (column in rev(colnames(df))){
      if (column %in% c("v", "t0", "a")){
        if (df[row, column] == "C"){
          cat(paste0("depends ", column, " condition"), "\n")
        }
        if (df[row, column] == "C, D"){
          cat(paste0("depends ", column, " condition difficulty"), "\n")
        }
        if (df[row, column] == "D"){
          cat(paste0("depends ", column, " difficulty"), "\n")
        }
        
      } else{
        if (df[row, column] != "-"){
          cat(paste0("set ", column, " ",  df[row, column]), "\n")
        }
      }
    }
    cat(paste0("format ", format, "\n",
               "load *.txt", "\n",
               "log model", row, ".log"))
    sink()
  }
}

# Return to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Move to Study 1
setwd("Study 1/")
# Create models for Study 1
create_models(models_csv="models_Study1.csv")


# Return to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Move to Study 2
setwd("Study 2/")
# Create models for Study 2
create_models(models_csv="models_Study2.csv")

# Return to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Move to Study 3
setwd("Study 3/pure_domains")
# Create models for Study 3 pure domains
create_models(models_csv="models_Study3.csv", format= "RESPONSE TIME condition")

# Move to Study 3 mixed domains
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("Study 3/mixed_domains")
create_models(models_csv="models_Study3.csv", format= "RESPONSE TIME condition")



##### RUN DDM



setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("Study 1")


n_models <- length(list.files(path = "./", pattern = "*.ctl"))

# If you only want to run some models, you have to change the 1:n_models to a vector of the desired models to analyze, ex. c(9, 10, 11)
for (model in 1:n_models) {
  system(paste0("fast-dm model",
                model, ".ctl"))
}


# Study 2
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("Study 2")
n_models <- length(list.files(path = "./", pattern = "*.ctl"))
for (model in 1:n_models) {
  system(paste0("fast-dm model",
                model, ".ctl"))
}

# Study 3 pure domains
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("Study 3/pure_domains")
n_models <- length(list.files(path = "./", pattern = "*.ctl"))
for (model in 1:n_models) {
  system(paste0("fast-dm model",
                model, ".ctl"))
}

# Study 3 mixed domains
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("Study 3/mixed_domains")
n_models <- length(list.files(path = "./", pattern = "*.ctl"))
for (model in 1:n_models) {
  system(paste0("fast-dm model",
                model, ".ctl"))
}





## READ ANALYZED MODELS

library("readr")
library("stringr")
library("dplyr")
library("R.utils")




read_analyzed_models <- function(study_folder="Study 2"){
  
  # Return to source file location and go to specified path
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  setwd(study_folder)
  
  
  models <- list.files(path = "./", pattern = "*.log")
  n_participants <- length(list.files(path = "./", pattern = "*.txt"))
  
  df_models <- NULL
  
  models_less_participants <- c()
  
  for (model in models) {
    df_model <- read_log(model, col_names = TRUE, show_col_types=FALSE)
    names(df_model)[1] <- "participant"
    
    # Initiate the new column
    df_model$n_trials <- 0
    
    # Replace the 0 for the number of trials per participant
    for (participant in df_model$participant) {
      n_trials_participant <- countLines(paste0(participant, ".txt"))
      df_model[df_model$participant == participant, "n_trials"] <- n_trials_participant
    }
  
    # If the ML method is chosen the presented fit index is -LL (Voss, Voss and Lerche, 2015)
    df_model$BIC <- 2*df_model$fit + (ncol(df_model) - 6) * log(df_model$n_trials)
    
    model_str <- str_extract(model, '.*(?=\\.log)')
    df_model$model_variant <- model_str
    
    
    if (nrow(df_model) < n_participants) {
      models_less_participants <- append(models_less_participants, model_str)
    }
    
    df_models <- bind_rows(df_models, df_model)
  }
  if (length(models_less_participants == 0)){
    cat("Less participants than expected in models ", models_less_participants, "\n", sep="\t")
    print(paste0(n_participants, " participants expected. ", nrow(df_model), " participants included."))
  }
  
  return(df_models)
}

get_best_models <- function(df, models_csv){
  
  n_models <-  length(unique(df$model_variant))
  
  print(paste0("Table has results from ",
             length(unique(df$participant)), " participants and ",
             n_models, " models."))

  
  
  for (participant in unique(df$participant)) {
    best_bic <- min(df[df$participant == participant, "BIC"])
    df[df$participant == participant, "model"] <- df[(df$participant == participant) & (df$BIC == best_bic), "model_variant"]
  }
  
  aggregated_best_models <- df %>%
    group_by(model) %>%
    summarize(n_participants = n()/n_models)
  
  aggregated_models <- df %>% select(-c(model)) %>%
    rename(model = model_variant) %>%
    group_by(model) %>%
    summarize(n = n(),
              mean_fit = mean(fit),
              mean_BIC = mean(BIC),
              median_BIC = median(BIC))

  
  aggregated_models <- merge(aggregated_models, aggregated_best_models)
  

  models <- read.csv2(models_csv)
  models$model <- paste0("model", models$model)
  final_aggregated <- merge(aggregated_models, models)
  
  if (length(unique(df$participant)) ==  sum(aggregated_models$n_participants)){
    model <- aggregated_models[aggregated_models$n_participants == max(aggregated_models$n_participants), "model"]
    print(paste0("It seems that the best model is ", model,
                 " because ", max(aggregated_models$n_participants),
                 " out of ", sum(aggregated_models$n_participants), " participants had a lower BIC with that model."))
  }
  # Return to source file location and go to specified path
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  return(final_aggregated)
}

get_mean_parameters <- function(results, best_models){
  summary_models <- results[results$model_variant %in% best_models$model,] %>%
    group_by(model_variant) %>%
    select(-method, -participant, -n_trials) %>%
    summarise_if(is.numeric, mean, na.rm = TRUE)
  
  return(summary_models)
}

results_Study2 <- read_analyzed_models(study_folder = "Study 2")
best_models_Study2 <- get_best_models(results_Study2, models_csv="models_Study2.csv")
final_parameters_Study2 <- get_mean_parameters(results_Study2, best_models_Study2)

# Lets save
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('Study 2')
write.csv(best_models_Study2, "results_moldes_Study2.csv", row.names=FALSE)
write.csv(final_parameters_Study2, "parameters_models_Study2.csv", row.names=FALSE)

results_Study3_pure <- read_analyzed_models(study_folder="Study 3/pure_domains")
best_models_Study3_pure <- get_best_models(results_Study3_pure, models_csv="models_Study3.csv")
final_parameters_Study3_pure <- get_mean_parameters(results_Study3_pure, best_models_Study3_pure)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('Study 3/pure_domains')
write.csv(best_models_Study3_pure, "results_moldes_Study3_pure.csv", row.names=FALSE)
write.csv(final_parameters_Study3_pure, "parameters_models_Study3_pure.csv", row.names=FALSE)

results_Study3_mixed <- read_analyzed_models(study_folder="Study 3/mixed_domains")
best_models_Study3_mixed <- get_best_models(results_Study3_mixed, models_csv="models_Study3.csv")
final_parameters_Study3_mixed <- get_mean_parameters(results_Study3_mixed, best_models_Study3_mixed)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('Study 3/mixed_domains')
write.csv(results_Study3_mixed, "results_moldes_Study3_mixed.csv", row.names=FALSE)
write.csv(final_parameters_Study3_mixed, "parameters_models_Study3_mixed.csv", row.names=FALSE)



# Lets plot some results

library("ggplot2")
library("tidyr")

plot_results_model <- function(df, model, parameters){
  # creating the modified dataframe
  data_mod <- df[df$model_variant == model, parameters] %>% pivot_longer(everything())
  # creating a plot 
  ggplot(data_mod) + 
    geom_boxplot(aes(x=name, y=value, color=name)) +
    labs(x="Parameter value", title=paste0("Boxplot for parameters from ", model))
  
}

# Plots for Study 2

plot_results_model(results_Study2, model="model11", parameters=c('v_2', 'v_4', 'v_6'))

plot_results_model(results_Study2, model="model15", parameters=c('t0_gain', 't0_loss', 't0_mixed'))

# Plots for Study 3 pure domains

plot_results_model(results_Study3_pure, model="model5", parameters=c('v_pure_mixed', 'v_pure_gain', 'v_pure_loss'))

plot_results_model(results_Study3_pure, model="model5", parameters=c('t0_pure_mixed', 't0_pure_gain', 't0_pure_loss'))


# Plots for Study 3 mixed domains

plot_results_model(results_Study3_mixed, model="model6", parameters=c('t0_mixed_gain', 't0_mixed_loss'))

plot_results_model(results_Study3_mixed, model="model6", parameters=c('v_mixed_gain', 'v_mixed_loss'))


