library(dplyr)
library(rlang)


##### READ AND CLEAN DATA
# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

read_and_clean <- function(file = "Study1cleaned_df.csv",
                           remove_basedOnRT = TRUE,
                           remove_outliers_rt = FALSE,
                           column_rt = "duration",
                           column_participants = "subject_id",
                           remove_participants = c(),
                           var_block = "block",
                           transform_log=FALSE) {
  # Read
  df <- read.csv(file)
  
  # Remove practice block
  df <- df[df[var_block] != 0,]
  
  if (remove_outliers_rt){
    df <- remove_outliers(df, column_outiers = column_rt)
  }
  
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
        removed_trials / original_size * 100
      )
    )
  } else{
    print("No trials have been removed based on RT.")
  }
  
  if (length(remove_participants) != 0){
    to_keep <- !pull(df, column_participants) %in% remove_participants
    df <- df[to_keep, ]
    print(paste0("Removed ", length(remove_participants), " participants: ", paste0(remove_participants, collapse=",")))
  }

  df$duration_secs <- round((df$duration/1000), 5)
  
  rownames(df) <- NULL
  if (transform_log){
    df["RT_transformed"] <- log10(df["duration"])
  }
  
  
  return(df)
}

remove_outliers <- function(df, column_outiers="duration"){
  
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
                 removed_trials/original_size*100))
  } else{
    print("There are no outliers in dataset.")
  }
  
  return(df)
}

# Read, remove trials based on RT and log transform RT
df1 <- read_and_clean(file = "Study1cleaned_df.csv",
                      remove_basedOnRT= TRUE,
                      remove_outliers_rt = FALSE,
                      column_participants = "subject_id",
                      remove_participants = c(),
                      var_block = "block",
                      transform_log=FALSE)


# Repeat for Study 2
df2 <- read_and_clean(file = "Study2cleaned_df.csv",
                      remove_basedOnRT= TRUE,
                      remove_outliers = FALSE,
                      column_participants = "subject_id",
                      remove_participants = c(),
                      var_block = "block_order",
                      transform_log=FALSE)


aggregated2 <- as.data.frame(df2 %>%
                              group_by(subject_id) %>%
                              summarize(prop_correct = round(mean(maximized_option), 2)
                              ))

boxplot(aggregated2$prop_correct,
        ylab="Proportion",
        main = "Proportion of correct in pure domains"
)
out <- boxplot.stats(aggregated2$prop_correct)$out
out_ind <- which(aggregated2$prop_correct %in% c(out))
participants_outlier <- aggregated2[out_ind, "subject_id"]
mtext(paste("Participant outliers: ", paste(participants_outlier, collapse = ", ")))


df2 <- df2[!(df2$subject_id %in% participants_outlier), ]
length(unique(df2$subject_id))


# Study 3
df3 <- read_and_clean(file = "Study3cleaned_df.csv",
                      remove_basedOnRT= TRUE,
                      var_block = "block_order",
                      transform_log=FALSE)


df3$pure_domain <- ifelse(substr(df3$domain, 0, 4) == "pure", 1, 0)

aggregated3 <- as.data.frame(df3 %>%
                              filter(pure_domain == 1)  %>%
                              group_by(participant_id) %>%
                              summarize(prop_correct = round(mean(correct), 2)
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
length(unique(df3$participant_id))






##### CREATE PARTICIPANT FILES

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

getwd()
setwd("DDM/Study 1/")
create_participants_DDM(df1, participant_column="subject_id",
                        c("maximized_option",
                          "duration_secs",
                          "domain",
                          "diff_net_value"))



# Return to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))




setwd("DDM/Study 2/")
create_participants_DDM(df2,
                        participant_column="subject_id",
                        c("maximized_option",
                               "duration_secs",
                               "domain",
                               "diff_net_value"))


# Return to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


setwd("DDM/Study 3/pure_domains")
create_participants_DDM(df3[df3$domain %in% c("pure_mixed", "pure_gain", "pure_loss"),],
                        participant_column = "participant_id",
                        c("correct",
                          "duration_secs",
                          "domain"))



# Return to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


setwd("DDM/Study 3/mixed_domains")
create_participants_DDM(df3[df3$domain %in% c("mixed_loss", "mixed_gain"),],
                        participant_column = "participant_id",
                        c("correct",
                          "duration_secs",
                          "domain"))



##### CREATE THE MODELS (you should have created a models_StudyX.csv to specify which models you want to create)

setwd("DDM")
create_models <- function(study="Study 1",
                          format="RESPONSE TIME condition difficulty"){
  
  df <- read.csv(paste0("models_", gsub(" ", "", study), ".csv"), row.names=NULL, sep=";")
  df <- subset(df, select = -c(model, p))
  
  for (row in 1:nrow(df)){
    sink(paste0("./", study, "/model", row, ".ctl"))
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

# Create models for Study 1
create_models(study="Study 1")

# Create models for Study 2
create_models(study="Study 2")

# Create models for Study 3 (for mixed and pure domains are the same models)
create_models(study="Study 3", format= "RESPONSE TIME condition")



##### RUN DDM

n_models <- 9

setwd("Study 1")
for (model in 1:n_models) {
  system(paste0("fast-dm model",
                model, ".ctl"))
}


# Return to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("DDM/Study 2")
for (model in 1:n_models) {
  system(paste0("fast-dm model",
                model, ".ctl"))
}

# Return to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("DDM/Study 3/mixed_domains")
for (model in 1:n_models) {
  system(paste0("fast-dm model",
                model, ".ctl"))
}

# Return to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("DDM/Study 3/pure_domains")
for (model in 1:n_models) {
  system(paste0("fast-dm model",
                model, ".ctl"))
}





## READ ANALYZED MODELS

library("readr")
library("stringr")
library("dplyr")




read_analyzed_models <- function(study_path="Study 1"){
  # Return to source file location
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  setwd(paste0("DDM/DDM_LossAttention/", study_path))
  
  models <- list.files(path = "./", pattern = "*.log")
  n_participants <- length(list.files(path = "./", pattern = "*.txt"))
  
  df_models <- NULL
  
  models_less_participants <- c()
  
  for (model in models) {
    df_model <- read_log(model, col_names = TRUE, show_col_types=FALSE)
    names(df_model)[1] <- "participant"
    
    df_model$BIC <- df_model$fit * 2 + (ncol(df_model) - 5) * log(nrow(df_model))
    
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

get_best_models <- function(df, study = "Study 2"){
  
  print(paste0("Table has results from ",
             length(unique(df$participant)), " participants and ",
             length(unique(df$model_variant)), " models."))

  
  
  for (participant in unique(df$participant)) {
    best_bic <- min(df[df$participant == participant, "BIC"])
    df[df$participant == participant, "model"] <- df[(df$participant == participant) & (df$BIC == best_bic), "model_variant"]
  }
  
  aggregated_best_models <- df %>%
    group_by(model) %>%
    summarize(n_participants = n()/9)
  
  aggregated_models <- df %>% select(-c(model)) %>%
    rename(model = model_variant) %>%
    group_by(model) %>%
    summarize(n = n(),
              mean_fit = mean(fit),
              mean_BIC = mean(BIC),
              median_BIC = median(BIC))

  
  aggregated_models <- merge(aggregated_models, aggregated_best_models)
  
  
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  setwd("DDM//DDM_LossAttention/")
  models <- read.csv2(paste0("models_", gsub(" ", "", study), ".csv"))
  models$model <- paste0("model", models$model)
  final_aggregated <- merge(aggregated_models, models)
  
  if (length(unique(df$participant)) ==  sum(aggregated_models$n_participants)){
    model <- aggregated_models[aggregated_models$n_participants == max(aggregated_models$n_participants), "model"]
    print(paste0("It seems that the best model is ", model,
                 " because ", max(aggregated_models$n_participants),
                 " out of ", sum(aggregated_models$n_participants), " participants had a lower BIC with that model."))
  }
  return(final_aggregated)
  }

results_Study2 <- read_analyzed_models(study_path="Study 2")

best_models_Study2 <- get_best_models(results_Study2, study = "Study 2")

for (model in unique(results_Study3_pure$model_variant)) {
  print(model)
  print(mean(as.numeric(unlist(results_Study3_pure[results_Study3_pure$model_variant == model, "BIC"]))))
  print(sd(as.numeric(unlist(results_Study3_pure[results_Study3_pure$model_variant == model, "BIC"]))))
  print(median(as.numeric(unlist(results_Study3_pure[results_Study3_pure$model_variant == model, "BIC"]))))
}


results_Study3_pure <- read_analyzed_models(study_path="Study 3/pure_domains")

best_models_Study3_pure <- get_best_models(results_Study3_pure, study = "Study 3")

results_Study3_mixed <- read_analyzed_models(study_path="Study 3/mixed_domains")

best_models_Study3_mixed <- get_best_models(results_Study3_mixed, study = "Study 3")


library(ggplot2) 

plot_results_model <- function(df, model, parameters){
  # creating the modified dataframe
  data_mod <- df[df$model_variant == model, parameters] %>% pivot_longer(everything())
  # creating a plot 
  ggplot(data_mod) + 
    geom_boxplot(aes(x=name, y=value, color=name)) +
    labs(x="Parameter value", title=paste0("Boxplot for parameters from ", model))
  
}
plot_results_model(results_Study3_pure, "model5", c('v_pure_mixed', 'v_pure_gain', 'v_pure_loss'))

plot_results_model(results_Study3_mixed, "model6", c('t0_mixed_gain', 't0_mixed_loss'))



plot_results_allModels <- function(df, parameters){
  # creating the modified dataframe
  data_mod <- df[, parameters] %>% pivot_longer(everything())
  # creating a plot 
  ggplot(data_mod) + 
    geom_boxplot(aes(x=name, y=value, color=name)) +
    labs(x="Parameter value", title=paste0("Boxplot for parameters from all models"))
  
}

plot_results_allModels(results_Study3_pure,  c('v_pure_mixed', 'v_pure_gain', 'v_pure_loss'))

plot_results_allModels(results_Study3_mixed, c('v_mixed_gain', 'v_mixed_loss'))

