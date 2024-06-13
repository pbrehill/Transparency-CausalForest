library(tidyverse)
library(haven)
library(grf)
library(magrittr)

# Set seed
set.seed(11)

# Define key function
not_all_na <- function(x) {
  length(x[!is.na(x)]) != 0
}

file_u <- file.path('Combined u210u.sav')
file_t <- file.path('Combined t210u.sav')
file_s <- file.path('Combined s210u.sav')

# Load HILDA data
data_s <- read_sav(file_u)


# Get multi-year income
data_s$income <- data_s %>%
  select(xwaveid, utifefp) %>%
  left_join(read_sav(file_s,
                     col_select = which((read_sav(file_s, n_max = 1) %>% colnames() %in% c("stifefp", "xwaveid")))
  ), by = "xwaveid") %>%
  left_join(read_sav(file_t,
                     col_select = which((read_sav(file_t, n_max = 1) %>% colnames() %in% c("ttifefp", "xwaveid")))
  ), by = "xwaveid") %>%
  select(-xwaveid) %>%
  mutate_all(as.numeric) %>%
  rowMeans()


# Get labels to use as names later on
labels <- data_s %>% map_df(~attributes(.)$label) %>% gather("name", "label")

# Get the names of valid pre-treatment variables
id_vars <- read_rds('id_names.rds')


# Create vars from https://core.ac.uk/download/pdf/6278539.pdf
# Ed years
data_s$highest_ed <- data_s %>%
  transmute(school_ed = tidyr::extract_numeric(uedhists %>% as_factor %>% as.character()),
            higher_ed = car::recode(uedhigh1, "-10:-1 = NA;
                                 1 = 17;
                                 2 = 16;
                                 3 = 15;
                                 4 = 12;
                                 else = 0
                                 ")
  ) %>%
  rowwise() %>% 
  transmute(max = max(school_ed, higher_ed)) %>%
  pull()



# Income
data_s$income[data_s$income < 0] <- 0

# Instrument
data_s$dob <- data_s %>% select(uhgdob) %>%
  pull() %>% 
  as.character() %>% 
  as.Date.character("%d/%m/%Y")


# Pull in Queensland data
data_s <- data_s %>% filter(!is.na(highest_ed) & !is.na(income))
# data_s <- data_s[sample(nrow(data_s), 1000), ]

data_s %<>%
  mutate(bin_treat = car::recode(highest_ed, "12 = 0; 15 = 1; else = NA")) %>%
  filter(!is.na(bin_treat))

X <- data_s %>% select(
  -uedhists, 
  -uedhigh1, 
  -utifefp, 
  -uhgdob,
  -income,
  -highest_ed,
  -bin_treat
)

X_orth <- data_s[id_vars] %>%
  mutate(uhgdob = uhgdob %>% as.Date(format = "%d/%m/%Y")) %>%
  mutate_all(as.numeric) %>% missMethods::impute_median()

X_orth <- X_orth[,X_orth %>% map_lgl(not_all_na)]





Z <- data_s %>% select(dob)
Y <- data_s %>% select(income)
W <- data_s %>% select(bin_treat)
weights <- data_s$uhhwte

Y[is.infinite(Y %>% pull),] <- NA
W[is.infinite(W %>% pull),] <- NA

Y %<>% as.data.frame() %>% missMethods::impute_median() %>% pull()
W %<>% as.data.frame() %>% missMethods::impute_median() %>% pull()


X  %<>%
  map_df(function (x) {
    tryCatch(
      {as.numeric(x)},
      error=function(cond) {rep(NA, length(x))}
    )
  }) %>%
  select_if(function(x) any(!is.na(x))) %>%
  missMethods::impute_median()



labels_t <- data_s %>% 
  map_df(~attributes(.)$label) %>% 
  gather("name", "label") %>%
  write_csv('labels.csv')

# var_imp_t <- variable_importance(trimmed_cf) %>% bind_cols(labels_t)



# names(X) <- labels_t$label[labels_t$name %in% names(X)]




# write_csv(X, 'X.csv')
# # write_csv(Y, 'Y.csv')
# write_csv(W, 'W.csv')



# X_id <- data_s %>% select(id_filter %>% pull) %>% mutate_all(as.numeric)%>% select(where(not_all_na)) %>% missMethods::impute_median()

orth_labels <- data.frame(
  names = labels_t$name[labels_t$name %in% names(X_orth)],
  labels = labels_t$label[labels_t$name %in% names(X_orth)]
)

X_orth %>%
  rename_with(~ orth_labels$labels[match(.x, orth_labels$names)]) %>%
  write_csv('X_id.csv')


not_all_na <- function(x) any(!is.na(x))
# Identifying models

list(
  X = X_orth, Y=Y, W=W, Xid = X_orth, 
  weights = as.numeric(weights),
  orth_labels = orth_labels
) %>%
  write_rds('data.rds')


# Files for Python
write_csv(X, 'X.csv')
write_csv(data.frame(income = Y), 'Y.csv')
write_csv(data.frame(treat = W), 'W.csv')