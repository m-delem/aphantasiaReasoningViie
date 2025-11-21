predictor <- gsub(" ", "", "group_3")

if (grepl("*", predictor, fixed = TRUE)) predictor <- strsplit(predictor, "*", fixed = TRUE)[[1]]

variables <- posterior::variables(m)

incl_classes <- c(
  "b", "bs", "bcs", "bsp", "bmo", "bme", "bmi", "bm",
  brms:::valid_dpars(m), "delta", "lncor", "rescor", "ar", "ma", "sderr",
  "cosy", "cortime", "lagsar", "errorsar", "car", "sdcar", "rhocar",
  "sd", "cor", "df", "sds", "sdgp", "lscale", "simo"
)
incl_regex <- paste0("^", brms:::regex_or(incl_classes), "(_|$|\\[)")

variables <- variables[grepl(incl_regex, variables)]

fit_levels <- grep(paste0("^Intercept$|", paste(predictor, collapse = "|")), gsub("^b_", "", variables)
                   , value = TRUE)

original_levels <-
  unlist(sapply(predictor, function(w) gsub(" |&", "", paste0(w, unique(m$data[, w])))))

base_levels <- setdiff(original_levels, fit_levels)
base_level <- paste(base_levels, collapse = ":")

pred_levels <- unlist(sapply(predictor, function(w) as.character(unique(m$data[, w]))))

pred_levels_list <- lapply(predictor, function(w) as.character(unique(m$data[, w])))
names(pred_levels_list) <- predictor

pred_levels <- lapply(seq_len(length(pred_levels_list)), function(x) paste0(names(pred_levels_list)[x], pred_levels_list[[x]]))

pred_levels <- apply(expand.grid(pred_levels), 1, paste, collapse = ":")

levels_df <- as.data.frame(t(utils::combn(pred_levels, 2)))
levels_df$V1.nospace <- gsub(" |&", "", levels_df$V1)
levels_df$V2.nospace <- gsub(" |&", "", levels_df$V2)

contrsts <- paste(apply(levels_df[, c("V1.nospace", "V2.nospace")], 1, paste, collapse = " - "), "= 0")

levels_df$sign <- 1
levels_df$sign[grep(base_level, levels_df$V1)] <- -1

names(contrsts) <- paste0(levels_df$V1, " - ", levels_df$V2)

contrsts <- gsub(paste0(base_level, " - "), "", contrsts)
contrsts <- gsub(paste0(" - ", base_level), "", contrsts)

if (length(predictor) > 1) {
  contrsts <- gsub(paste(paste0(base_levels, ":"), collapse = "|"), "", contrsts)
  contrsts <- gsub(paste(paste0(":", base_levels), collapse = "|"), "", contrsts)
  contrsts <- gsub(paste(paste0(base_levels, " - "), collapse = "|"), "", contrsts)
  contrsts <- gsub(paste(paste0(" - ", base_levels), collapse = "|"), "", contrsts)
  contrsts <- gsub(paste(paste0(base_levels, " "), collapse = "|"), "", contrsts)
  contrsts <- gsub(paste(paste0(" ", base_levels), collapse = "|"), "", contrsts)

  contrsts <-  gsub(":=", " =", contrsts)
  contrsts <-  gsub(": ", " ", contrsts)
}

names(contrsts) <- gsub(paste(predictor, collapse = "|"), "", names(contrsts))
