library(vroom)
library(data.table)

patent_tbl <- vroom(file="content/01_journal/00_data/03_wrangling/Patent_data_reduced/patent.tsv", delim="\t", na=c("", "NA", "NULL"), show_col_types = FALSE)
assignee_tbl <- vroom(file="content/01_journal/00_data/03_wrangling/Patent_data_reduced/assignee.tsv", delim="\t", na=c("", "NA", "NULL"), show_col_types = FALSE)
patent_assignee_tbl <- vroom(file="content/01_journal/00_data/03_wrangling/Patent_data_reduced/patent_assignee.tsv", delim="\t", na=c("", "NA", "NULL"), show_col_types = FALSE)
uspc_tbl <- vroom(file="content/01_journal/00_data/03_wrangling/Patent_data_reduced/uspc.tsv", delim="\t", na=c("", "NA", "NULL"), show_col_types = FALSE)

patent_dt <- as.data.table(patent_tbl)
assignee_dt <- as.data.table(assignee_tbl)
patent_assignee_dt <- as.data.table(patent_assignee_tbl)
uspc_dt <- as.data.table(uspc_tbl)

patent_dominance_dt <- patent_assignee_dt[assignee_dt, on = .(assignee_id = id)][!is.na(organization)]
patent_dominance_dt[, .(count = .N), by = organization][order(-count)][1:10]

patent_recent_activity_dt <- patent_dominance_dt[patent_dt, on = .(patent_id = id)][!is.na(organization)]
patent_recent_activity_dt[format(date, "%Y-%m") == "2014-08", .(count = .N), by = organization][order(-count)][1:10]

uspc_dt[, patent_id := as.character(patent_id)]
top_organizations <- patent_dominance_dt[, .(count = .N), by = organization][order(-count)][1:10]$organization

patent_innovation_dt <- patent_dominance_dt[uspc_dt, on = .(patent_id)][!is.na(organization)]

result_list <- list()
for (org in top_organizations) {
  # Filter data for the current organization
  org_data <- patent_innovation_dt[organization == org]
  
  # Calculate the top 5 mainclass_id for the current organization
  top_mainclass <- org_data[, .(count = .N), by = mainclass_id][order(-count)][1:5]$mainclass_id
  
  # Store the result in the list
  result_list[[org]] <- top_mainclass
}

# Create a matrix to hold the top 5 mainclass_id values for each organization
result_matrix <- matrix(NA_character_, nrow = 10, ncol = 5, dimnames = list(top_organizations, paste0("Top_Mainclass_", 1:5)))

# Fill in the matrix with the top 5 mainclass_id values for each organization
for (i in 1:length(result_list)) {
  result_matrix[i, 1:length(result_list[[i]])] <- result_list[[i]]
}

# Convert the matrix to a data.table
result_dt <- data.table(organization = rownames(result_matrix), result_matrix)
