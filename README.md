library(rentrez)
library(dplyr)

# Read gene list from CSV file
gene_list <- read.csv("name_of_file")

# Read columns of genes from the CSV file
all_genes <- gene_list$name_of_gene_column

search_pubmed <- function(gene) {
  tryCatch({
    # Construct queries for cancer and cilia with date range from 2000 to present
    query_cancer <- paste0(gene, "[Title/Abstract] AND (Cancer[Title/Abstract] OR Neoplasms[MeSH]) AND ((2000[Date - Publication] : 3000[Date - Publication]))")
    query_cilia <- paste0(gene, "[Title/Abstract] AND (Cilia[Title/Abstract] OR Cilia[MeSH]) AND ((2000[Date - Publication] : 3000[Date - Publication]))")
    query_all <- paste0(gene, "[Title/Abstract] AND (Cilia[Title/Abstract] OR Cilia[MeSH]) AND (Cancer[Title/Abstract] OR Neoplasms[MeSH]) AND ((2000[Date - Publication] : 3000[Date - Publication]))")
    # Function to perform search and to summarize results
    perform_search <- function(query) {
      result <- entrez_search(db = "pubmed", term = query, retmax = 0)
      list(has_articles = result$count > 0, num_papers = result$count)
    }
    # Perform searches for cancer, cilia, and both
    cancer_results <- perform_search(query_cancer)
    cilia_results <- perform_search(query_cilia) 
    all_results <- perform_search(query_all)
    # Return a data frame without PMIDs, only article existence and counts
    data.frame(gene = gene, 
               has_cancer = cancer_results$has_articles, 
               num_cancer = cancer_results$num_papers, 
               has_cilia = cilia_results$has_articles, 
               num_cilia = cilia_results$num_papers, 
               has_all = all_results$has_articles, 
               num_all = all_results$num_papers)
  }, error = function(e) {
    # Return NA values in case of an error
    data.frame(gene = gene, 
               has_cancer = NA, 
               num_cancer = NA, 
               has_cilia = NA, 
               num_cilia = NA, 
               has_all = NA, 
               num_all = NA)
  })
}
# Use lapply for processing
combined_results <- do.call(rbind, lapply(all_genes, search_pubmed))

#Save the dataframe as a CSV File
write.csv(combined_results, "~/Downloads/combined_results.csv", row.names = FALSE)
![image](https://github.com/Sumhithaasriram/R-code-metanalysistoconnectsiloedfields/assets/166162867/61650d17-bf88-40b9-8c2b-25cf53914171)
