# Functions to use in pharmuse_constructor.R script

################################################################################
# remove_drugs() function:
#   * Description: Takes in a large list of pharmaceuticals and a smaller list
#                from within the larger list to remove. Smaller list is removed
#                from the the larger list.
#   * Input: 
#       * full_drug_list: class = list, larger list that needs truncating
#       * removal_drug_list: class = list, shorter list to be truncated from
#                            larger list
#
#   * Output:
#       * full_drug_list: class = list, truncated larger list
################################################################################
remove_drugs = function(full_drug_list, removal_drug_list){
  for (i in 1:length(removal_drug_list)){
    full_drug_list = full_drug_list [ !full_drug_list == removal_drug_list[i]]
  }
  return(full_drug_list)
}

################################################################################
# fix_drug_coding() function:
#   * Description: Corrects entries that have been miscoded in the MEPS data.
#                  Note: some entries required special treatment outside this
#                        function.
#   * Input: 
#       * df: class = dataframe, dataframe with incorrect entries
#       * column: class = vector, column of dataframe with miscoded entries
#       * colname: class = character, name of column with miscoded entries
#       * incorrect: class = many possible, incorrect entry contents
#       * correct: class = many possible, correct entry contents
#
#   * Output:
#       * df: class = dataframe, dataframe with correct entries replacing 
#             incorrect ones
################################################################################
fix_drug_coding = function(df, column, colname, incorrect, correct){
  df = df %>%
    mutate(column = case_when(column==incorrect ~ correct,
                          .default=column)) %>%
    relocate(column, .before=colname) %>%
    select(-c(colname))
  names(df)[names(df)=="column"] = colname
  return(df)
}

################################################################################
# remove_entries() function:
#   * Description: Removes entries from dataframe when they are incorrect.
#   * Input: 
#       * df: class = dataframe, dataframe with incorrect entries
#       * column: class = vector, column of dataframe with incorrect entries
#       * incorrect: class = many possible, incorrect entry contents
#
#   * Output:
#       * df: class = dataframe, dataframe with incorrect entries removed
################################################################################
remove_entries = function(df, column, incorrect){
  df = subset(df, column != incorrect)
  return(df)
}

################################################################################
# replace_na_with_missing_drug() function:
#   * Description: Replaces NA values with names of drugs with missing data
#                  points
#   * Input: 
#       * df: class = dataframe, dataframe with NA values
#       * missing_drugs: class = list, list of missing drug names
#
#   * Output:
#       * df: class = dataframe, dataframe with missing drugs appended to
#             original dataframe
################################################################################
replace_na_with_missing_drug = function(df, missing_drugs) {
  df$Pharmaceutical[is.na(df$Pharmaceutical)] = missing_drugs
  return(df)
}

################################################################################
# calculate_median() function: 
#   * Description: calculates median of "Value" for each group of
#                  "ID" in a data frame of properties from CompTox
#   * Input: 
#       * df: class = dataframe, dataframe for a particular physicochemical
#             property from CompTox
#
#   * Output:
#       * df: class = dataframe, dataframe with median calculated for property
#             (no return statement b/c of deployment with lapply)
################################################################################
calculate_median = function(df) {
  df %>%
    group_by(DTXSID, NAME, UNITS) %>%
    summarize(median_VALUE = median(VALUE))
}

################################################################################
# replace_na_with_missing_dtxsid() function:
#   * Description: replace NA values with missing DTXSID values
#   * Input: 
#       * df: class = data frame, data frame with NA values for DTXSID
#       * missing_dtxsid: class = list, list of DTXSIDs without data available
#                         for given property in CompTox
#
#   * Output:
#       * df: class = data frame, data frame with NA replaced by DTXSID
################################################################################
replace_na_with_missing_dtxsid <- function(df, missing_dtxsid) {

  na_indices <- which(is.na(df$DTXSID))
  
  num_missing <- length(na_indices)
  
  num_to_fill <- min(num_missing, length(missing_dtxsid))
  
  if (num_to_fill > 0) {
    df$DTXSID[na_indices[1:num_to_fill]] <- missing_dtxsid[1:num_to_fill]
  }
  
  return(df)
}

################################################################################
# join_with_common_df() function:
#   * Description: joins each dataframe in properties with dataframe with DTXSID
#                  value known, lets DTXSID be matched with pharmaceutical name
#   * Input: 
#       * df: class = dataframe, dataframe in properties list with DTXSID only
#       * common_df: class = dataframe, df from ID/molecular formula/molar mass
#                    sheet with both DTXSID and pharmaceutical name
#
#   * Output:
#       * df: class = dataframe, df that joins df and common_df (no return
#             statement needed because used in map function)
################################################################################
join_with_common_df = function(df, common_df) {
  df_with_dtxsid = df %>% filter(!is.na(DTXSID))
  left_join(df_with_dtxsid, common_df, by = "DTXSID")
}

################################################################################
# add_rows() function:
#   * Description: adds rows to a certain column of a dataframe and add certain
#                  values to the column of interest in those rows
#   * Input: 
#       * df: class = dataframe, dataframe where rows are being added
#       * column_name: class = character, name of column where certain values
#                      are being added
#       * specific_values: class = list, values being added to column in new
#                          rows
#
#   * Output:
#       * df: class = dataframe, dataframe with new rows added
################################################################################
add_rows <- function(df, column_name, specific_values) {
  new_rows <- as.data.frame(matrix(NA, nrow = 4, ncol = ncol(df)))
  colnames(new_rows) <- colnames(df)  
  
  new_rows[[column_name]] <- specific_values
  
  df <- bind_rows(df, new_rows)
  
  return(df)
}

################################################################################
# rename_column() function:
#   * Description: function to rename columns in dataframe (typically for those
#                  inside list structure)
#   * Input: 
#       * df: class = dataframe, dataframe where column name needs to be changed
#       * new_name: class = character or list of characters, name(s) to change
#                   column name to
#       * old_name: class = character or list of characters, original column
#                   name(s)
#
#   * Output:
#       * df: class = dataframe, dataframe with column name(s) changed
################################################################################
# Function to rename a column in a data frame
rename_column = function(df, new_name, old_name) {
  df = df %>% rename(!!new_name := old_name)
  return(df)
}

################################################################################
# reorder_columns() function:
#   * Description: change column order in dataframe (typically list of
#                  dataframes)
#   * Input: 
#       * df: class = dataframe, dataframe where columns need reordering
#       * new_order: class = list of characters, list of column names in order
#                    of how they should be changed
#
#   * Output:
#       * df: class = dataframe, dataframe with columns reordered
################################################################################
# Function to reorder columns of a dataframe
reorder_columns <- function(df, new_order) {
  df <- df %>% select(all_of(new_order))
  return(df)
}