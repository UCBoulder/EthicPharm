# remove drugs from main for loop
remove_drugs = function(full_drug_list, removal_drug_list){
  for (i in 1:length(removal_drug_list)){
    full_drug_list = full_drug_list [ !full_drug_list == removal_drug_list[i]]
  }
  return(full_drug_list)
}

# function to fix miscoded drugs
fix_drug_coding = function(df, incorrect, correct){
  df = df %>%
    mutate(Drug = case_when(Drug==incorrect ~ correct,
                          .default=Drug))
  return(df)
}