remove_drugs = function(full_drug_list, removal_drug_list){
  for (i in 1:length(removal_drug_list)){
    full_drug_list = full_drug_list [ !full_drug_list == removal_drug_list[i]]
  }
  return(full_drug_list)
}