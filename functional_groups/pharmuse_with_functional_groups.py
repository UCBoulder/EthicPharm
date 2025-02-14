""" Imports PharmUse database and adds functional groups present for each pharmaceutical
    based on SMILES

	* ppf.file_reader - reads csv file and stores contents in df
"""

#TODO: continue adding best practices as going!!! but do PEP8 at end
#TODO: find Python package that works better than thermo or rdkit, the current
#      implementation is using rdkit.Chem.Fragments, which seems to perform
#      slightly better than thermo, as it does not give false positives for esters;
#      planning to try pikachu or CDK for final paper; current implementation
#      includes the 40 most common functional groups based on this article:
#      https://pubs-acs-org.colorado.idm.oclc.org/doi/10.1021/acs.jmedchem.0c00754

import argparse
import string
import os
import pandas as pd
import sys
from rdkit import Chem
from rdkit.Chem import inchi, Fragments

import pharmuse_python_functions as ppf

def main():

	parser = argparse.ArgumentParser(
        	description='adds functional groups to chemical database',
        	prog='functional_groups')
	parser.add_argument('--database_file_name',
              	type=str,
             	help='Name of file with database',
              	required=True)
	args = parser.parse_args()


# read in file with PharmUse database & remove first column
	try:
		pharmuse_db = ppf.file_reader(args.database_file_name)
		pharmuse_db = pharmuse_db.drop(pharmuse_db.columns[0], axis=1)
	except FileNotFoundError as e:
		print(f"could not find {args.database_file_name}")
		sys.exit(1)
	except Exception as e:
		print(f"error of type {type(e)} occurred")
		sys.exit(1)

# add 40 new columns to PharmUse database for 40 most common functional groups available in Fragments
# NOTE: aromatic ketones, alkenes, alkynes, enone skipped (no function in Fragments), aliphatic and aromatic primary amines
#       are counted as primary amines since not distinguished in Fragments, 1 skipped (row 3, col 8) b/c
#       could not ID and same for row 4, col 2; row 4, col 3; row 4, col 5; row 4, col 8; row 5, col 3; row 5, col 6; row 5, col 7
#       P group left out b/c no exact match
	try:
		for i in range(22):
    			pharmuse_db[f'Condition_{i+1}'] = 0

		func_group_colnames = ['Amides','Ethers','Tertiary amines','Alkyl halides',
					'Secondary amines','Aliphatic alcohols',
					'Aromatic alcohols','Carboxylic acids','Sulfonamides',
					'Primary amines','Ureas','Esters','Nitriles','Ketones',
					'Sulfides','Carbamates','Sulfones','Guanidines',
					'Amidines', 'Thiols','Imines','Imides']

		old_colnames = pharmuse_db.columns[38:61]
		columns_to_rename = dict(zip(old_colnames, func_group_colnames))
		pharmuse_db = pharmuse_db.rename(columns=columns_to_rename)

	except Exception as e:
		print(f"error of type {type(e)} occurred")
		sys.exit(1)

# populate the functional group columns with 1 if functional group present, 0 if not for each pharmaceutical
# NOTE: should count number of each functional group, but not sure if it does that b/c only 0,1 output
# NOTE: issues with Fragments: misses some ketones (cipro) and carbamates (methocarbamol),
#       counts all esters as ethers, may be other errors too
	try:
		func_group_checks = {
			'Amides': Fragments.fr_amide,
			'Ethers': Fragments.fr_ether,
			'Tertiary amines': Fragments.fr_NH0,
			'Alkyl halides': Fragments.fr_alkyl_halide,
			'Secondary amines': Fragments.fr_NH1,
			'Aliphatic alcohols': Fragments.fr_Al_OH,
			'Aromatic alcohols': Fragments.fr_Ar_OH,
			'Carboxylic acids': Fragments.fr_COO,
			'Sulfonamides': Fragments.fr_sulfonamd,
			'Primary amines': Fragments.fr_NH2,
			'Ureas': Fragments.fr_urea,
			'Esters': Fragments.fr_ester,
			'Nitriles': Fragments.fr_nitrile,
			'Ketones': Fragments.fr_ketone,
			'Sulfides': Fragments.fr_sulfide,
			'Carbamates': Fragments.fr_alkyl_carbamate,
			'Sulfones': Fragments.fr_sulfone,
			'Guanidines': Fragments.fr_guanido,
			'Amidines': Fragments.fr_amidine,
			'Thiols': Fragments.fr_SH,
			'Imines': Fragments.fr_Imine,
			'Imides': Fragments.fr_imide
		}

		# ISSUES WITH thermo and rdkit.Chem.Fragments
		# NOTE: SMILES that did not work were updated to PubChem isomeric SMILES (when available) or PubChem canonical SMILEs (when not); lit documents inconsistent SMILES b/t CompTox & PubChem
		# NOTE: issue with lactulose and naproxen ester ID - need to keep vetting the other drugs and see if just esters or other issue too, contact creator and look for other pkg, possibly
		# with Inchikey instead (check lit if more consistent)
		# canonical SMILES may be better than isomeric b/c always present and w/out @, currently using canonical Pubchem for lactulose, isomeric if available (canonical if not) for valacyclovir,
		# pregabalin, diltiazem, lisinopril, phenytoin - will need to update in code that produces all_data.csv, so changes to all_data.csv are not overwritten (currently overwritten - also, these,
		# are the drugs with >1 version in CompTox, check rdkit docs too - must be
		# canonical SMILES? still gives issue with ester though for lactulose - check for naproxen; amine is right but primary/secondary not found for metformin (tertiary is found); also something
		# funky with imines (looks like secondary ketimine?), check also if larger func groups contain smaller ones and that's why missing

		# Apply the checking functions to each columns
		for group_name, check_function in func_group_checks.items():
			print(f"Processing group: {group_name} with function: {check_function}")
			pharmuse_db[group_name] = pharmuse_db['INCHI_STRING'].apply(lambda inchistring: 1 if check_function(inchi.MolFromInchi(inchistring)) else 0)

	except Exception as e:
		print(f"error of type {type(e)} occurred: {e}")
		print(f"Check function: {check_function}")
		print(f"Group name: {group_name}")
		print(f"INCHIKEY value: {pharmuse_db['INCHIKEY'].head()}")
		sys.exit(1)
	#print(pharmuse_db)
	#print(func_group_checks)

	# Write the DataFrame to a CSV file
	output_csv_file = 'pharmuse_db_with_functional_groups.csv'
	pharmuse_db.to_csv(output_csv_file, index=False)

if __name__ == '__main__':
    main()
