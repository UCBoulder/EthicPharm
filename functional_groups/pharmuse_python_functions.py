import string
import os
import pandas as pd

def file_reader(file_name):
	"""Read a csv file and produce dataframe

	Parameters:
	----------
	file_name : string
		Name of the file to be processed

	Returns:
	--------
	file_df: dataframe
		Pandas dataframe of csv file
	"""

	if type(file_name) != str:
		raise TypeError("invalid input: must be string")

	# check if error raised for empty file
	if os.path.getsize(file_name) == 0:
		raise Exception('file is empty')

	file_contents = open(file_name, 'r')
	# Read the CSV file into a DataFrame
	file_df = pd.read_csv(file_contents)
	file_contents.close()
	return file_df

	raise FileNotFoundError('file not found')
