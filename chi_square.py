from scipy import stats
import csv

rawData = []
titles = []

obs_values = []
exp_values = []

def parse(fileName):

	f = open(str(fileName)+'.csv') #open file
	csv_f = csv.reader(f)
	for row in csv_f:
		rawData.append(row) #appends each row to rawData array
	titles = rawData.pop(0)	#removes first row as titles
	
	print titles
	print rawData

	test = raw_input("Please select '1' for goodness or '2' for independence test: ")

	if test=="1":
		
		obs_index = titles.index(raw_input("Please select observed category: ")) #finds index in array of column
		
		exp_input = raw_input("Please select expected category. If none, leave blank: ")

		try: 
			exp_index = titles.index(exp_input) #tries finding index
			
			for i in range(len(rawData)):
				obs_values.append(float(rawData[i][obs_index]))
				exp_values.append(float(rawData[i][exp_index]))
			
			goodness(obs_values, exp_values)
		
		except ValueError:
			#doesn't find index, so assume expected is uniformly distributed

			for i in range(len(rawData)):
				#converts string to float based on column selected
				obs_values.append(float(rawData[i][obs_index]))
			
			goodness(obs_values)

	elif test=="2":
		
		for i in rawData:
			try: 
				obs_values.append(map(float,i)) #converts strings to floats
			except ValueError: 
				obs_values.append(map(float,i[1:])) #discards first column
		
		print obs_values
		
		independence(obs_values)

	else: 
		print "exception handling"

def goodness(f_obs, f_exp=None):
	chi2, p = stats.chisquare(f_obs, f_exp=f_exp)
	print "chi-square statistic: %.6f \np-value: %.6f" %(chi2, p)

def independence(table):
	chi2, p, df, f_exp = stats.chi2_contingency(table)
	print "chi-square statistic: %.6f \np-value: %.6f \ndegrees of freedom: %d \nexpected values: %s" %(chi2, p, df, f_exp)

def prompt():
	fileName = raw_input("Enter csv file name: ")
	print fileName
	parse(fileName)

prompt()
