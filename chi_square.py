from scipy import stats
import csv

rawData = []
titles = []

obs_values = []
exp_values = []

def parse():

	rawData = []
	titles = []

	obs_values = []
	exp_values = []
	
	while True:
		fileName = raw_input("Enter csv file name: ")
		try: 
			f = open(str(fileName)+'.csv') #open file
			break
		except IOError:
			print "No such file or directory. Please try again."

	csv_f = csv.reader(f)
	for row in csv_f:
		rawData.append(row) #appends each row to rawData array
	titles = rawData.pop(0)	#removes first row as titles
	
	print titles
	print rawData

	return titles,rawData


def main():

	titles, rawData = parse()

	while True: #input checking
		test = raw_input("Please select '1' for goodness, '2' for independence test, or '3' to select a different file: ")
		#print test
		if test == "1" or test == "2" or test == "3":
			break

	if test=="1":

		while True:
			try:
				obs_index = titles.index(raw_input("Please select observed category: ")) #finds index in array of column
				break
			except ValueError:
				print "Could not find category in list."
		
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

	elif test=='3': 
		repeat()

	if raw_input("Another test? y/n: ") =="y":
		repeat()


def goodness(f_obs, f_exp=None):
	chi2, p = stats.chisquare(f_obs, f_exp=f_exp)
	print "chi-square statistic: %.6f \np-value: %.6f" %(chi2, p)

def independence(table):
	chi2, p, df, f_exp = stats.chi2_contingency(table)
	print "chi-square statistic: %.6f \np-value: %.6f \ndegrees of freedom: %d \nexpected values: %s" %(chi2, p, df, f_exp)

def repeat():
	main()

main()
