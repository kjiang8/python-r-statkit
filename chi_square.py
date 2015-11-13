#!/usr/bin/env python
from scipy import stats
import csv

rawData = []
titles = []

obs_values = []
exp_values = []

obs_index = 0
exp_index = 0

def parse(test):
	'''
	this function is a file input handler. it opens .csv files, reads the file column names and contents, and then closes the file

	this function takes one parameter, test, which defaults to False. turning setting test to TRUE runs programs in a test mode, which allows for additional  information in each section
	'''

	rawData = []
	titles = []

	obs_values = []
	exp_values = []
	
	while True:
		fileName = raw_input("Enter csv file name: ")
		try: 
			f = open(str(fileName)+'.csv','rU') #open file
			break
		except IOError:
			print "No such file or directory " + fileName + ".csv, please try again."

	csv_f = csv.reader(f,dialect='excel')
	for row in csv_f:
		rawData.append(row) #appends each row to rawData array
	titles = rawData.pop(0)	#removes first row as titles
	
	print "[Section 2]"
	print "Coln # Coln Name"
	for i in range(len(titles)):
		print str.center(str(i+1), 8), titles[i]

	f.close()#close file to clean up

	return titles,rawData

def x2_gof(test=False):
	'''
	chi-square test for goodness of fit: to see if a sample of data matches a population with a specific distribution
	test is an optional parameter that defaults to False. when test is turned on, additional information is printed
	'''

	reset() #makes sure all variables are reset before beginning
	titles, rawData = parse(test)

	while True:
		obs_input = raw_input("Please select observed category by coln number or name: ")
		
		try: #test for integer
			testcase = int(float(obs_input))
			if testcase==True & testcase < len(rawData)+1:
				obs_index = testcase-1
			if test==True:
				print "[Section 3]"
				print 'integer obs_index: ',obs_index
			break
		
		except ValueError: #not integer
			try:
				obs_index = titles.index(obs_input) #finds index in array of column
				if test==True:
					print "[Section 3]"
					print "matched obs_index: ", obs_index
				break
			except ValueError:
				print "Could not find category in list."
		
	exp_input = raw_input("Please select expected category by coln number or name. If none, leave blank: ")

	if test==True:
		print "[Section 4]"

	if exp_input=="":
		print "Evenly distributed data"
		for i in range(len(rawData)):
				#converts string to float based on column selected
			obs_values.append(float(rawData[i][obs_index]))
			
		goodness(obs_values)

	else:
	
		try: #test for integer
			testcase2 = int(float(exp_input))
			if testcase2 < len(rawData)+1:
				exp_index = testcase2-1
				if test==True:
					print 'integer exp_index: ',exp_index

				getgofdata(obs_index, exp_index, rawData, test)

			else:
				print "index out of range"
			
		except ValueError: #not integer
			try:
				exp_index = titles.index(exp_input) #tries finding index
				
				if test==True:
					print "matched exp_index: ", exp_index

				getgofdata(obs_index, exp_index, rawData, test)

			except ValueError:
				#doesn't find index, so assume expected is uniformly distributed
				print "Could not find index in list"



def x2_ind(test=False):
	'''
	chi-square test for independence: used to determine if there is a significant association between two variables
	test is an optional parameter that defaults to False. when test is turned on, additional information is printed
	'''

	reset()
	titles, rawData = parse(test)

	#print 'rawData: ',rawData

	sel_input = raw_input("Please select columns by name or number to conduct independence test. If selecting multiple columns, use commas or semicolons (ex. 1:3,6): ")

	list_index = parseinput(sel_input, titles, test)

	flipped = zip(*rawData) #transpose rows and columns of rawData, since loops horizontally

	if list_index < len(rawData):

		for i in list_index:
			try:
				obs_values.append(map(int,flipped[i]))
			except IndexError:
				print "could not find index in table"

		if test==True:
			print "[Section 3]"
			print "Data input values: ",obs_values
				
		independence(obs_values, test)

	else:
		print "List index out of range"

def parseinput(sel_input, titles,test):
	#parses selected input to create list_index

	list_index = []

	sel_input = sel_input.replace(" ","") #strips all whitespace
	sel_input = sel_input.split(",") #splits by commas

	print "selected input: ",sel_input

	for i in range(len(sel_input)):

		if ":" in sel_input[i]:
			chain = sel_input[i].split(":") #split by semicolon
			
			try:
				for i in range(int(chain[1]) - int(chain[0])+1):
					list_index.append(i + int(chain[0])-1)
			except ValueError:
				print "if using semicolon, must use column number"
		
		else:	
			try: #test for integer
				sel_index = int(float(sel_input[i]))
				list_index.append(sel_index-1)
			except ValueError:
				try:
					sel_index = titles.index(sel_input[i]) #tries finding index
					list_index.append(sel_index)
				except IndexError:
					print "cannot find index in headers"
				except ValueError:
					print "cannot find column name in header"

	if test==True:
		print "list_index to use for independence test: ", list_index

	return list_index

def getgofdata(obs_index, exp_index, rawData, test):
	#prints observed and expected values for goodness of fit test using indeces

	try: 
		for i in range(len(rawData)):
			obs_values.append(float(rawData[i][obs_index]))
			exp_values.append(float(rawData[i][exp_index]))
		
		if test==True:		
			print "obs_values: ", obs_values
			print "exp_values: ", exp_values
		
		goodness(obs_values, exp_values)

	except IndexError:
		print "Unequal lengths of observed and expected frequencies"

def goodness(f_obs, f_exp=None): #conducts actual test and prints result
	chi2, p = stats.chisquare(f_obs, f_exp=f_exp)
	print "chi-square statistic: %s \np-value: %s" %(chi2, p)

def independence(table, test): #conducts test for independence and prints result depending on mode
	chi2, p, df, f_exp = stats.chi2_contingency(table)

	if test==True:
		print "chi-square statistic: %s \np-value: %s \ndegrees of freedom: %d \nexpected values: %s" %(chi2, p, df, f_exp)
	else: 
		print "chi-square statistic: %s \np-value: %s" %(chi2, p)

def reset():
	#resets all global variables

	global rawData
	rawData = []
	global titles
	titles = []

	global obs_values
	obs_values = []
	global exp_values
	exp_values = []

	global obs_index
	obs_index = 0
	global exp_index
	exp_index = 0

