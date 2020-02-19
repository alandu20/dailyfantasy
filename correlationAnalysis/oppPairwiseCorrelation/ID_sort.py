import csv as csv

csv_file_object = csv.reader(open('2015_teams.csv', 'rb'))
data=[]        
sortedString = ""
myfile = open('testAlan.csv', 'wb')
wr = csv.writer(myfile, quoting=csv.QUOTE_ALL)
for row in csv_file_object:
    sortedString = ''.join(sorted(row[0])) 
    wr.writerow([sortedString])

