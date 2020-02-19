from bs4 import BeautifulSoup
import urllib2
import csv 


def writeStatsToFile(stats):
  counter = 0;
  player = '';
  fantasyPoints = '';
  team = '';
  for line in stats:
    #QB CASE
    if positionID == 10:
      if counter == 0:
        player = line.text.encode('utf-8');
        playerName = player[player.find(' ')+1:]
      if counter == 1:
        team = line.text.encode('utf-8');
      if counter == 11:
        fantasyPoints = line.text.encode('utf-8');
      if counter == 12:
        print([playerName, team, positionDict[str(positionID)], season, week, fantasyPoints])
        writer.writerow([playerName, team, positionDict[str(positionID)], season, week, fantasyPoints])
        counter = -1;
        playerName = '';
        fantasyPoints = '';
        team = '';
      counter += 1;
      #RB/WR CASE
    elif positionID == 20 or positionID == 30:
      if counter == 0:
        player = line.text.encode('utf-8');
        playerName = player[player.find(' ')+1:]
      if counter == 1:
        team = line.text.encode('utf-8');
      if counter == 10:
        fantasyPoints = line.text.encode('utf-8');
      if counter == 11:
        print([playerName, team, positionDict[str(positionID)], season, week, fantasyPoints])
        writer.writerow([playerName, team, positionDict[str(positionID)], season, week, fantasyPoints])
        counter = -1;
        playerName = '';
        fantasyPoints = '';
        team = '';
      counter += 1;
      #TE Case
    elif positionID == 40:
      if counter == 0:
        player = line.text.encode('utf-8');
        playerName = player[player.find(' ')+1:]
      if counter == 1:
        team = line.text.encode('utf-8');
      if counter == 7:
        fantasyPoints = line.text.encode('utf-8');
      if counter == 8:
        print([playerName, team, positionDict[str(positionID)], season, week, fantasyPoints])
        writer.writerow([playerName, team, positionDict[str(positionID)], season, week, fantasyPoints])
        counter = -1;
        playerName = '';
        fantasyPoints = '';
        team = '';
      counter += 1; 
      #Kicker Case
    elif positionID == 80:
      if counter == 0:
        player = line.text.encode('utf-8');
        playerName = player[player.find(' ')+1:]
      if counter == 1:
        team = line.text.encode('utf-8');
      if counter == 8:
        fantasyPoints = line.text.encode('utf-8');
      if counter == 9:
        print([playerName, team, positionDict[str(positionID)], season, week, fantasyPoints])
        writer.writerow([playerName, team, positionDict[str(positionID)], season, week, fantasyPoints])
        counter = -1;
        playerName = '';
        fantasyPoints = '';
        team = '';
      counter += 1; 
      # Defense Case
    elif positionID == 99:
      if counter == 0:
        player = line.text.encode('utf-8');
        playerName = player[player.find(' ')+1:]
      if counter == 1:
        team = line.text.encode('utf-8');
      if counter ==11:
        fantasyPoints = line.text.encode('utf-8');
      if counter == 12:
        print([playerName, team, positionDict[str(positionID)], season, week, fantasyPoints])
        writer.writerow([playerName, team, positionDict[str(positionID)], season, week, fantasyPoints])
        counter = -1;
        playerName = '';
        fantasyPoints = '';
        team = '';
      counter += 1; 
positionDict = {'10': 'QB', '20': 'RB', '30': 'WR', '40': 'TE', '80': 'K', '99': 'DEF' }


with open('historicalStats.csv', 'wb') as csvfile:
  writer = csv.writer(csvfile)
  for season in range(2000,2016):
    for positionID in [10,20,30,40,80,99]:
      for week in range(1,18):
        baseUrl = 'http://fftoday.com'
        url = 'http://fftoday.com/stats/playerstats.php?Season=' + str(season) +'&GameWeek=' + str(week) +'&PosID=' + str(positionID) +'&LeagueID=26955'
        page = urllib2.urlopen(url)
        soup = BeautifulSoup(page, "html.parser")
        stats = soup.findAll(class_= 'sort1')
        while True: 
          writeStatsToFile(stats);
          if (soup.find('a', href=True, text='Next Page') is None):
            break;
          else:
            temp = soup.find('a', href=True, text='Next Page')
            print(temp['href'])
            url = baseUrl + temp['href']
            page = urllib2.urlopen(url)
            soup = BeautifulSoup(page, "html.parser")
            stats = soup.findAll(class_= 'sort1')
        

        
            
      



