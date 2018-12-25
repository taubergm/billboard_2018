import requests
from bs4 import BeautifulSoup
import re
import csv
import sqlite3
import datetime

conn = sqlite3.connect('billboard.sqlite')
conn.text_factory = bytes
cur = conn.cursor()

# Make some fresh tables using executescript()
cur.executescript('''

DROP TABLE IF EXISTS Headlines;


CREATE TABLE Headlines (
    id  INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT UNIQUE,
    date    TEXT, 
    url     TEXT UNIQUE,
    headline TEXT
);

''')


def quantize_date(date):
    """Quantizes the passed date to the nearest Saturday, since
    Billboard charts are always dated by Saturday.
    This behavior is consistent with the website, even though charts
    are released 11 days in advance.
    E.g., entering 2016-07-19 corresponds to the chart dated 2016-07-23.

    Args:
        date: The chart date as a string, in YYYY-MM-DD format.
    """
    year, month, day = map(int, date.split('-'))
    passedDate = datetime.date(year, month, day)
    passedWeekday = passedDate.weekday()
    if passedWeekday == 5:  # Saturday
        return date
    elif passedWeekday == 6:  # Sunday
        quantizedDate = passedDate + datetime.timedelta(days=6)
    else:
        quantizedDate = passedDate + datetime.timedelta(days=5 - passedWeekday)
    return str(quantizedDate)

stories = []   # array of stories from TMZ

OUTFILE = "billboard_2018.csv"


num_days_to_get = 350

with open(OUTFILE, 'wb') as output_file:
    
    keys = ['date', 'artist', 'title', 'rank', 'peak_pos', 'last_week', 'weeks']
    dict_writer = csv.DictWriter(output_file, keys)
    dict_writer.writeheader()
    

    songlist = [dict() for x in range(100)]  # creat list of 100 track dicts
    
    
    # create an array og 


    for i in range(0,num_days_to_get,7):
        #day = datetime.datetime.now() - datetime.timedelta(days=21008) - datetime.timedelta(days=i) 
        day = datetime.datetime.now() - datetime.timedelta(days=i) 
        date = str(day.date())
        print date 
        date = quantize_date(date)
        url = "https://www.billboard.com/charts/hot-100/%s" % date

        print url
        page = requests.get(url)
        #print page.content
        
        soup = BeautifulSoup(page.content, 'html.parser')
        #print(soup.prettify())


        # the number one song is presented differently than the rest and needs special handling
        data = soup.find_all(class_='chart-number-one__weeks-on-chart')
        songlist[0]['weeks'] = data[0].renderContents()
    
        data = soup.find_all(class_='chart-number-one__title')
        songlist[0]['title'] = data[0].renderContents()
    
        #data = soup.find_all(class_='chart-number-one__artist')
        #print data[0].renderContents()
        #songlist[0]['artist'] = data[0].renderContents()

        data = soup.find_all(class_='chart-video__wrapper')
        full_title = data[0]['data-title']
        artist,title = full_title.split(" - ")
        songlist[0]['artist'] = artist
        songlist[0]['rank'] = '1'


        # Now get the rest of the tracks
        data = soup.find_all(class_='chart-list-item')
    
        i = 1
        for track in data:
            songlist[i]['artist'] = track['data-artist']
            songlist[i]['title'] = track['data-title']
            songlist[i]['rank'] = track['data-rank']
            i = i + 1
            

        data = soup.find_all(class_='chart-list-item__last-week') # weeks on chart
        i = 1
        for d in data:
            songlist[i]['last_week'] = d.renderContents()
            i = i + 1
        
        data = soup.find_all(class_='chart-list-item__weeks-on-chart') # weeks on chart
        i = 1
        for d in data:
            songlist[i]['weeks'] = d.renderContents()
            i = i + 1

        data = soup.find_all(class_='chart-list-item__weeks-at-one')  # peak position
        i = 1
        for d in data:
            songlist[i]['peak_pos'] = d.renderContents()
            i = i + 1

        for song in songlist:
            song['date'] = date
            dict_writer.writerow(song)     

        
        

        #for entrySoup in soup.find_all('data-artist', {'class': 'chart-list-item'}):
        #    print entrySoup
            #import sys
            #sys.exit()


