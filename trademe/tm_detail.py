# Import libraries
import urllib2
import json
import datetime
import re
import time
from bs4 import BeautifulSoup

dt = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")

# specify the url
root = 'https://www.trademe.co.nz'

all_listings =[]

with open("overviewToDetail.json", "r") as read_file:
    links = json.load(read_file)

for link in links:
    
    url = root+link

    # query the website and return the html to the variable 'page'
    page = urllib2.urlopen(url)

    # parse the html using beautiful soup and store in soup
    soup = BeautifulSoup(page, 'html.parser')

    attributes = soup.find('table', attrs={'id','ListingAttributes'})
    rows_list = attributes.find_all('tr')

    listing = {}

    description = soup.find('div', attrs={'class': 'ListingDescription'})

    listing['Description'] = description.text.encode('utf-8').strip()
    listing['Link'] = link
    
    for row in rows_list:
    
        key = row.find('th')
        value = key.next_sibling.next_sibling.text.encode('utf-8').strip()
        key = key.text.encode('utf-8').strip()
        listing[key] = value

    all_listings.append(listing)
        
with open('tradeMeDetail.json', 'w') as outfile:
        json.dump(all_listings, outfile)
