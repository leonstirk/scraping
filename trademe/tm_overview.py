# Import libraries
import urllib2
import json
import datetime
import re
import time
from bs4 import BeautifulSoup

dt = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    
i = 1
switch = 0
stop = 0

all_listings = []

while stop == 0:

    page_listings = []
    
    # specify the url
    url = 'https://www.trademe.co.nz/Browse/CategoryAttributeSearchResults.aspx?keyval=10222766&sort_order=expiry_desc&page='+str(i)
    
    # query the website and return the html to the variable 'page'
    page = urllib2.urlopen(url)

    # parse the html using beautiful soup and store in soup
    soup = BeautifulSoup(page, 'html.parser')

    card_list = soup.find_all('li', attrs={'class': 'tmp-search-card-list-view'})

    for card in card_list:

        listing = {}
        
        listing_date = card.find('div', attrs={'class': 'tmp-search-card-list-view__listing-date'})
        title = card.find('div', attrs={'class': 'tmp-search-card-list-view__title'})

        if str(title) == 'None' or len(listing_date.contents) < 2:
            pass
        elif str(listing_date.contents[1].text) == 'Listed Today':
            pass
        elif str(listing_date.contents[1].text) == 'Listed Yesterday':
            title = title.text.encode('utf-8').strip()
            subtitle = card.find('div', attrs={'class': 'tmp-search-card-list-view__subtitle'}).text.encode('utf-8').strip()
            price = card.find('div', attrs={'class': 'tmp-search-card-list-view__price'}).text.encode('utf-8').strip()
            link = card.find('a').get('href')

            attributes = card.find('div', attrs={'class':'tmp-search-card-list-view__attributes'}).find_all('div', attrs={'class': 'tmp-search-card-list-view__attribute'})

            for a in attributes:
                key = a.find('img')
                value = key.previous_sibling.previous_sibling.text.encode('utf-8').strip()
                key = key.get('alt').encode('utf-8').lower()
                listing[key] = value
            
            listing['title'] = title
            listing['subtitle'] = subtitle
            listing['price'] = price
            listing['link'] = link

            page_listings.append(listing)
        else:
            pass                            
        
    # rate limit requests
    # time.sleep(1)
        
    if len(page_listings) == 0 and switch == 0:
        i += 1
        print 'pre-data'
    elif len(page_listings) > 0:
        switch = 1
        i += 1
        print url
        all_listings += page_listings
    elif len(page_listings) == 0 and switch == 1:
        stop = 1
        print 'post-data'
    else:
        print 'error'
        stop = 1
    
with open('tradeMeOverview.json', 'w') as outfile:
    json.dump(all_listings, outfile)
