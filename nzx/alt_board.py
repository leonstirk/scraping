# Import libraries
import urllib2
import json
import datetime
from bs4 import BeautifulSoup


date_handler = lambda obj: (
        obj.isoformat()
        if isinstance(obj, (datetime.datetime, datetime.date))
        else None
    )

# Alternative markets
url = 'https://www.nzx.com/markets/NZAX'

# query the website and return the html to the variable 'page'
page = urllib2.urlopen(url)

# parse the html using beautiful soup and store in soup
soup = BeautifulSoup(page, 'html.parser')

rows = soup.find_all('td', attrs={'data-title': 'Code'})

links = []

for row in rows:
    link = row.find('a').get('href').strip().encode('utf-8')
    links.append(link)

root = 'https://www.nzx.com'

data = []
dt = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")

for link in links:
    url = root+link

    stock = {}
    
    # query the website and return the html to the variable 'page'
    page = urllib2.urlopen(url)

    # parse the html using beautiful soup and store in soup
    soup = BeautifulSoup(page, 'html.parser')

    # get instrument-snapshot div
    instrument_snapshot = soup.find('section', attrs = {'class':'instrument-snapshot'})
    instrument_info     = soup.find_all('div', attrs = {'class':'instrument-info'})

    ticker  = instrument_snapshot.find('h2').text.strip().encode('utf-8')
    price   = instrument_snapshot.find('h1').text.strip().encode('utf-8')

    snapshot = instrument_snapshot.find_all('td')

    name     = snapshot[1].text.strip().encode('utf-8')
    issuer   = snapshot[3].text.strip().encode('utf-8')
    isin     = snapshot[5].text.strip().encode('utf-8')
    instrument_type = snapshot[7].text.strip().encode('utf-8')

    stock['ticker'] = ticker
    stock['price'] = price
    stock['name'] = name
    stock['issuer'] = issuer
    stock['isin'] = isin
    stock['instrument_type'] = instrument_type
    stock['data_access_timestamp'] = dt

    for i in range(0,len(instrument_info)):
        name_box  = instrument_info[i].find_all('strong')
        info_box  = instrument_info[i].find_all('td', attrs = {'class':'text-right'})
        for j in range(0,len(info_box)):
            name = name_box[j].text.strip().encode('utf-8').lower()
            info = info_box[j].text.strip().encode('utf-8')
            stock[name] = info
    
    data.append(stock)

with open('alt_'+dt+'.json', 'w') as outfile:
    json.dump(data, outfile)
