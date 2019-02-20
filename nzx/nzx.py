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

# hit the main board page and scrape all the instrument links to populate the quote page link list below

# specify the url
quote_page = [
    'https://www.nzx.com/instruments/ABA',
    'https://www.nzx.com/instruments/AFC',
    'https://www.nzx.com/instruments/AFI',
    'https://www.nzx.com/instruments/AFT',
    'https://www.nzx.com/instruments/AIA',
    'https://www.nzx.com/instruments/AIR',
    'https://www.nzx.com/instruments/ALF',
    'https://www.nzx.com/instruments/AMP',
    'https://www.nzx.com/instruments/ANZ',
    'https://www.nzx.com/instruments/AOR',
    'https://www.nzx.com/instruments/APA',
    'https://www.nzx.com/instruments/APL',
    'https://www.nzx.com/instruments/ARG',
    'https://www.nzx.com/instruments/ARV',
    'https://www.nzx.com/instruments/ASBPA',
    'https://www.nzx.com/instruments/ASBPB',
    'https://www.nzx.com/instruments/ASD',
    'https://www.nzx.com/instruments/ASF',
    'https://www.nzx.com/instruments/ASP',
    'https://www.nzx.com/instruments/ASR',
    'https://www.nzx.com/instruments/ATM',
    'https://www.nzx.com/instruments/AUG',
    'https://www.nzx.com/instruments/AWF',
    'https://www.nzx.com/instruments/BGI',
    'https://www.nzx.com/instruments/BGP',
    'https://www.nzx.com/instruments/BIT',
    'https://www.nzx.com/instruments/BLT',
    'https://www.nzx.com/instruments/BRM',
    'https://www.nzx.com/instruments/BRMWE',
    'https://www.nzx.com/instruments/CAV',
    'https://www.nzx.com/instruments/CBL',
    'https://www.nzx.com/instruments/CDI',
    'https://www.nzx.com/instruments/CEN',
    'https://www.nzx.com/instruments/CMO',
    'https://www.nzx.com/instruments/CNU',
    'https://www.nzx.com/instruments/CO2',
    'https://www.nzx.com/instruments/CVT',
    'https://www.nzx.com/instruments/DGL',
    'https://www.nzx.com/instruments/DIV',
    'https://www.nzx.com/instruments/DOW',
    'https://www.nzx.com/instruments/EBO',
    'https://www.nzx.com/instruments/EMF',
    'https://www.nzx.com/instruments/ERD',
    'https://www.nzx.com/instruments/EUF',
    'https://www.nzx.com/instruments/EVO',
]

# for loop
data = []
print datetime.datetime.now()
dt = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
print dt

for qp in quote_page:

    stock = {}
    
    # query the website and return the html to the variable 'page'
    page = urllib2.urlopen(qp)

    # parse the html using beautiful soup and store in soup
    soup = BeautifulSoup(page, 'html.parser')

    # get instrument-snapshot div
    instrument_snapshot = soup.find('section', attrs = {'class':'instrument-snapshot'})
    instrument_info     = soup.find_all('div', attrs = {'class':'instrument-info'})

    ticker  = instrument_snapshot.find('h2').text.strip().encode('utf-8')
    price   = instrument_snapshot.find('h1').text.strip().encode('utf-8')
    name    = instrument_snapshot.find_all('td', limit = 2)[1].text.strip().encode('utf-8')
    
    stock['Ticker'] = ticker
    stock['Price'] = price
    stock['Name'] = name
    stock['Data access datetime'] = dt
    
    for i in range(0,len(instrument_info)):
        name_box  = instrument_info[i].find_all('strong')
        info_box  = instrument_info[i].find_all('td', attrs = {'class':'text-right'})
        for j in range(0,len(info_box)):
            name = name_box[j].text.strip().encode('utf-8')
            info = info_box[j].text.strip().encode('utf-8')
            stock[name] = info
    
    data.append(stock)

with open(dt+'.json', 'w') as outfile:
    json.dump(data, outfile)
