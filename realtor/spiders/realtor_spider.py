from scrapy import Spider
from realtor.items import RealtorItem
from scrapy import Request
import re
from scraper_api import ScraperAPIClient

client = ScraperAPIClient('85a00469c8831d13f76aab639fe4e676')
num_items = 0


class RealtorSpider(Spider):
    name = 'realtor_spider'
    allowed_domains = ['api.scraperapi.com']
    def start_requests(self):
        yield Request(client.scrapyGet('https://www.realtor.com/realestateandhomes-search/San-Francisco_CA/'), self.parse, meta={'num_items':0, 'area': 'San Francisco'})
        yield Request(client.scrapyGet('https://www.realtor.com/realestateandhomes-search/San-Jose_CA/'), self.parse, meta={'num_items':0, 'area': 'San Jose'})
        yield Request(client.scrapyGet('https://www.realtor.com/realestateandhomes-search/New-York_NY/'), self.parse, meta={'num_items':0, 'area': 'NYC'})
        yield Request(client.scrapyGet('https://www.realtor.com/realestateandhomes-search/Chicago_IL/'), self.parse, meta={'num_items':0, 'area': 'Chicago'})
        yield Request(client.scrapyGet('https://www.realtor.com/realestateandhomes-search/Los-Angeles_CA/'), self.parse, meta={'num_items':0, 'area': 'Los Angeles'})
        yield Request(client.scrapyGet('https://www.realtor.com/realestateandhomes-search/Houston_TX/'), self.parse, meta={'num_items':0, 'area': 'Houston'})
        yield Request(client.scrapyGet('https://www.realtor.com/realestateandhomes-search/Denver_CO/'), self.parse, meta={'num_items':0, 'area': 'Denver'})
        yield Request(client.scrapyGet('https://www.realtor.com/realestateandhomes-search/Phoenix_AZ/'), self.parse, meta={'num_items':0, 'area': 'Phoenix'})
        yield Request(client.scrapyGet('https://www.realtor.com/realestateandhomes-search/Philadelphia_PA/'), self.parse, meta={'num_items':0, 'area': 'Philadelphia'})
        yield Request(client.scrapyGet('https://www.realtor.com/realestateandhomes-search/Washington_DC/'), self.parse, meta={'num_items':0, 'area': 'DC'})
        yield Request(client.scrapyGet('https://www.realtor.com/realestateandhomes-search/Seattle_WA/'), self.parse, meta={'num_items':0, 'area': 'Seattle'})
        yield Request(client.scrapyGet('https://www.realtor.com/realestateandhomes-search/Charlotte_NC/'), self.parse, meta={'num_items':0,'area': 'Charlotte'})


        # yield Request('https://www.realtor.com/realestateandhomes-search/San-Francisco_CA/', 
            # callback=self.parse,
            # errback=self.errback,
            # headers={'User-Agent':
            # 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_4) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/13.1 Safari/605.1.15'})            
    def parse(self, response):
        num_items = response.meta['num_items']

        property_list_container = response.xpath('//ul[@data-testid="property-list-container"]')
        print(response.body)
        property_cards = property_list_container.xpath('.//div[@data-label="property-card"]')
        for property_card in property_cards:
            price = property_card.xpath('.//span[@data-label="pc-price"]/text()').get()
            bed = property_card.xpath('.//li[@data-label="pc-meta-beds"]/span[@data-label="meta-value"]/text()').get()
            bath = property_card.xpath('.//li[@data-label="pc-meta-baths"]/span[@data-label="meta-value"]/text()').get()
            sqft = property_card.xpath('.//li[@data-label="pc-meta-sqft"]/span[@data-label="meta-value"]/text()').get()
            #sqftlot = property_card.xpath('.//li[@data-label="pc-meta-sqftlot"]/span[@data-label="meta-value"]/text()').get()
            address = property_card.xpath('.//div[@data-label="pc-address"]/text()').get()
            
            property_url= property_card.xpath('.//a[@data-testid="property-anchor"]/@href').get()
            full_property_url = f'https://www.realtor.com{property_url}'
            #print(full_property_url)

            item = RealtorItem()


            #yield Request(client.scrapyGet(full_property_url), self.parse_property, meta={'item':item})

            try:
                city,raw_state,_,zip = property_card.xpath('.//div[@data-label="pc-address-second"]/text()').getall()
            except:
                city,raw_state,_,zip = None, None, None, None
            if raw_state:
                state = re.search('\w+', raw_state).group()
            else:
                state = None

            if price:
                item['price'] = price.replace(',','')[1:]
            
            item['bed'] = bed
            if bath:
                item['bath'] = bath.replace('+','')
            if sqft:
                item['sqft'] = sqft.replace(',','')
            # if sqftlot:
            #     item['sqftlot'] = sqftlot.replace(',','')
            item['address'] = address
            item['city'] = city
            item['state'] = state
            item['zip'] = zip
            item['area'] = response.meta['area']




            num_items +=1
            if num_items <= 5:
                yield Request(client.scrapyGet(full_property_url), self.parse_property, meta={'item':item})
            else: 
                return


            print(item)
            # yield item
        pagination = response.xpath('//ul[@title="Pagination"]/li//a[@rel="next"]/@href').get()
        #next_page_link = pagination.xpath('//a[@rel="next"]/@href')
        print(pagination)
        url = f'https://www.realtor.com{pagination}'
        yield Request(client.scrapyGet(url), self.parse, meta={'num_items':num_items})
        # yield Request(url=url, callback=self.parse)

    def parse_property(self, response):
        #print(response.body)
        item = response.meta['item']

        year_built_full = response.xpath('//li[contains(text(), "Year Built:")]/text()').get()
        
        if year_built_full:
            item['year_built'] = year_built_full.split(": ")[1]

        #print(f'Year built {year_built_value}')
        property_tax_rows = response.xpath('//div[@id="ldp-history-taxes"]/div/div/table/tbody/tr')
        
        for property_tax_row in property_tax_rows[:5]:
            property_taxes_td = property_tax_row.xpath('.//td/text()').getall()
            year = property_taxes_td[0]
            taxes = property_taxes_td[1]
            #print(f'property tax {property_tax}')
            #print(f'year {year}')
            #print(f'taxes {taxes}')

            if taxes and int(year) >= 2015:
                item[f'taxes_{year}'] = taxes.replace(',','')[1:]
            

        garage_full = response.xpath('//li[contains(text(), "Garage Spaces:")]/text()').get()
        
        if garage_full:
            item['garage_space'] = garage_full.split(": ")[1]

        property_type = response.xpath('//li[@data-label="property-type"]/div[@class="key-fact-data ellipsis"]/text()').get()

        item['property_type'] = property_type

        description = response.xpath('//p[@id = "ldp-detail-romance"]/text()').getall()

        item['description'] = description

        yield item






    # Find the total number of pages in the result so that we can decide how many pages to scrape next
        # print(response.body)
    #     print(response.xpath('//span[@data-testid="results-header-count"]/text()').extract_first())
    #     text = response.xpath('//span[@data-testid="results-header-count"]/text()').extract_first()
    #     print(text)
    #     number_pages = response.xpath('//ul[@title="Pagination"]/li/a/text()')[-1]
    #     print(number_pages)
    #     total = map(lambda x: int(x), re.findall('\d+', text))
    #     #number_pages = total // per_page

    #     # List comprehension to construct all the urls
    #     result_urls = response.xpath('//ul[@title="Pagination"]/li/a/@href')
    #     for url in result_urls:
    #         print(url.get())
    #     print(f'Results: {result_urls}')


    #     #result_urls = ['https://www.realtor.com/realestateandhomes-search/San-Francisco_CA'.format(x) for x in range(1,number_pages+1)]
    #     # Yield the requests to different search result urls, 
    #     # using parse_result_page function to parse the response.
    #     for url in result_urls[:2]:
    #         yield Request(url=url.get(), callback=self.parse_result_page)

    # def parse_result_page(self, response):
    #     # This function parses the search result page.
    #     # We are looking for url of the detail page.
    #     detail_urls = response.xpath('//li[@class="component_property-card"]/').extract()
    #     print(len(detail_urls))

    def errback(self, failure):
        print(f'FAILURE: {failure}')
