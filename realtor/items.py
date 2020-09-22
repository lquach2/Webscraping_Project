# -*- coding: utf-8 -*-

# Define here the models for your scraped items
#
# See documentation in:
# https://doc.scrapy.org/en/latest/topics/items.html

import scrapy


class RealtorItem(scrapy.Item):
    # define the fields for your item here like:
    price = scrapy.Field()
    bed = scrapy.Field()
    bath = scrapy.Field()
    sqft = scrapy.Field()
    #sqftlot = scrapy.Field()
    address = scrapy.Field()
    city = scrapy.Field()
    state = scrapy.Field()
    zip = scrapy.Field()
    area = scrapy.Field()
    year_built = scrapy.Field()
    taxes_2019 = scrapy.Field()
    taxes_2018 = scrapy.Field()
    taxes_2017 = scrapy.Field()
    taxes_2016 = scrapy.Field()
    taxes_2015 = scrapy.Field()
    garage_space = scrapy.Field()
    property_type = scrapy.Field()
    description = scrapy.Field()