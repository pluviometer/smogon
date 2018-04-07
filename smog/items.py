# -*- coding: utf-8 -*-

# Define here the models for your scraped items
#
# See documentation in:
# http://doc.scrapy.org/en/latest/topics/items.html

import scrapy


class SmogItem(scrapy.Item):
    # define the fields for your item here like:
        num = scrapy.Field()
        link = scrapy.Field()
        time = scrapy.Field()
        time2 = scrapy.Field()
        user = scrapy.Field()
        text = scrapy.Field()
