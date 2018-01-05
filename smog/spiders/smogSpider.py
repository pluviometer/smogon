# -*- coding: utf-8 -*-
import re
import scrapy
from smog.items import SmogItem

class smogSpider(scrapy.Spider):
    name = 'spinarak'
    allowed_domains = ["smogon.com"]
    main_url = "http://www.smogon.com/forums/threads/smogon-exhibition-signups.3623346/"
    start_urls = [main_url]

    def __init__(self):
        self.i = 1
        self.numPages = 25

    def parse(self, response):    
        if self.i <= self.numPages:
            for sel in response.xpath("//div/article[@class = 'message message--post js-post js-inlineModContainer  ']"):
                item = SmogItem()             
                item['num'] = sel.xpath(".//div[@class = 'message-permalink']/a/text()").re(r'(\d+)')
                item['user'] = sel.xpath(".//@data-author").extract()
                item['text'] = sel.xpath(".//div[@class = 'bbWrapper']//text()").extract()      
                yield item
            
            url = main_url + "page-" + str(self.i + 1) 
            yield scrapy.Request(url, callback = self.parse)
        self.i += 1
