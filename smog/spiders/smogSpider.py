# -*- coding: utf-8 -*-
import re
import scrapy
from smog.items import SmogItem

starturl = "http://www.smogon.com/forums/threads/official-smogon-tournament-xiv-signups.3625049/"

class smogSpider(scrapy.Spider):
    name = 'spinarak'
    allowed_domains = ["smogon.com"]
    start_urls = [starturl]

    def __init__(self):
        self.i = 1
        self.numPages = 500

    def parse(self, response):    
        if self.i <= self.numPages:
            for sel in response.xpath("//div/article[@class = 'message message--post js-post js-inlineModContainer  ']"):
                item = SmogItem()             
                item['num'] = sel.xpath(".//div[@class = 'message-permalink']/a/text()").re(r'(\d+)')
                item['time'] = sel.xpath(".//header[@class = 'message-attribution']/a[@rel = 'nofollow']/time[@class = 'u-dt']/@title").extract()
                item['user'] = sel.xpath(".//@data-author").extract()
                #item['text'] = sel.xpath(".//div[@class = 'bbWrapper']//text()").extract()
                #item['Spe'] = sel.xpath(".//td[@style = 'background:#FA92B2']/text()").re(r'(\d+)')         
                yield item
            
            url = starturl + "page-" + str(self.i + 1) 
            yield scrapy.Request(url, callback = self.parse)
        self.i += 1
