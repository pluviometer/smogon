# -*- coding: utf-8 -*-
import re
import scrapy
from smog.items import SmogItem

main_url = "https://www.smogon.com/forums/threads/underused-premier-league-vi-player-signups.3631034/"

class smogSpider(scrapy.Spider):
    name = 'spinarak'
    allowed_domains = ["smogon.com"]
    start_urls = [main_url]

    def __init__(self):
        self.i = 1
        self.numPages = 1000

    def parse(self, response):    
        if self.i <= self.numPages:
            for sel in response.xpath("//div/article[@class = 'message message--post js-post js-inlineModContainer  ']"):
                item = SmogItem()             
                item['num'] = sel.xpath(".//div[@class = 'message-permalink']/a/text()").re(r'(\d+)')
                item['link'] = sel.xpath(".//a[@class = 'message-attribution-main u-concealed']/@href").extract()
                item['time'] = sel.xpath(".//header[@class = 'message-attribution']/a[@rel = 'nofollow']/time[@class = 'u-dt']/@title").extract()
                item['time2'] = sel.xpath(".//header[@class = 'message-attribution']/a[@rel = 'nofollow']/time/@datetime").extract()
                item['user'] = sel.xpath(".//@data-author").extract()
                item['text'] = sel.xpath(".//div[@class = 'bbWrapper']//text()").extract()     
                yield item
            
            url = main_url + "page-" + str(self.i + 1) 

            yield scrapy.Request(url, callback = self.parse)
        self.i += 1
