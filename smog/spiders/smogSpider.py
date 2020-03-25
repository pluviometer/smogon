# -*- coding: utf-8 -*-
import re
import scrapy
from smog.items import SmogItem

main_url = "https://www.smogon.com/forums/threads/rupl-player-signups.3660558/"

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
                item['link'] = sel.xpath(".//div[@class = 'message-attribution-main']/a/@href").extract()
                item['time'] = sel.xpath(".//div[@class = 'message-attribution-main']/a/time/@title").extract()
                item['time2'] = sel.xpath(".//a/time[@class = 'u-dt']/@datetime").extract()[::2]
                item['user'] = sel.xpath(".//@data-author").extract()
                item['text'] = sel.xpath(".//div[@class = 'bbWrapper']//text()").extract()
                item['likes'] = sel.xpath(".//a[@class = 'reactionsBar-link']/text()[3]").re(r'(\d+)')     
                yield item
            
            url = main_url + "page-" + str(self.i + 1) 

            yield scrapy.Request(url, callback = self.parse)
        self.i += 1
