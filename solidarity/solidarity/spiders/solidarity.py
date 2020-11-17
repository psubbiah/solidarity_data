# -*- coding: utf-8 -*-
"""
Created Mon 25 March 2019
@author: Ale
"""

import scrapy
from scrapy import Request

class SolidaritySpider(scrapy.Spider):
    name = 'solidarity'
    start_urls = ['https://www.handsoffvenezuela.org/solidarity/']
    user_agent = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/73.0.3683.75 Safari/537.36'

    def parse(self, response):
        articles = response.xpath('//h1[@class="title"]')

        for article in articles:
            title = article.xpath('a/text()').extract_first()
            relative_url = article.xpath('a/@href').extract_first()
            absolute_url = response.urljoin(relative_url)

            yield Request(absolute_url, callback=self.parse_page, meta={'URL': absolute_url, 'Title': title})

        relative_next_url = response.xpath('//*[@class="pagination-next"]/a/@href').extract_first()
        absolute_next_url = "https://www.handsoffvenezuela.org" + relative_next_url

        yield Request(absolute_next_url, callback=self.parse)

    def parse_page(self, response):
        url = response.meta.get('URL')
        title = response.meta.get('Title')
        date = response.xpath('//dd[@class="rt-date-posted"]/text()').extract_first()
        author = response.xpath('//dd[@class="rt-author"]/text()').extract_first()
        #images = response.xpath('//div[@id="rt-mainbody"]//img/@src')
        text = " ".join(line for line in response.xpath('//*[@id="rt-mainbody"]/div/article/p//text()').extract())
        post_script = " ".join(line for line in response.xpath('//b/text()').extract())

        #for i in images:
        #    if images[i].startswith('http'):
        #        image_url = images[i].extract_first()
        #    else:
        #        image_url = 'https://www.handsoffvenezuela.org' + image[i].extract_first() #for images stored within handsoffvenezuela

        yield {'URL': url, 'Title': title, 'Date': date, 'Author': author, 'Text': text, 'PS': post_script}
