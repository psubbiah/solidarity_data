# Textual Analysis of UK Solidarity with the Bolivarian Revolution

Created by P. Subbiah in 2020.

## Introduction
Textual Analysis insights into groups supportive of the Maduro Government in Venezuela.

The Hands off Venezuela Campaign, is a campaign that, in their own words, stand: 
- “in solidarity with the Bolivarian Revolution”; 
- are “opposed to imperialist intervention in Venezuela”
- are “building direct links with the revolutionary trade unions in Venezuela.”

In this project, I analyse the discourse of ‘solidarity’ by documenting the online output of the Hands Off Venezuela Campaign, and using several computational methods.

## Data

The data used for the analysis was scraped by building a custom ‘Scrapy spider’, which you can find under the folder “solidarity.” The spider scraped all the articles published on the Hands Off Venezuela website, that date back to 2003. The spider is written in Python, and you can run the spider by running the following commands:

First install Scrapy

	pip install Scrapy 

After moving the solidarity spider to your desired folder, from that folder, run: 

	scrapy crawl solidarity - o solidarity.csv 

To save the output of the spider onto a csv file, that you will then use to run the analyses.



hov\_csv2.csv is comprised of data scraped off of the handsoffvenezuela.org webpage. It includes all the articles written on the Hands off Venezuela Page.

hov\_sentiments.R is the code written for sentiment analysis using NRC lexicon, on the HOV articles. 
hov\_LDA.R: Topic Modeling of the text data with LDA and RAKE algorithms. 
