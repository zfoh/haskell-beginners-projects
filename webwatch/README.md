# Webwatch

## Introduction

This is a demo beginners project for ZuriHac 2016. It watches a certain webpage
for links with keywords (e.g. search http://news.ycombinator.com for "Haskell").
When such a link is found, it sends you a slack message. See the
[codelab](codelab/webwatch_codelab.md) for more details.

## Code layout

The code is organised in three modules.

- `WebWatch.Links`: Scrapes a web page and pulls out links matching given
  keywords
- `WebWatch.Slack`: Performs a HTTP POST request to a Slack webhook
- `Main.hs`: Main module, reads the configuration file and then runs the
  scraper/chatbot at a configurable interval

# Libraries

The implementation uses the following libraries:

- `http-client` for downloading a web page and sending a POST
- `tagsoup` for parsing the web page and getting out the links
- `network-uri` to deal with relative/absolute URIs
- `configurator` to read the configuration file
