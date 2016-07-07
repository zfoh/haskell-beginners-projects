# Webwatch

## Introduction

This is a demo beginners project for ZuriHac 2016.  It watches a certain webpage
for links with keywords (e.g. search http://news.ycombinator.com for "Haskell").
When such a link is found, it sends you an email.

## Code layout

The code is organised in three modules.

- `WebWatch.Links`: Scrapes a web page and pulls out links matching given
  keywords
- `WebWatch.Mailer`: Module to send the email
- `Main.hs`: Main module, reads the configuration file and then runs the
  scraper/mailer at a configurable interval

# Libraries

My implementation uses the following libraries:

- `http-conduit` for downloading a web page
- `tagsoup` for parsing the web page and getting out the links
- `network-uri` to deal with relative/absolute URIs
- `amazonka-ses` to send an email
- `configurator` to read the configuration file

## Deployment

I compile a statically linked binary on Linux and drop that on an EC2 message.
