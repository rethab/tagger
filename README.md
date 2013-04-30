# Tagger

## Idea
I often buy physical CDs and consequently copy them to my computer. 
Because quite a lot of CDs lack complete ID3 tag information, I have 
to complete these manually by searching the internet for information. 
The tags I use typically are: Artist, Album, Song, Genre, Release and 
Cover Art. As of today, this program supports all of those except cover art,
but only in my particular library organization (but I'm open to add flexibility), 
which looks like this: Music/Artist/Album/Song.mp3. I use http://en.wikipedia.org/wiki/ABCDE 
for ripping, which can be configured to run a certain script after ripping.

## Features
- Use last.fm webservice for ID3 Tag completion

## Howto
Once installed, run:

``` $ tagger /home/user/Music ```
