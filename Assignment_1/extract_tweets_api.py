import twitter
import tokens_twitt
import pandas as pd
from time import sleep

columns = ['screen_name','text', 'name','listed_count']
df = pd.DataFrame(columns=columns)

api = twitter.Api(consumer_key=tokens_twitt.CONSUMER_KEY,
  consumer_secret=tokens_twitt.CONSUMER_SECRET,
    access_token_key=tokens_twitt.ACCESS_TOKEN,
    access_token_secret=tokens_twitt.ACCESS_SECRET)

count = 0

while(count < 5000):
  tweets = api.GetSearch(term='winter olympics', since=2018-1-15, count=100)
  for idx,line in enumerate(tweets):
    s = twitter.models.Status.AsDict(line)
    try:
        lc = s['user']['listed_count']
    except:
        lc = 0
    df.loc[count+idx] = [s['user']['screen_name'],s['text'],s['user']['name'],lc]

  count = len(df)
  print "Tweets processed: " + str(count)
  if(count<18000):
      sleep(60)
  else:
      sleep(2000)


df.to_csv("raw_tweets.csv",index=False,encoding='utf-8')
