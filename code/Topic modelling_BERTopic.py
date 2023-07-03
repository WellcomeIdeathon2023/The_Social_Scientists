
import pandas as pd
from bertopic import BERTopic


# read data from corpus
data = pd.read_csv('vax_tweets_with_sentiment_entities_3.csv', error_bad_lines=False)
documents= data[['cleaned_text']] 
documents = documents.drop_duplicates() # remove duplicated tweets
tweets = documents['cleaned_text'].tolist()

# Fine-tune your topic representations
from bertopic.representation import KeyBERTInspired
representation_model = KeyBERTInspired()

# Train model
topic_model = BERTopic(representation_model=representation_model, language = 'multilingual', verbose = True)
topics, probs = topic_model.fit_transform(tweets)


# Reduce no. of topics to 30
topic_model.reduce_topics(tweets, nr_topics=30)



df = topic_model.get_document_info(tweets) # Get all of the document info with allocated topics
df.to_csv('deduplicated_tweets_with_topics_4.csv')





