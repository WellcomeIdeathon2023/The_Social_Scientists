import nltk
import pandas as pd

df = pd.read_csv("./data/vax_tweets_cleaned_with_sentiment_2.csv",index_col=0)

# Download the NER model if not already downloaded
nltk.download('maxent_ne_chunker')
nltk.download('words')
nltk.download('punkt')
nltk.download('averaged_perceptron_tagger')


def perform_ner(text_vector):
    # Tokenize each tweet into sentences
    tokenized_tweets = [nltk.sent_tokenize(tweet) for tweet in text_vector]
    
    # Tokenize each sentence into words
    tokenized_sentences = [[nltk.word_tokenize(sentence) for sentence in tweet] for tweet in tokenized_tweets]
    
    # Apply part-of-speech (POS) tagging
    pos_tagged_sentences = [[nltk.pos_tag(sentence) for sentence in tweet] for tweet in tokenized_sentences]
    
    # Perform named entity recognition
    chunked_sentences = [nltk.ne_chunk_sents(pos_tagged_tweet, binary=True) for pos_tagged_tweet in pos_tagged_sentences]
    
    # Extract named entities
    named_entities = []
    for chunked_tweet in chunked_sentences:
        tweet_entities = []
        for tree in chunked_tweet:
            for entity in tree:
                if hasattr(entity, 'label') and entity.label() == 'NE':
                    tweet_entities.append(' '.join([leaf[0] for leaf in entity.leaves()]))
        named_entities.append(tweet_entities)
    
    return named_entities

entities = perform_ner(df['text'])

# Add entities to dataframe
df["entities"] = entities


df.to_csv("./data/vax_tweets_with_sentiment_entities_3.csv")
