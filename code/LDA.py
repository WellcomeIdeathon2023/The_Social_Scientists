import pandas as pd

# read data from corpus
data = pd.read_csv('vax_tweets_with_sentiment_entities_3.csv', error_bad_lines=False)
documents= data[['text']] 


import re
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize

def clean_tweet(tweet):
    
    # Remove None objects
    tweet = tweet.replace("None", " ") 
    
    # Remove hashtags
    tweet = re.sub(r'\#\w+', '', tweet)
    
    # Remove emojis
    tweet = re.sub(r'[^\w\s,]', '', tweet)
    
    # Remove URLs
    tweet = re.sub(r'http\S+|www\S+', '', tweet)
    
    # Remove mentions (@)
    tweet = re.sub(r'@\w+', '', tweet)
    
    # Convert to lowercase
    tweet = tweet.lower()
    
    # Tokenize tweet into words
    words = word_tokenize(tweet)
    
    # Remove stopwords
    stop_words = set(stopwords.words('english'))
    words = [word for word in words if word.lower() not in stop_words]  
    
    # Join words back into a sentence
    cleaned_tweet = ' '.join(words)
    
    return cleaned_tweet.strip()



documents['cleaned_text'] = documents['text'].apply(clean_tweet)


import stanza
stanza.download('en') 
nlp = stanza.Pipeline('en')  


def lemmatize_text(text, nlp):
    doc = nlp(text)
    lemmas = [word.lemma for sentence in doc.sentences for word in sentence.words if word.lemma]
    return ' '.join(lemmas)



from tqdm import tqdm
tqdm.pandas()

documents['preprocessed_text'] = documents['cleaned_text'].progress_apply(lambda x: lemmatize_text(x, nlp))


# save lemmatisd data for topic modelling
documents.to_csv('data.csv')


# build your corpus for training
tweets = documents['preprocessed_text'].tolist()


# Remove numbers, but not words that contain numbers.
cleaned_tweets = []
for tweet in tweets:
    new_tweet = ' '.join([token for token in tweet.split() if not token.isnumeric()])
    cleaned_tweets.append(new_tweet)


docs = list(set(cleaned_tweets))


# Tokenize the documents.
from nltk.tokenize import RegexpTokenizer

# Split the documents into tokens.
tokenizer = RegexpTokenizer(r'\w+')
for idx in range(len(docs)):
    docs[idx] = docs[idx].lower()  # Convert to lowercase.
    docs[idx] = tokenizer.tokenize(docs[idx])  # Split into words.

# Remove words that are only one character.
docs = [[token for token in doc if len(token) > 1] for doc in docs]


from gensim.models import Phrases

# Add bigrams and trigrams to docs (only ones that appear 20 times or more).
bigram = Phrases(docs, min_count=20)

for idx in range(len(docs)):
    for token in bigram[docs[idx]]:
        if '_' in token:
            # Token is a bigram, add to document.
            docs[idx].append(token)


# Remove rare and common tokens.
from gensim.corpora import Dictionary

# Create a dictionary representation of the documents.
dictionary = Dictionary(docs)

# Filter out words that occur less than 20 documents, or more than 50% of the documents.
dictionary.filter_extremes(no_below=20, no_above=0.5)

# Bag-of-words representation of the documents.
corpus = [dictionary.doc2bow(doc) for doc in docs]

print('Number of unique tokens: %d' % len(dictionary))
print('Number of documents: %d' % len(corpus))


# Train LDA model.
from gensim.models import LdaModel

# Set training parameters.
num_topics = 10
chunksize = 2000 # control how many documents are processed at a time
passes = 20 # how often you train the model on the entire corpus
iterations = 400
eval_every = None  # Don't evaluate model perplexity, takes too much time.

# Make an index to word dictionary.
temp = dictionary[0]  # This is only to "load" the dictionary.
id2word = dictionary.id2token

model = LdaModel(
    corpus=corpus,
    id2word=id2word,
    chunksize=chunksize, 
    alpha='auto',
    eta='auto',
    iterations=iterations,
    num_topics=num_topics,
    passes=passes,
    eval_every=eval_every
)



import pyLDAvis.gensim_models as gensimvis
import pyLDAvis
vis_data = gensimvis.prepare(model, corpus, dictionary)


pyLDAvis.display(vis_data)
pyLDAvis.save_html(vis_data, 'LDA.html')


topic_probabilities = model.get_document_topics(corpus)

data = []
for tweet, topics in zip(cleaned_tweets, topic_probabilities):
    topic = max(topics, key=lambda x: x[1])[0]  # Get the dominant topic for each tweet
    data.append([tweet, topic])

# Create a DataFrame with "tweets" and "topics" columns
df = pd.DataFrame(data, columns=['tweets', 'topics'])

# Save the DataFrame to a CSV file
df.to_csv('tweet_topics.csv', index=False)






