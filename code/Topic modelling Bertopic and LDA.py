# Data Preprocessing


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



# Lemmatision
import stanza
stanza.download('en') 
nlp = stanza.Pipeline('en')  


def lemmatize_text(text, nlp):
    doc = nlp(text)
    lemmas = [word.lemma for sentence in doc.sentences for word in sentence.words if word.lemma]
    return ' '.join(lemmas)


lemmatize_text(doc1, nlp)


from tqdm import tqdm

tqdm.pandas()

documents['preprocessed_text'] = documents['cleaned_text'].progress_apply(lambda x: lemmatize_text(x, nlp))


documents.to_csv('vax_tweets_lemmatised_text_4.csv')


# Topic Modelling: BERTopic


data = pd.read_csv('data.csv', error_bad_lines=False)
# remove duplicates
documents = data[['preprocessed_text']]
documents = documents.drop_duplicates()

tweets = documents['preprocessed_text'].tolist()



# Remove numbers, but not words that contain numbers.
cleaned_tweets = []
for tweet in tweets:
    if isinstance(tweet, str):
        new_tweet = ' '.join([token for token in tweet.split() if not token.isdigit()])
        cleaned_tweets.append(new_tweet)



from bertopic.representation import KeyBERTInspired
# Fine-tune your topic representations
representation_model = KeyBERTInspired()
topic_model = BERTopic(representation_model=representation_model, language = 'multilingual', verbose = True)


topics, probs = topic_model.fit_transform(cleaned_tweets)


# The first run of model generated 1000 topics. Reduce nr_topics.
topic_model.reduce_topics(cleaned_tweets, nr_topics=12)

# save tweet and topic distribution
df = topic_model.get_document_info(cleaned_tweets)
df.to_csv('BERTopic_WI_10.csv')



topic_model.visualize_topics()

# Results are not good: Topics heavily overlapped; Not meaningful topics




# Topic Modeling: Dynamic LDA

import pandas as pd

data = pd.read_csv('vax_tweets_lemmatised_text_4.csv', error_bad_lines=False)
data1 = pd.read_csv('vax_tweets_with_sentiment_entities_3.csv', error_bad_lines=False)
date = data1['date'] # get the date info to see how topics change over time
data['date'] = date


# remove duplicates
documents = data[['preprocessed_text','date']]
documents = documents.drop_duplicates()


tweets = documents['preprocessed_text'].tolist()

# Remove numbers, but not words that contain numbers.
def remove_numbers(tweet):
    if isinstance(tweet, str):
        new_tweet = ' '.join([token for token in tweet.split() if not token.isdigit()])
        return new_tweet
    else:
        return ''

documents['preprocessed_text'] = documents['preprocessed_text'].apply(remove_numbers)



# Remove some words we identified in the first run of the model. Some geo terms and highly distributed terms 

stop = ['vaccine', 'vaccinate', 'vaccination', 'covid', 'get', 'do', 
        'go', 'take', 'like', 'say', 'amp', 'am', 'pm', 'cv', 'md', 'el', 'dr', 'st', 'still', 'via']

def remove_stop (text):
    words = [token for token in text.split() if token not in stop]
    new_text = ' '.join(words)
    return new_text


documents['preprocessed_text'] = documents['preprocessed_text'].apply(remove_stop)



from gensim.utils import simple_preprocess
from gensim import corpora, matutils
import numpy as np
import pandas as pd
import lda

# Convert preprocessed text column to a list of tokenized documents
tokenized_docs = [simple_preprocess(doc) for doc in documents['preprocessed_text']]

# Create dictionary and corpus
dictionary = corpora.Dictionary(tokenized_docs)
# Filter out words that occur less than 20 documents, or more than 50% of the documents.
dictionary.filter_extremes(no_below=20, no_above=0.5)
corpus = [dictionary.doc2bow(doc) for doc in tokenized_docs]

# Convert corpus to document-term matrix (DTM)
dtm = matutils.corpus2csc(corpus).T.astype(int)  # Convert to integer sparse matrix


dates = pd.to_datetime(documents['date']).tolist()


# Instantiate and train the DTM model
dtm_model = lda.LDA(n_topics=12, n_iter=1000, random_state=0)
dtm_model.fit(dtm)

# Extract topic distributions for each document
document_topics = dtm_model.doc_topic_

# Assign most dominant topic for each document
dominant_topics = np.argmax(document_topics, axis=1)

# Add the topic distributions and dominant topics to the DataFrame
documents['topic_distribution'] = document_topics.tolist()
documents['dominant_topic'] = dominant_topics

# Save the DataFrame to a new CSV file
documents.to_csv('Dynamic_LDA.csv', index=False)



# Get the top 30 relevant terms for each topic
top_terms = dtm_model.topic_word_

# Create a DataFrame for the top terms
top_terms_df = pd.DataFrame(columns=['Topic', 'Top Terms'])


for topic_idx, topic_terms in enumerate(top_terms):
    terms = [dictionary[i] for i in np.argsort(topic_terms)[:-31:-1]]
    topic_name = f'Topic {topic_idx + 1}'
    top_terms_df = top_terms_df.append({'Topic': topic_name, 'Top Terms': ', '.join(terms)}, ignore_index=True)


top_terms_df.to_csv('top_terms.csv', index=False)


# Plotting: no. of topics change over month

documents['date'] = pd.to_datetime(documents['date'])
documents['month'] = documents['date'].dt.to_period('M')
documents.head()


topic_counts = documents.groupby(['month', 'dominant_topic']).size().reset_index(name='count')
import matplotlib.pyplot as plt


topic_counts['month'] = topic_counts['month'].astype(str)

fig, ax = plt.subplots(figsize=(18, 6))
for topic in topic_counts['dominant_topic'].unique():
    topic_data = topic_counts[topic_counts['dominant_topic'] == topic]
    ax.plot(topic_data['month'], topic_data['count'], label=f'Topic {topic}')

ax.set_xlabel('Date')
ax.set_ylabel('Topic Number')
ax.legend()
plt.xticks(rotation=45)
plt.show()






