import pandas as pd
import numpy as np
from textblob import TextBlob
import ast

# Try to convert string to list
def convert_string_to_list(s):
    try:
        return ast.literal_eval(s)
    except (ValueError, SyntaxError):
        return []

# Clean dataframe
def clean_data(df):

    # Specific users have columns in messed up order
    for ii in [4225,34381,44732]:
        df.loc[ii,"is_retweet"] = False
        df.loc[ii,"hashtags"] = df.loc[ii,"date"]
        df.loc[ii,"text"] = df.loc[ii,"user_verified"]
        df.loc[ii,"date"] = df.loc[ii,"user_favourites"]
        df.loc[ii,"user_verified"] = False
        df.loc[ii,"user_favourites"] = df.loc[ii,"user_followers"]
        df.loc[ii,"user_followers"] = 0
        df.loc[ii,"user_friends"] = 0
        df.loc[ii,"user_location"] = ""

    # Clean data
    df["user_location"] = df["user_location"].fillna("")
    df["user_description"] = df["user_description"].fillna("")
    df["user_followers"] = df["user_followers"].fillna(0).astype(int)
    df["user_friends"] = df["user_friends"].fillna(0).astype(int)
    df["user_favourites"] = df["user_favourites"].fillna(0).astype(int)
    df["user_verified"] = df["user_verified"].apply(lambda x: True if x=="TRUE" else x)
    df["user_verified"] = df["user_verified"].apply(lambda x: False if x=="FALSE" else x)
    df["user_verified"] = df["user_verified"].astype(bool)
    df['text'] = df['text'].astype(str)
    df['date'] = pd.to_datetime(df['date'], format="mixed")
    df['hashtags'] = df['hashtags'].apply(convert_string_to_list)
    del(df["is_retweet"])

    # There's some missing dates (12) which we eliminate
    df = df.dropna(subset=['date'])

    return df

# Define a function to apply sentiment analysis to the text
def get_polarity(text):
    blob = TextBlob(text)
    return blob.sentiment.polarity
def get_subjectivity(text):
    blob = TextBlob(text)
    return blob.sentiment.subjectivity

# Takes dataframe and create sentiments for description and text
# (This takes a couple of minutes)
def create_sentiments(df):
    df['polarity_text'] = df['text'].apply(get_polarity)
    df['subjectivity_text'] = df['text'].apply(get_subjectivity)
    df['polarity_description'] = df['user_description'].apply(get_polarity)
    df['subjectivity_description'] = df['user_description'].apply(get_subjectivity)
    return df

if __name__=="__main__":

    # Open data
    df = pd.read_csv("./data/vax_tweets.csv",index_col=0)

    # Clean it
    df = clean_data(df)

    # Get sentiment columns
    df = create_sentiments(df)

    # Save as clean version for quicker use
    df.to_csv("./data/vax_tweets_cleaned_with_sentiment.csv")

    # Load again
    df = pd.read_csv("./data/vax_tweets_cleaned_with_sentiment.csv",index_col=0)


    ## Additional analysis:
    # What are the hashtags associated with the lowest sentiment?
    # 'explode' splits hashtag lists into separate rows
    df_exploded = df.explode('hashtags')

    # Group by 'hashtags' and calculate mean sentiment
    average_sentiments = df_exploded.groupby('hashtags')['polarity_text'].mean()

    # Sort by sentiment and take the first 10
    lowest_sentiment_hashtags = average_sentiments.sort_values(ascending=True).head(10)

    # Print the top 10 hashtags with the lowest average sentiment
    print(lowest_sentiment_hashtags)