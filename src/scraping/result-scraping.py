import numpy as np
import pandas as pd
import re

def extract_league_data(url):
    '''Extract the following information from the URL: nation, league, season.
    '''
    # Extract nation 
    m = re.search(r'/((eng)|(esp))-', url)
    if not m:
        nation = 'UEFA'
    elif m.group(1) == 'eng':
        nation = 'England'
    else:
        nation = 'Spain'

    # Extract league 
    league_names_map = {
        'champions-league': 'champions-league',
        'championship': 'championship',
        'premier-league': 'premier-league',
        'league-one': 'league-one',
        'league-two': 'league-two',
        'national-league': 'national-league',
        'fa-cup': 'fa-cup',
        'league-cup': 'efl-cup',
        'copa-del-rey': 'copa-del-rey',
        'primera-division': 'primera',
        'segunda': 'segunda',
        'primera-aufstieg': 'segunda',
    }
    for name in league_names_map:
        if name in url:
            league = league_names_map[name]
            break

    # Extract season
    season = re.search(r'\d{2}(\d{2})-\d{2}(\d{2})', url)
    season = '/'.join([season.group(1), season.group(2)])
    return pd.DataFrame({'nation': [nation], 'league': [league], 'season': [season]})

def extract_match_data(url):
    '''Extract match data from the page
    '''
    df = pd.read_html(url)[1]
    df.columns = ['date', 'time', 'home', 
                  'dash', 'away', 'score', 'NA']
    
    # Remove columns with no data
    df.drop(columns=['dash', 'NA'], inplace=True)

    # Forward fill Date columns, and drop rows that are not dates
    df.date = df.date.ffill() 
    df.date = pd.to_datetime(df.date, format='%d/%m/%Y', errors='coerce')
    df.dropna(axis='index', subset=['date'], inplace=True)

    # Split 'score' column to 'HG' (home goal) and 'AG' (away goal) columns
    df.score = df.score.str.split(expand=True)
    goals = df.score.str.split(':', expand=True)
    df['HG'], df['AG'] = goals.iloc[:, 0], goals.iloc[:, 1]
    df.drop(columns=['score'], inplace=True)

    # Create 'outcome' column
    df['outcome'] = np.select([df.HG>df.AG, df.HG<df.AG, df.HG == df.AG],
                              ['H', 'A', 'D'])
    return df

def extract_data(url):
    '''Extract data from the given URL
    '''
    print('Scraping: ' + url)
    league_data = extract_league_data(url)
    league_data['key'] = 1
    match_data = extract_match_data(url)
    match_data['key'] = 1
    return league_data.merge(match_data, on = 'key').drop(columns = ['key'])

if __name__ == "__main__":
    with open('urls.txt', 'r') as f:
        urls = f.readlines()
    urls = [url.strip() for url in urls]
    data = pd.concat(map(extract_data, urls), ignore_index=True)
    data['match_id'] = range(1, len(data)+1)
    
    data.to_csv('match_result.csv', index=False)