import requests
import json
from tqdm import tqdm
import datetime as dt

import aiohttp
import asyncio
import os

from aiohttp import ClientSession
from urllib.error import HTTPError


ls = []

async def get_book_details_async(i, session):
    """Get book details using Google Books API (asynchronously)"""
    url = f'https://hacker-news.firebaseio.com/v0/item/{i}.json?print=pretty'
    try:
        response = await session.request(method='GET', url=url)
        response.raise_for_status()
    except HTTPError as http_err:
        print(f"HTTP error occurred: {http_err}")
    except Exception as err:
        print(f"An error ocurred: {err}")
    response_json = await response.json()
    return response_json


async def run_program(i, session):
    """Wrapper for running program in an asynchronous manner"""
    try:
        response = await get_book_details_async(i, session)
        ls.append(response)
    except Exception as err:
        print(f"Exception occured: {err}")
        pass

start = dt.datetime.now()

async with ClientSession() as session:
    await asyncio.gather(*[run_program(i, session) for i in range(23200000, 23300000)])

# 24496213, 24400000

end = dt.datetime.now()

print(len(ls))
print(end-start)

dir = os.path.join('C:\\', 'items_12.json')

with open(dir, 'w') as f:
    json.dump(ls, f)
