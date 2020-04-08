from geopy.extra.rate_limiter import RateLimiter
from geopy.geocoders import Nominatim
import os
import pandas as pd
import numpy as np

column_names = pd.read_excel(os.path.join("..", "..", "data", "Assessment", "2020 Only.xlsx")).columns
column_names = np.array(column_names)
data = pd.read_csv(os.path.join("..", "..", "data", "Assessment", "2016 - 2020 Raw.csv"), header=None,
                   names=column_names, usecols=['AddressFullAddress'])
data.drop_duplicates(inplace=True)

data.to_csv(os.path.join("..", "..", "data", "Assessment", "addresses.csv"), index=False, header=False)