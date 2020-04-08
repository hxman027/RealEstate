import os
import geopandas as gpd
import pandas as pd

shapes = gpd.read_file(os.path.join("..", "..", "data", "Assessment", "addresses_points.shp"), rows=100)

new_data = []
for pt in shapes.values:
    address = pt[1].replace("\"", "")
    latitude = pt[2].y
    longitude = pt[2].x
    new_data.append([address, latitude, longitude])

df = pd.DataFrame(new_data)
df.to_csv(os.path.join("..", "..", "data", "Assessment", "addresses_points.csv"))