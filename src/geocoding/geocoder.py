import os
from geopy import geocoders
from osgeo import ogr, osr

def geocode(address):
    g = geocoders.Nominatim(user_agent="myGeocoder")
    place, (lat, lng) = g.geocode(address)
    return place, lat, lng

def parse_file(filepath, output_shape):
    # create the shapefile
    drv = ogr.GetDriverByName("ESRI Shapefile")
    if os.path.exists(output_shape):
        drv.DeleteDataSource(output_shape)
    ds = drv.CreateDataSource(output_shape)
    # spatial reference
    sr = osr.SpatialReference()
    sr.ImportFromProj4('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
    lyr = ds.CreateLayer(output_shape, sr, ogr.wkbPoint)
    # fields
    featDefn = lyr.GetLayerDefn()
    fld_id = ogr.FieldDefn('id', ogr.OFTInteger)
    fld_address = ogr.FieldDefn('ADDRESS', ogr.OFTString)
    fld_address.SetWidth(255)
    lyr.CreateField(fld_id)
    lyr.CreateField(fld_address)
    # read text addresses file
    i = 0
    f = open(filepath, 'r')
    for address in f:
        try:
            address = address.replace("\"", "")
            print("Trying address " + address)
            place, lat, lng = geocode(address)
            point = ogr.Geometry(ogr.wkbPoint)
            point.SetPoint(0, lng, lat)
            feat = ogr.Feature(lyr.GetLayerDefn())
            feat.SetGeometry(point)
            feat.SetField('id', i)
            feat.SetField('ADDRESS', address)
            lyr.CreateFeature(feat)
            feat.Destroy()
            i = i + 1
            print("Done address " + address)
        except:
            print('Error, skipping address...' + address)

parse_file(os.path.join("..", "..", "data","Assessment", "addresses.csv"),
           os.path.join("..", "..", "data", "Assessment", "addresses_points.shp"))