# Curve fitting in Python using the Assayfit Pro service
# https://colab.research.google.com/drive/1h-UKkSdTGVs7oceZTz-gy71_jaN_Bakb
# free to use for limited number of fits without registration
# in Colab click start symbol on the left to run
import html
import requests
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import sys
from io import StringIO


#set input data you can modify this section with your data
xdata = "1;2;3;4;5"
ydata = "1.0;3.5;5.0;6.0;5.5"
yknown = "4;3"


#GET request use this as alternative, it can use a maximum of 2000 characters
#url = 'https://service1.assayfit.com/service.svc/http/assayfitget?email=pythoncolab@assaycloud.com&key=free&xdata=1;2;3;4;5&ydata=1.0;3.5;5.0;6.0;6.5&yknown=4'
#url = "https://service1.assayfit.com/service.svc/http/assayfitget?email=pythoncolab@assaycloud.com&key=free&" + xdata + "&ydata=" + ydata + "&yknown=" + yknown
#getresponse = requests.get(url)


#POST request, no size restriction
url = 'https://service1.assayfit.com/Service.svc/http/Assayfitpostcsv'
headers = {'Content-type': 'application/json'}
body = '"email=pythoncolab@assaycloud.com&key=free&xdata=' + xdata + '&ydata=' + ydata + '&yknown=' + yknown +'"'
postresponse = requests.post(url, data=body, headers=headers)

if len(postresponse.text) < 300:
    #in case of error print error
    print(html.unescape(postresponse.text))
else:
    #fit data is returned in CSV format
    responsedata = StringIO(html.unescape(postresponse.text))

    header_row = ['xdata' ,'ydata', 'weights', 'percent', 'yfitted','resid','paraminfo', 'param', 'yknown', 'xfromyknown', 'xknown', 'yfromxknown', 'xcurve', 'ycurve', 'infotitle', 'info']
    df = pd.read_csv(responsedata, sep=";", header=None)
    xdata = np.array(df[0])
    ydata = np.array(df[1])
    xcurve = np.array(df[12])
    ycurve = np.array(df[13])
    params = np.array(df[5])
    xfromyknown = np.array(df[9])
    yknown = np.array(df[8])
    #change here if grapth needs to be on log scale
    #plt.yscale('log')
    #plt.xscale('log')
    plt.xlabel('xdata')
    plt.ylabel('ydata')
    plt.title('Calibration curve')
    plt.scatter(xdata, ydata, color='black', marker='+',s=80, linewidth=1)
    plt.scatter(xfromyknown, yknown, color='green', marker='*')
    plt.plot( xcurve, ycurve, color='orange', linewidth=0.5)
    plt.show()
 
    df.columns = header_row
    string = df.to_string(index=False)
    string = string.replace("NaN", "")
    print(string)



  












