from __future__ import absolute_import

from six.moves import range

from automation import CommandSequence, TaskManager

import pandas as pd
import os

# The list of sites that we wish to crawl
NUM_BROWSERS = 1

#read HPWs 
sites= "http://"+pd.read_csv("FileHPWsAlexaRankCookies.csv").site.iloc[::-1]

print sites

# Loads the manager preference and 3 copies of the default browser dictionaries
manager_params, browser_params = TaskManager.load_default_params(NUM_BROWSERS)

# Update browser configuration (use this for per-browser settings)
for i in range(NUM_BROWSERS):
    # Record HTTP Requests and Responses
    browser_params[i]['http_instrument'] = True
    # Enable flash for all three browsers
    browser_params[i]['disable_flash'] = True
    browser_params[i]['headless'] = True  # Launch only browser 0 headless
    browser_params[i]['js_instrument'] = True
    browser_params[i]['save_javascript'] = True
    #browser_params[i]['random_attributes']=True
    browser_params[i]['cookie_instrument']=True
    #browser_params[i]['cp_instrument']=True
    #browser_params[i]['save_all_content']=True
    #browser_params[i]['profile_archive_dir']='WSC'
    #browser_params[i]['profile_tar']='../personas/Seniors_California_Men/'

# Update TaskManager configuration (use this for crawl-wide settings)
manager_params['data_directory'] = '~/OpenWPM'
manager_params['log_directory'] = '~/OpenWPM'
manager_params['database_name']= "AWS.sqlite"

# Instantiates the measurement platform
# Commands time out by default after 60 seconds
manager = TaskManager.TaskManager(manager_params, browser_params)

# Visits the sites with all browsers simultaneously
for site in sites:
    command_sequence = CommandSequence.CommandSequence(site,reset=True)

    # Start by visiting the page
    command_sequence.get(sleep=2, timeout=360)

    # dump_profile_cookies/dump_flash_cookies closes the current tab.
    command_sequence.dump_profile_cookies(120)

    # index='**' synchronizes visits between the three browsers
    manager.execute_command_sequence(command_sequence, 0)
    #os.system('sudo sh -c "sync; echo 1 > /proc/sys/vm/drop_caches"')
# Shuts down the browsers and waits for the data to finish logging
manager.close()
