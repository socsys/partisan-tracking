from __future__ import absolute_import
import os
#os.chdir("./OpenWPM/")
print os.getcwd()

from six.moves import range
from automation import CommandSequence, TaskManager
import pandas as pd
#from itertools import permutations
import random
import numpy as np
import time

#creating sets for url profiles.
#Take unique at the end
global_path=".."
def get_profiles():
    all_sites=pd.read_csv("FileUrlLabels.csv")
    all_sites
#    print all_sites
    category=all_sites.Category.unique()

    profile=[]
    for i in all_sites.Type[all_sites.Category.str.contains(category[0])].unique():
        profile.append(i)
        for j in all_sites.Type[all_sites.Category.str.contains(category[1])].unique():
            profile.append(j)
            profile.append(i+"_"+j)
            for k in all_sites.Type[all_sites.Category.str.contains(category[2])].unique():
                profile.append(k)
                profile.append(j+"_"+k)
                profile.append(i+"_"+k)
                profile.append(i+"_"+j+"_"+k)
    profile=pd.Series(profile)
    profile=profile[(profile.str.count("_")==2) | (profile.str.count("_")==0)].unique()
    profile=pd.Series(profile).unique()
    return profile

def profile_crawl():
    profile=get_profiles()
    all_sites=pd.read_csv("../url_labels.csv")
    #taking unique sets and appending sites and doing crawl
    for i in profile[10:12]:
        urls_type=i.rsplit("_")
        sites=pd.Series()
        print i
        for j in urls_type:
            url_sites=all_sites[all_sites.Type.str.contains(j)][0:10]
            sites = sites.append(url_sites)  
        sites= sites.Url.unique()
        random.shuffle(sites)
        dump_crawl(sites,profile_name=i+"/")
        time.sleep(10)
	print len(sites)


def dump_crawl(sites,profile_name):
    #os.system('sudo sh -c "sync; echo 1 > /proc/sys/vm/drop_caches"')
    # The list of sites that we wish to crawl
    print sites,profile_name
    NUM_BROWSERS = 1 #3
    # Loads the manager preference and 3 copies of the default browser dictionaries
    manager_params, browser_params = TaskManager.load_default_params(NUM_BROWSERS)

    # Update browser configuration (use this for per-browser settings)
    for i in range(NUM_BROWSERS):
        # Record HTTP Requests and Responses
        #browser_params[i]['http_instrument'] = True
        # Enable flash for all three browsers
        browser_params[i]['disable_flash'] = True
        browser_params[i]['headless'] = True  # Launch all and not only browser 0 headless
        browser_params[i]['js_instrument'] = True
#        browser_params[i]['save_javascript'] = True
        #browser_params[i]['random_attributes']=True
        browser_params[i]['cookie_instrument']=True
     #   browser_params[i]['cp_instrument']=True
#        browser_params[i]['save_all_content']=True
        if 'load_name' in locals():
            browser_params[i]['profile_tar']=load_name
        browser_params[i]['profile_archive_dir']="/home/ubuntu/personas/"+profile_name
        
    # Update TaskManager configuration (use this for crawl-wide settings)
    manager_params['data_directory'] = '~/OpenWPM/'
    manager_params['log_directory'] = '~/OpenWPM/'
    manager_params['database_name']= "persona.sqlite"

   

    # Instantiates the measurement platform
    # Commands time out by default after 60 seconds
    manager = TaskManager.TaskManager(manager_params, browser_params)

    # Visits the sites with all browsers simultaneously
    for i in range(0,len(sites)):
        print sites[i]
        site=sites[i]    
        command_sequence = CommandSequence.CommandSequence(site)
        # Start by visiting the page
        command_sequence.get(sleep=0, timeout=300)
        # index='**' synchronizes visits between the three browsers
        #command_sequence.dump_profile_cookies(120)
    	#command_sequence.dump_profile(dump_folder="~/personas/", close_webdriver=True)
	manager.execute_command_sequence(command_sequence,(i%NUM_BROWSERS))
	time.sleep(2)
    # dump_profile_cookies/dump_flash_cookies closes the current tab.
    # dump stores history last cookies/sites only stored 
    #    os.system('sudo sh -c "sync; echo 1 > /proc/sys/vm/drop_caches"')
    #command_sequence.dump_profile_cookies(120)
    #command_sequence.dump_profile(dump_folder="~/personas/"+profile_name, closer_webdriver=True, compress, timeout)
	# Shuts down the browsers and waits for the data to finish logging
    manager.close()

profile_crawl()
