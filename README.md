# Stop tracking me Bro! Differential Tracking of User Demographics on Hyper-Partisan Websites (WWW 2020)

#### We extend an existing crawling tool with our methodology, and open source our framework in order to enable fellow researchers, policy makers or even end-users to audit websites on how they personalize tracking technology based on the visitor’s web profile. These codes and links to the tools used in our WWW 2020 paper are listed below (still needs some clean-up).

#### Note : The dataset generated using these scripts can be requested from our dataset page https://nms.kcl.ac.uk/netsys/datasets/partisan-news/ (Please abide to our listed T&C).

### 1. Methodology (./methodologyCodes)

This includes codes to fetch website lists, setting up OpenWPM tool and some shell scripts to orchestrate the machinery for training personas.

#### a) Required files: Available at https://nms.kcl.ac.uk/netsys/datasets/partisan-news/

1. Websites from Alexa.com (FileUrlLabels.csv): We build personas by visiting top Alexa ranking websites for different demographics (https://www.alexa.com/topsites/category)
2. Hyper-partisan websites (HPWs) list by Buzzfeed (FileHPWsAlexaRankCookies.csv): The list was originally published by Buzzfeed at https://github.com/BuzzFeedNews/2017-08-partisan-sites-and-facebook-pages in their article https://www.buzzfeednews.com/article/craigsilverman/inside-the-partisan-fight-for-your-news-feed. We use and visit these HPW which have been carefully categorized (manually) as left- or right-leaning by domain experts (journalists and fact checkers). In our processed file: FileHPWsAlexaRankCookies.csv, we also add columns such as (a) 'Global rank' of these HPWs as shown by Alexa.com in 2019 and (b) 'cookies counts' which we obtain by visiting these HPWs websites without any user history (i.e. baseline).
3. Disconnect List (FileServicesDisconnectP.csv): We use a list from https://github.com/disconnectme/disconnect-tracking-protection/blob/master/services.json (Feb 2019) to understand the type of cookies set for users. We breakdown the cookies into six types: first-party, advertising, analytics, content, social, and other.
4. WhoTracks.me List (FileWhoTracksMeDomains.csv): We extract the top trackers on the Web using a live list maintained by whotracks.me (https://github.com/cliqz-oss/whotracks.me/tree/master/whotracksme/data/assets) on 25/09/19 and compare this list against the trackers detected in the two groups of websites, when they are visited by a baseline user.

#### b) Machinery Set-up 

1. Install OpenWPM: We make use of OpenWPM (https://github.com/mozilla/OpenWPM), a popular tool for measurements and automating web browsers, which is developed by Princeton University under the Web Transparency & Accountability Project. We install OpenWPM on multiple AWS* instances and generate user personas using following scripts (2 to 4). We discuss additional set-up details in our paper (Section 2.4 Crawling Engine of Framework). One needs to copy the following scripts in the installed OpenWPM folder.

 	2. Build persona (crawl_personas.py): Using this script we build 'stable' personas by consecutively visiting top Alexa ranking websites (listed in FileUrlLabels.csv) for different demographics. The trained personas (.tar) can directly be downloaded from our dataset page and loaded in OpenWPM. This will help in direct testing of demographics on any new websites as discussed in step 4 here.  
 	3. Crawl Baseline (crawl_base.py): We start by crawling HPWs visit data (cookies, HTTPs logs etc.) with no loaded user history and considered this as a baseline (null personas). These crawls are stored as .sqlite files.
 	4. Crawl data (crawl_data.py): We then start crawling HPWs visit data (cookies, HTTPs logs etc.) with a loaded user history (persona) using .tar files created in step 2 here. Note that unlike step 2 of building persona, step 4 which is HPWs crawl with a loaded persona is stateless, i.e., each HPW website visit is independent.
 	5. Shell script (Optional*: ssh_script_run_crawl_all_in_one.sh): Use this to orchestrate your AWS servers. Common methods such as SSH, SCP, cd, ls are implemented across multiple personas stored as array.

​	*Note that installation on AWS is optional. The tool and methodology works wherever OpenWPM can be installed.

### 2. Analysis (./analysisCodes)

We include here some quick codes to analyse the datasets from section 1.

#### a. Read and write (Rreadwrite.R): 

The code has snippets to extracts tables from an "sqlite" file, calculate basic counts from tables and check unique third-party domains.

#### b. Functions used (RFunctions.R)

The code snippets here are used across other R codes as functions. This includes functions like export/import csv, ggplots, check type of cookies, KS test, basic stats, NMF code etc.

#### c. Basic Analysis (Ranalysis.R)

This include codes to perform analysis such as calculation of Z-scores, KS test, websites ranks, Jaccard, and NMF clustering.

#### d. OpenDAMP (Open Digital Advertising Measurement Platform) for RTB model and Cookie Synchronizations

The model is available here https://github.com/panpap/openDAMP

#### e. HPWs' on Alexa.com (Optional, RalexaCodes.R): 

This code compute latest rank of HPWs from Alexa.com and stores in a file.

### Stay tuned for more updates...

```R
Please cite our WWW 2020 papers:

@inproceedings{agarwal2020stop,
  title={Stop Tracking Me Bro! Differential Tracking Of User Demographics On Hyper-partisan Websites},
  author={Agarwal, Pushkal and Joglekar, Sagar and Papadopoulos, Panagiotis and Sastry, Nishanth and Kourtellis, Nicolas},
  booktitle={Proceedings of the 2020 world wide web conference},
  year={2020}
}

```