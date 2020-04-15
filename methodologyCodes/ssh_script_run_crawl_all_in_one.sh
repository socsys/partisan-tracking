#!/bin/bash

#names of users personas exported from training

profile=(
Youth_Nebraska_Women
Youth_Nebraska_Men
Youth_California_Women
Youth_California_Men
Youth_Alabama_Women
Youth_Alabama_Men

Seniors_Nebraska_Women
Seniors_Alabama_Women
Seniors_Alabama_Men
Seniors_Nebraska_Men
Seniors_California_Women
Seniors_California_Men

Seniors
Women
Men
Youth
Nebraska
California
Alabama
)

#renaming array to short names

profile_name=(
		 YNW
YNM
YCW
YCM
YAW
YAM

SNW
SAW
SAM
SNM
SCW
SCM

S
W
M
Y
N
C
A
)

#IP of your AWS account or commment if local
array=(XXX)


#checking AWS accounts and training personas counts.
echo "Number of elements: ${#array[@]}"
echo "Number of elements: ${#profile[@]}"
echo "Number of elements: ${#profile_name[@]}"

#run loop for all profiles
len=${#profile[@]}
#len=1
for ((i=0; i<${len};i++));
do
	#check which server, if using multiple SSH
	echo $((i+1))" : "${array[$i]}
	# ssh -l ubuntu -q "${array[0]}" "${script}"
	#use OpenWPM installed folder
	# script="cd OpenWPM;
	#run python crawling script, pass profile to crawl as parameter
	# python crawl_data.py ${profile[$i]}"
	#copying data and renaming
   	#scp ubuntu@"${array[0]}":./OpenWPM/weather_"${profile[$i]}".sqlite .
	#mv weather_"${profile[$i]}".sqlite ./weather_"${profile_name[$i]}".sqlite

	echo "$script"
done

