PEW RESEARCH CENTER
SOCIAL & DEMOGRAPHIC TRENDS PROJECT
2012 GENDER AND GENERATIONS SURVEY
(November 28-December 5, 2012)

Interviews were conducted in English and Spanish.

National sample: N=2,511

***Note: The WEIGHT variable is weighting variable intended for use on all questions.

***Note for variables: Race of the respondent "Race1" allows multiple responses, 
and "racecmb" in the dataset combines different responses to the race questions.
The combined variable “racethn” identifies the respondent’s race and ethnicity. 

******************************************************************************************************************************************

The Pew Research Center uses respondents’ self-reported zip code as the basis for geographic variables such as region,state and county. 
We continue to include the original sample geographic variables in the datasets (these variables are preceded by an ‘s’)
for archival purposes only.

To protect the privacy of respondents, telephone numbers, zip code and other identifying variables have been removed from
the public data file.

Some special technical variables were also removed, but are available upon request.

******************************************************************************************************************************************

CREATED VARIABLES: 

Please create HISPORIG as follows and include variable in dataset, and backcode Hispanic responses to race question:
compute hisporig=hisp4.
variable label hisporig ‘ORIGINAL HISP. Are you of Hispanic, Latino, or Spanish origin, such as Mexican, Puerto Rican or Cuban?’.
value label hisporig
1 "Yes "
2 "No"
9 "Don't know/Refused (VOL.)".
if race_1=7 or race_2=7 or race_3=7 or race_4=7 hisp4=1.
variable label hisp4 ‘HISP4. Are you of Hispanic, Latino, or Spanish origin, such as Mexican, Puerto Rican or Cuban?—Includes RACE backcodes’.

Please create RACECMB as follows and include variable in dataset:
recode race_1 (1=1) (2=2) (3=3) (4 thru 7=5) (8 thru 9=9) into racecmb.
if race_2>0 and race_2 <8 racecmb=4.
variable label racecmb "Combining Race".
value label racecmb
1 "White"
2 "Black or African-American"
3 "Asian or Asian-American"
4 "Mixed Race"
5 "Or some other race"
9 "Don't know/Refused (VOL.)".

Please create RACETHN as follows and include variable in dataset:
if racecmb=1 and hisp4 ge 2 racethn=1.
if racecmb=2 and hisp4 ge 2 racethn=2.
if (racecmb ge 3 and racecmb le 5) and (hisp4 ge 2) racethn=4.
if racecmb=9 and hisp4 ge 2   racethn=9. 
if hisp4=1 racethn=3.

variable label racethn “Race-Ethnicity”.
value label racethn
1 “White non-Hispanic”
2 “Black non-Hispanic”
3 “Hispanic”
4 “Other non-Hispanic”
9 “Don’t know/Refused (VOL.)”.

Please create PARENTID as follows and include variable in dataset:
This variable identifies which parent or step-parent the respondent was the point of reference for answering the Parent Series (Q18-Q23).  This variable should be system missing for respondents who were not administered the parent series. 
	Answered Parent Series thinking about: 
		Both		PARENTID = 1
		Mother		PARENTID = 2
		Father		PARENTID = 3
		Stepmother	PARENTID = 4
		Stepfather	PARENTID = 5

Please create PARENT65 based on F.1, F.2, F.3, F.3a, F.6 and F.7
IF (F.6=65-97 or F.7=65-97) PARENT65 = 1
IF (F.6=18-64 and F.7=18-64) OR (F.6=18-64 and F.7=missing OR (F.6=missing and F.7=18-64) PARENTS65=0

Please create GOVSELF based on GOV
If (1,3) to any item in the list (GOV a-h), GOVSELF=1
If (2,4,5,9) to all items in list (GOV a-h), GOVSELF=0

Please create GOVHH based on GOV
If (1-4) to any item in the list (GOV a-h), GOVHH=1
If (5,9) to all items in list (GOV a-h), GOVHH=0

Please create GOV6SELF based on GOV b,c,d,f,g,h
If (1,3) to any of the following items (GOV b,c,d,f,g,h), GOV6SELF=1
If (2,4,5,9) to all of the following items (GOV b,c,d,f,g,h), GOV6SELF=0

Please create GOV6HH based on GOV b,c,d,f,g,h
If (1-4) to any of the following items (GOV b,c,d,f,g,h), GOV6HH=1
If (5,9) to all of the following items (GOV b,c,d,f,g,h), GOV6HH=0

Please create GEN based on USBORN series
This variable identifies which immigrant generation the respondent belongs to. 
1st Generation			(USBORN1A=2 or BIRTH_HISP=3) and (USBORN2=2)
2nd Generation			(USBORN1A=1,3,4 or BIRTH_HISP=1,2) and (USBORN2=2) 
3rd Generation or more  	(USBORN1A=1,3,4 or BIRTH_HISP=1,2) and (USBORN2=1) 

Please create IDEO3X using IDEO 
Conservative	(1,2)
Moderate	(3)
Liberal       	(4,5) 



