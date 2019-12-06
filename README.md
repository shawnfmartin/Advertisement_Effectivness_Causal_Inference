# Advertisement Effectivness: Causal Inference

CS 7290 - Northeastern University - Olga Vitek

## Authors

- Shawn Martin
- Yi He
- Alexi Quintana Mathé

## Introduction

Advertising campaigns have become a staple in any company looking to expand their network and customer base. Effectiveness of these advertisements are becoming more and more crucial as campaign costs are on the rise in this competitive market. It is important to track these advertisement campaigns such that companies can make better advertising decisions to maximize company revenue and outreach to the most appropriate customers. However, in the real world proper randomized control experiments are not always available and under this circumstance, the quantity of advertisement effectiveness is hard to measure.

The goal of this project is to gauge the effect of an advertisement campaign by an online retailer on a search engine, using three datasets of observational data and imperfect randomized experiments. After conducting preliminary analysis on the observational data obtained by displaying the add on the search engine to some users and recording if they click on it or not and their behavior on the advertiser’s webpage, two imperfect randomized experiments have been conducted. We used causal inference methods to correct the effect of this imperfect randomizations and find the best approximation possible of the causal relation between showing the add and purchasing. 

## Datasets

The first dataset consists of observational data as users navigate through the search engine where some users are shown the ad while others are not. The available variables are the common ones just described, for N=4000 observations of different individuals. This data overpredicts the effectiveness of the advertisement, due to confounders such as demographic factors not known for this dataset.

For the second dataset a test of the ad on users on a small website is conducted. Users register on the website and are assigned a property whether they will see ads every time they go on the website. Then, if they click the ad and go to the retailer’s website their activity there is recorded. There are 3040 observations of 2000 different users of the website, as the same user may access the website multiple times. Additional user data includes age, gender and income level (coded 1-11). As data comes from a particular website, it is skewed to particular populations (especially older people, as 98% of the subjects are older than 45), and the ad property is not assigned randomly to the users, as we will see later. Another problem of this dataset is the presence of different observations from the same user. In addition, potential visits of the users to the retailer’s website not after clicking the ad are not recorded.

The third dataset consists of the results of a randomized trial from a reliable website. Users enter the trial and register, so their IP, a Trial ID, and their gender, age, and income level (also coded 1-11) are known. Then they are randomly shown the ad, and a coupon for purchasing on the company website is given to them if they do so. Their activity on the website of the retailer when they enter it and use the coupon at any given moment is then recorded, so that the same variables than for the second dataset are available. The advantages of this dataset is that it is properly randomized, as we will see, and now we record any visit of the subjects to the retailer’s website and not only visits right after seeing the ad. However, some users game the system by entering multiple times the trial until they get a coupon, so we will have to take this into account. This is why there are 461 observations and 400 different users, what also means the number of observations is low.
