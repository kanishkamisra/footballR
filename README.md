# footballR
R codes for some football(soccer) analytics

# To-do
1. Get the right data, currently I have a SQLite file from Kaggle that has data from the 2008/09 season to part of 2015/16 season.
2. If I stick with the Kaggle dataset, it'll need heavy tidying and wrangling to fit a desirable model (all hail lord Hadley)
3. Apply bayesian principles to the tidied data learned in David Robinson's blog, [variance explained](http://varianceexplained.org "Variance Explained")
4. Document your steps as you perform them, very important in the field.
5. Expand on this brief to-do xd

# Data

The data for this analysis can be found [here](https://www.kaggle.com/hugomathien/soccer/downloads/database.sqlite.zip)!

Make sure you unzip the sqlite file and then replace the name of the database in the R code in "data-wrangle.R" with "database.sqlite"
