# Sample-Models
Data Portfolio Projects: performed on various datasets deploying different techniques

1. Use Multi-Linear Regression to project out Rookie RB fantasy points for the upcoming season
- Did extensvie data wrangling using PFF,ESPN and Stathead to get relevant data
- Predict using a player's projected draft round and their Weight-adjusted 40 yard dash
- Test against historical runningbacks to see how well points can be predicted
- Predict based on current rookies; this was done prior to the nfl draft
- Last step is to visualize in tableau to show how many PPR points the model predicted for each back

![](https://github.com/eziowayne22/Sample-Models/blob/All-Models%2C-Visuals-and-Work/images/ppr_predictions.png)

2. Create a wordcloud showing the Top 25 Most Frequently Used Words in The Summa Theologica Part i
- Importing the data from the gutenbergr library
- Filtering out filler/unimportant words with dplyr
- Created the final wordcloud

![](https://github.com/eziowayne22/Sample-Models/blob/All-Models%2C-Visuals-and-Work/images/summa_1.png)

3. Create a classification model using logistic regression to predict whether or not a given pokemon is legendary
- train a logstic regression model using capture rate and base stats as the predictors
- ran model off of the test dataset with over 98% accuracy; base stats combined with capture rate are very predictive

![](https://github.com/eziowayne22/Sample-Models/blob/All-Models%2C-Visuals-and-Work/images/mewtwo.jpg)
![](https://github.com/eziowayne22/Sample-Models/blob/All-Models%2C-Visuals-and-Work/images/pokemon_accuracy.png)
![](https://github.com/eziowayne22/Sample-Models/blob/All-Models%2C-Visuals-and-Work/images/Screenshot%202022-07-10%20151141.png)

4. Use Simple Linear Regression to predict a professional soccer striker's salary based on his Finishing stat in the FIFA video game
- Normalize salaries using log10
- Train the simple linear regresion model
- Test the model; use the TIDY yardstick functions to gage the model's effectiveness

![](https://github.com/eziowayne22/Sample-Models/blob/All-Models%2C-Visuals-and-Work/images/ronaldo9.jpg)
![](https://github.com/eziowayne22/Sample-Models/blob/All-Models%2C-Visuals-and-Work/images/fifa_metrics.png)
![](https://github.com/eziowayne22/Sample-Models/blob/All-Models%2C-Visuals-and-Work/images/salary_pred.png)

5. Predict the Price of an Ounce of Gold using Time Series Models

- Train models on daily gold price data going back to 1990
- Ran several different models off of testing subset
- Predicted the price of gold 3 years into the future

![](https://github.com/eziowayne22/Sample-Models/blob/All-Models%2C-Visuals-and-Work/images/gold_ts.png)

