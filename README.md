# F1Qualifying

The overall purpose of my project was to investigate the effect of different weather attributes on lap times in Formula One qualifying sessions. I sought to model the effect of weather conditions on lap times when accounting for the differences in the tracks themselves. This model could be used by teams to generate an expected lap time based on the weather and assess whether their driver outperforms or underperforms this expectation.

To achieve this, I pulled data on qualifying laps from the 2023 and 2024 seasons (via the [OpenF1 API](https://openf1.org/)). After wrangling, I was left with a final dataset of 3,967 rows and 11 columns. Each row represented one lap, and columns consisted of track attributes (track name, length, and number of turns), weather attributes (air temperature, track temperature, pressure, humidity, etc), and the outcome (lap duration).

I then applied four modeling paradigms to this data (linear regression, Bayesian linear regression with a random intercept for track, random forest, and XGBoost), and found that the random forest model was best, achieving an RMSE of 0.5 seconds and explaining 99.8% of the variance in the outcome.

While track length and number of turns were the most important variables, I found that humidity, air and track temperature, and air pressure had predictive value. Faster lap times were associated with nonextreme humidity, low pressure, high air temperature, and moderately low track temperature. The most important weather factor was humidity, as there was a huge jump in lap times when the humidity was extremely high (which could be because high humidity often comes with rain). Because of this model’s extremely high predictive accuracy, it is a useful tool for teams to evaluate how their drivers handle different weather conditions.


## App
The production version of the accompanying app is deployed [here](https://oh8w2h-cole-wagner.shinyapps.io/F1Qualifying/).
