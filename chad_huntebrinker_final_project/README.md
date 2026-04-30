# Home Price Prediction Using Multiple Linear Regression
 
This project applies multiple linear regression to predict U.S. home prices using listing data scraped from Realtor.com, covering all 50 states. A stratified random sample of 2,500 homes (50 per state) was analyzed to identify which property and location features most influence home sale price. The final model performs reasonably well for general price estimation, though prediction intervals remain wide — reflecting the inherent variability in real estate pricing across diverse markets.
 
## Tools Used
 
- **R** — primary analysis language
- **leaps** — automatic variable selection via `regsubsets`
- **lmtest** — coefficient significance testing (`coeftest`)
- **dplyr** — data wrangling and stratified sampling
- **psych** — descriptive statistics

## Key Insights
 
- **Final model predictors**: number of bathrooms (`bath`), house size in sq ft (`house_size`), zip code (`zip_code`), and whether the home is in one of the 25 most expensive states (`expensive_state`)
- **Number of bedrooms and lot size** were dropped by automatic selection — they added little predictive value once bathrooms and house size were included
- **State-level cost of living** (`expensive_state`) was a meaningful predictor, with homes in high-cost states priced ~$122,549 higher on average, all else equal
- **Each additional bathroom** was associated with roughly a $49,710 increase in price; each additional square foot added ~$84
- **Model fit is modest** (adjusted R² ≈ 0.37), indicating that the selected variables explain some but not all price variation — location nuance captured only by zip code and state tier leaves room for improvement. To better improve in the future, have the model better understand zip code (it thinks it's just a number rather than a location) and have a more in-depth way of breaking down housing by location
- **Predictions were generally within ~$50,000** of actual values and all fell within 95% prediction intervals, suggesting acceptable practical accuracy for a simplified model
