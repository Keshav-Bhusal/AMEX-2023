# AMEX-2023

**Applied Econometric and Data Analytics**

## Introduction**

Welcome to the exciting world of practical data analysis in the field of economics! In this project, we're departing from the traditional, well-defined tasks typically encountered in academic settings. Instead, we're immersing you in the dynamic, real-world landscape where data isn't always neatly packaged, problems aren't always crystal clear, and the elegance of your solutions truly matters.

This year, you'll be presented with various complex business challenges, each offering its own unique dataset. You'll need to navigate the uncharted territory of evaluating data, determining feasible approaches, and deciding what to tackle and how. The stakes are high, as your solutions need to not just solve problems but also win over partners and stakeholders.

The course's overarching objective is to equip you with the skills and confidence to be effective data analysts in the real world. You'll roll up your sleeves, dive into data cleaning and manipulation, and even engage with experienced applied econometricians from the industry. The foundation of econometric theory will be an essential part of your journey.

## Key learning outcomes

1. Subject Matter Expertise: Demonstrate your proficiency in applying microeconomic and econometric approaches to solve real-world economic problems.
   
3. Critical Thinking & Problem Solving: Design and execute research strategies to answer substantial questions with tangible real-world implications. Independently apply quantitative reasoning to evaluate economic hypotheses and predictions.
   
5. Communication: Hone the ability to precisely convey your findings in written, spoken, and graphical forms, effectively communicating your insights to both economists and non-economist audiences.
   
7. Leadership & Collaboration: Master the art of effective collaboration, demonstrating initiative and leadership when appropriate in workplace settings.
   
Get ready for a challenging yet rewarding experience that will shape you into a proficient and adaptable data analyst, ready to tackle the complexities of the real world.

# Business Problem: Early Legal Effectiveness

**What is the problem?** 

In US Credit, one strategy we use for high dollar, non-communicative cases is so-called ‘Early Legal’, which warns card members about upcoming placement with a collections attorney (in lieu of a standard collections agency).

As part of a broader initiative at AXP, we are trying to understand when to push each of our many strategies as the primary collections tool. To that end, we need to understand better about why the strategy motivates some cardmembers to work with us while others take no action and are ultimately placed with an attorney.

The primary ask is for you to analyze our historic WCC credit cases and help us to understand which aspects/attributes of cases in this strategy explain the observed outcome(s) for strategy entrants, i.e.
        - Cardmembers make contact/payment and case remains in-house (or resolves somehow)
        - Cardmembers ultimately moved to outside legal partners

**Why is it important?**
There is keen interest in understanding when the strategy works and when it does not. Further, it’s important to keep our CMs in house whenever practical, since legal is an expensive collection method.

**Deliverable** 

Ideally an analysis based around a predictive model or models that can help us get at something like marginal effects, though methodology is entirely (and purposely) left open ended.  

1. What attributes of a case make it more likely that Early Legal will evoke either payment or at least contact/RPC in the case? 
2. Is the strategy more effective on Consumer or OPEN Small Business cases? If so, why? 
3. Today we have no SMS strategy for Early Legal. Is there any evidence that those cardmembers who received SMS prior to entering Early Legal were more engaged with us? Are they equally likely to be sent to attorney or more responsive to the legal warning letters? 
4. Does getting an RPC matter when it comes to success in getting a payment? In other words, should we bother dialing these folks or just send the letter and wait for an inbound call? 
5. We currently give them 14 days to pay to avoid legal placement. How does the time elapsed in the strategy influence the probability of a payment coming in? Are we being too generous giving two weeks?

## Business Problem: OA Placement

**What is the problem?** 

In US Credit, one strategy we generally place cases (all accounts in a case) with an outside agency within about 30 days of cancelling them. 

One question that has come up, is whether we’re keeping the right population in-house vs. sending them to an agency. Should everyone be placed at that point, or are some of them likely to pay if we keep them in house longer? 

The primary ask is for you to analyze our historic WCC credit cases and help us to understand why some cardmembers pay us post-cancellation and others do not, ultimately getting placed with an outside collections agency. 

**Why is it important?**
Today there isn’t much differentiation in this strategy, but it’s important to get debts to an agency as early as possible. Should we accelerate more of what we keep in house today?

**Deliverables**

Ideally an analysis based around a predictive model or models that can help us get at something like marginal effects, though methodology is entirely (and purposely) left open ended.  

1. What attributes of a case make it more likely that a cancelled cardmember will make some kind of payment or at least contact/RPC in the case? 
2. Does the distinction between Consumer or OPEN Small Business case matter here? If so, why? 
3. Does the number and type of communication we have with the CM make a material impact post-cancellation on whether they pay/RPC or end up going to an OA? Does more engagement (SMS, email) help? 
4. Does getting an RPC matter when it comes to success in getting a payment post-cancellation? Should we make more/less effort to contact cancelled cases than we do today?

## Dependencies 
All the analytical works will be basically done in [Rstudio](https://www.r-project.org/). 
This project depends on the private datasets of American Express [AMEX]([https://www.cdc.gov/](https://www.americanexpress.com/en-us/careers/?intlink=us-amex-career-en-us-navigation-logo))

## Datasets
This project includes the American Express (AMEX) dataset. The dataset contains the sensitive public information and cannot be shared due to the organizational policy. 

## R-packages
The analysis requires setting the R environment using various packages. The some of the packages include-
* tidyverse
* diplyr
* readr
* ggplot2
* rio
* VIM
* Zoo

## Authors

Keshav Bhusal

[LinkedIn](www.linkedin.com/in/keshav-bhusal-37a3a3145/)


## License
Apache License
Version 2.0, January 2004
http://www.apache.org/licenses/


## Acknowledgments
**Supervisor**
Professor. Dr. Gary Thompson
Professor and Head
Department of Agricultural & Resource Economics
McClelland Park 304C
650 N. Park Ave.
Tucson, AZ  85719

**Contact Person from AMEX** 
Charles 
American Express, Phoenix

**Team Member**
Zoey Reed-Spitzer
Pavan Kalyan Thodeti
