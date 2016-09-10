Statistics (Jake Vanderplas)
============================

'Sometimes the questions are complicated and the answers are simple'
- Dr. Seuss

Warm-up
-------

Coin toss

* 22 Heads / 30 Flips
    - Could be just by chance!
    - No! 15 / 30 is the average, coin is not fair!
* Ends up with a p-value (probability) of 0.08%
    - p`<`0.05 or 5%, reject!
    - Assuming null hypothesis = assuming

1. Simulating (can simulate above warm up)

Four Recipes
------------

2. Shuffling

* Helps you determine if there's any difference between two groups
* Allows you to create models if you don't have a generative one
* Shuffle data and see if there's any difference

3. Bootstrap Resampling

* Assuming that your dataset is a representative of the underlying model.
* Sample from this dataset and average/standard deviation many many times.

4. Cross Validation

* Cut up data randomly
* Fit to one piece of the data and test its fit on the other half

