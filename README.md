# conjoints-power
Power analysis for Conjoint Experiments: People vs. Trials


# To DO based on PA comments 

Major:

1. Collect entire range of AMCEs from the papers in the lit review. Use MTurk or students 
2. Add a note on hetero at individaul level and be more excplixit both in the simulation description and conclusions
3. Run simulation just varying n factors and explicity explain that AMCE marginalize over variables [R: *no formal results on the question of respondents vs. number of trials*]
4. VALIDATION DGP using bootstrapping on a study that is very well powered. Place a note when we talk about parametric approaches and at the end of the retrospective data power analysis]
5. Completely remove critiques parametric approaches and Sawtooth software.
6. Stress that we can do more with the simulation compared to parametric approaches. Stress that our DGP should be used as a framework to test a set of hypotheses. 
7. Include correction for multiple-testing

Mid: 

1. Make more explict that experimentalists **really claim** that power is irrelevant with conjoints. That's why power calculations are relevant,
2. Explain why biased cofficents might be problematic [R: **If experimentalists are focusing on the substantively largest effects, but the design is underpowered such that researchers might actually be paying attention to the wrong factors**]
3. Explain that in some context trade-off are important to consider [R: **the acknowledgment that there are some contexts where it isn’t realistic or feasible to reduce the number of levels (with respect to Hainmueller and Hopkins 2015) was useful and compelling.**]
2. Be more explicit on the goal of the paper [**this manuscript tries to provide more rigorous guidance, in particular focusing on the questions of the number of levels (k), choices (T) and subjects (N)**]
3. Stress the importance of the shiny app in the paper
6. Remove part on the variance of the estimates in conjoint experiments and simply say that we have no clue about Type M and Type S errors 

Minor:

1. Fix Abstract **R: The abstract mentions that Type S and Type M errors are especially pronounced for experimental designs with relatively small effective sample sizes (< 3000), which seems concerning until midway through the manuscript, when it becomes clear that the effective sample size is N*T rather than just N.** This should be clear upfront.** 
2. Fix **Throughout the manuscript (including e.g. p. 2), the manuscript should be clear that “experimental conditions” or “treatments” here is referring to levels, rather than factors/attributes** [TO DO: OK]
3. Footnote on the converstion betweent probabilities to choen D and say we are not using standardized effects
4. Fix **R: The notation used is a bit confusing. For instance, using X_1 to refer to the first attribute but X_p1 as the full set of attributes for a profile, is a bit difficult to follow.** 
5. Clarify **R: What do you mean by a latent probability of choosing a profile? Are you talking about the probability that one chooses a particular profile over a randomly chosen other profile.** Need to clarify 
6. Fix **R: On the following: "As can be immediately seen, these latent underlying probabilities do not sum to 1, hence it is not immediately possible to form an expectation about profile selection. One of the approaches, identified by Gall (2020), proposes the following: start with a 50% probability of selecting either of the profiles and adjust this probability by the causal effect divided by two (for two profile alternatives): e.g. the probability of selecting Profile 1 given its gender (assuming ”1 = 2%) would increase to 51% = 50% + 2%/2. This adjustment guarantees that the resulting probabilities sum to 1." It is unclear what the exposition is try to get at here, and how this connects at all to AMCEs and the estimation of AMCEs, which are formalized in a completely different manner.**



- R1: **Finally, the acknowledgment that there are some contexts where it isn’t realistic or feasible to reduce the number of levels (with respect to Hainmueller and Hopkins 2015) was useful and compelling. Acknowledging and incorporating these tradeoffs more in the manuscript will help better inform experimental practices, and increase the manuscript’s impact in the scholarly literature.** maybe but i think it is also enough for this paper. 

- R2: **On the paragraph: "Another point of interest is the variance of the estimates in conjoint experiments. ...These results are, however, derived only from simulations of three specific papers, so it is questionable to what extent do they generalise to conjoint designs used in political science." I'm not aware of the guidance cited in this paragraph being followed widely by political scientists.** ok, We can remove and just say we have no clue if results are precise or not. 

- R2: **And even a basic two-condition experiment can exhibit the problem of there being many estimates of interest, as there are often many different outcomes that are measured and hence many different ATEs can be estimated. This issue and its implications for statistical testing are often addressed through multiple testing corrections. It was interesting to me that there is no mention or consideration of such methods anywhere in this paper.** I agree, we might want to include a tukey test.  [TO DO: OK, this should be included]

-  R2:  **On a side note, I don't believe it is ever clearly stated what their test statistic and estimation procedures actually are, which makes things a bit difficult to follow. I assume it is some estimator for individual AMCEs. This needs to be clarified.** I think we did quite clearly. 

- R2: **In addition, conjoint designs often include attributes that contain levels on an interval scale (e.g. age of candidates, percentage tax cuts in policy packages, etc.). In those cases, researchers are often interested not in single AMCEs but rather in multi-AMCE gradients, and hence small AMCE estimates at the beginning of those interval scales (e.g. difference between 0% and 5% tax cuts) are not as concerning when considered in the broader context (e.g. when also looking at the estimates with respect to 10%, 20%, 30%, etc. tax cuts). These are very standard characteristics of conjoint experimental design in political science (and beyond) that applied researchers must consider, and the study's treatment of all AMCEs as essentially independent, equally important estimands seems a bit misguided from a practical perspective.** Not sure i see where the editor is going 


- R2: **On p. 5, I don't understand the approximation formula attributed to the Sawtooth Software. A required sample size should be a minimum size, so the directionality of the inequality does not make sense. The formula also does not take any statistical power considerations into account (e.g. effect size, variance, error tolerance). Am I missing something here?** No

- R2: **In terms of the results presented in figure 1, specifically the distribution of estimates presented, are these all based on forced-choice conjoint dependent variables? It would not make sense to compare effects in terms of their absolute magnitude for DVs that are coded differently, if this also includes results where the DVs are rating scales or something else.** Sure but no. [TO DO: Put a footnote to convert probabilities to choen D and say we are not using standardized effects]

- R2: **On p. 6 and elsewhere, the term "effect size" is often used in the power analysis literature to specifically refer to standardized effects (i.e. the effect of interest divided by a reference standard deviation, often the pooled standard deviation of the outcome). However, you seem to be using it to refer to effects in their raw, absolute terms. This should be clarified throughout the paper, since it will be highly confusing for people more familiar with power analysis.** ture that. 



- 


# PA comments


**Usage of CJ in the literature**

- R1: **"My sense is that experimentalists don’t really claim that power is irrelevant with conjoints, but merely, that conjoint experiments let researchers manipulate a larger number of factors at once than their conventional counterparts."** Not true. We should add add the Political Communication paper in addition to Ketzer
- R1:  **"It doesn’t actually look at the effects of varying the number of *factors* on statistical power, which is always held constant in their simulations, but rather, the number of *levels* within a given factor."** Explain that looking at factors is irrelevant 

**Lit review**

- R2: **That is, you can't give summary statistics on only the "minimum statistically significant AMCE" estimates, without discussing the overall distribution of AMCE estimates (including the large ones), and then conclude that a "large portion of effects found in conjoint experiments are relatively small". Since the authors have already collected data on all of their studies, it would actually be extremely interesting to provide more statistics on the distribution of AMCE estimates more broadly.** agree [TO DO: run simulation just varying n factors and make marginalisation more explicit]

**Trade-off**

- R1: **"It sometimes seemed as if the authors were going to discuss satisficing as a rationale for nonexchangeability between these two dimensions, but never ended up going in this direction"** Maybe we should add something on this. 
- **R2: And yet, while the simulations do alter the numbers of subjects and trials, the results presented do not explore or shed light on this tradeoff, and there are no formal results on the question of respondents vs. number of trials.** let's add a note on this 

[TO DO: Add a note on hetero at individaul level and be more excplixit both in the simulation description and conclusions]



**Schuessler and Freitag 2020**


- R1 and R2: **On the following sentence: "Furthermore, recent work that uses parametric approaches to calculate the minimal required sample size does not distinguish between the number of respondents and the number of trials, which can have consequences for power calculation when treatment heterogeneity is present (Schuessler and Freitag 2020)." This is a potentially interesting line of inquiry to follow, but it is currently underdeveloped and should be further expounded, ideally with some formalization.** This scares me.
- R2: **In addition, the study very briefly mentions a paper on conjoint power by Schuessler and Freitag, which seems extremely relevant here, but there is little detail provided on that paper and how it compares to the material provided here. Does the simulation-based approach presented here lead to meaningfully different results than in that paper? This would be useful to discuss.** Scares me but we need to tackle if we cite it. 

**parametric approaches**

- R2: **P. 6 presents a critique of parametric power analyses for conjoint. Many of the critiques in this section are either overblown or insufficiently developed.** This is how they are presented in other studies on conjoint but it is clear that the reviewer is a fun of parametric approches. I think we need to drop the part on the parametric approaches and just stress the advantages of doing a simulation 

- R2: On the following claim: "While parametric approaches can be used to measure a choice probability with some desired level of accuracy, they typically disregard the number of attributes and levels included in the design. Hence, they assume that the size of the design does not impact its statistical power." This statement feels quite overblown. Even with relatively non-sophisticated power analysis, users understand that the more levels there are, the fewer observations there will be for each level.  I don't agree here. Of course you can but that's not what it is happening in practice. 

- R2: **The use of clustered standard errors has little influence on statistical power. If this is the case, then provided some threshold conditions are met (e.g. sufficient number of respondents such that cluster robust variance matrix estimator's asymptotic properties can kick in), then there should in theory not be a significant trade-off between subjects and trials (again, within some reasonable limts).** Good point. Not sure we can test this with our simulation 

- R2: **On the following claims: "Second, such rules of thumb are not usually designed to provide guidance for testing for specific hypotheses based on the hypothesised parameter values. That is, sample requirements do not seem to depend on the magnitude of the effect size. Given the design complexity of conjoint experiments, this is unlikely to provide efficient guidance about sample requirements. Finally, sample size considerations based on current approaches often ignore the desired power for the hypothesis tests of interest." It feels like this whole section has set up an unrealistic strawman, much of which may be based on the commercial Sawtooth software. Perhaps I am not fully aware of what the entire discipline is doing, but at least in my experience and conversations with political scientists using conjoint experiments, they typically do not rely on Sawtooth or Sawtooth-based recommendations. This reads more like a critique of Sawtooth than a critique of common practices among conjoint researchers.** kinda true but only maybe for top journals. Also, political scientists might still read the recommendations produced using such rules.

[TO DO: need to say we can do more with the simulation compared to parametric approaches 
TO DO: need to remove critiques of parametric approaches ]

**Rationale and motivations**


- R1: **"Experimentalists are increasingly aware of the importance of statistical power, but power analysis in conjoint experiments is often complicated. Rather than providing fuzzy rules of thumb, this manuscript tries to provide more rigorous guidance, in particular focusing on the questions of the number of levels (k), choices (T) and subjects (N)."** I think we do but this we can stress more [TO DO: be more explicit on the goal and stress app in the paper]

- R1: **"for the simulations, a discussion of how power concerns with conjoint experiments distort the ranking of AMCEs. If experimentalists are focusing on the substantively largest effects, but the design is underpowered such that researchers might actually be paying attention to the wrong factors, this is something experimentalists should be aware of, and which would increase the value of the manuscript to researchers."** good point. we should include [TO DO: be more explicit in the paper]

- R2: **[Simlations] However, they should be in the service of either (a) providing insights that would have otherwise been difficult/impossible to know using analytical or other methods, (b) providing a proof of concept or baseline evidence related to something previously not known, or (c) providing clear-cut answers with some form of verifiable "correctness" and/or generalizability that can then be employed in practice. 

-R2: **The simulation is highly stylized, with a very specific data-generating process that is simulated with no validation that this DGP matches real-world data.** I agree but i am not sure how you can validate DGP with real-word data. [TO DO: VALIDATION using bootstrapping on a study that is very well powered. end of undepower litterature]

- R2: **the results are generally obvious and predictable (e.g. the more levels there are, the less power one has to detect an effect pertaining to a particular level). As a result, it is also unclear if there is any useful unique guidance at a high level that is not already known by researchers.** Of course this is the case, but researchers seems not to have understood this. 

- R2:  **I am extremely curious as to whether they would actually lead to results that are meaningfully different from what would one get from adapting a simple t-test power analysis, simply adapting for the cluster structure and the knowledge that tasks would be spread across how ever many levels are in an attribute. Yet there are no results presented in the paper that actually demonstrate that the simulation-based approach presented adds value beyond simpler approaches.** It is kinda true. [TO DO: sell that our simulation is a framework ]


**Experiments are all shait:**

- R2: **it is a common observation that empirical research in the social sciences is often underpowered, across all research designs. Is there evidence that conjoint studies tend to be underpowered even more frequently (or perhaps less frequently) than other experimental studies?** No but this is still relevant.....

- R2: **The final main advice seems to be that "This finding underlines how—even with relatively large sample sizes and the number of trials—conjoint experiments are not suited to draw inferences for experimental conditions with relatively small effect sizes." But there is nothing unique about conjoint experiments that makes this the case. Small effects in conjunction with a small sample size is a statistically problematic situation and will lead to the various types of error discussed in the paper regardless of one's research design, estimation method, etc.** Agree but this is more problematic because people think they can 

- R1: **"(e.g. p. 17 - “we take them as effects that have made it through peer-review in the publication process, hence effects of this size are likely of interest to the research community.”) should be tempered somewhat, and the discussion of minimum AMCEs on p. 5 should be qualified." Agree on this.** We should maybe report both the mix and max AMCE. However, insisting on the fact that "control variable" estimates in conjoint are biased should be in there [TO DO: try brining all the distribution of AMCEs]

- R2: **I have not observed it to be a common occurrence for conjoint studies to emphasize or go at length to interpret estimated effects that are so small; they typically focus on the large effects. So is this actually information that should change current practices?** I think so. 


**Other stuff**

- R1:**The abstract mentions that Type S and Type M errors are especially pronounced for experimental designs with relatively small effective sample sizes (< 3000), which seems concerning until midway through the manuscript, when it becomes clear that the effective sample size is N*T rather than just N.** This should be clear upfront. ok, good point but depends how we address the previous point on non exchangeability between responents/levels [TO DO: fix]

-  R1:**Throughout the manuscript (including e.g. p. 2), the manuscript should be clear that “experimental conditions” or “treatments” here is referring to levels, rather than factors/attributes** [TO DO: OK]

- R1: **Finally, the acknowledgment that there are some contexts where it isn’t realistic or feasible to reduce the number of levels (with respect to Hainmueller and Hopkins 2015) was useful and compelling. Acknowledging and incorporating these tradeoffs more in the manuscript will help better inform experimental practices, and increase the manuscript’s impact in the scholarly literature.** maybe but i think it is also enough for this paper. 

- R2: **On the paragraph: "Another point of interest is the variance of the estimates in conjoint experiments. ...These results are, however, derived only from simulations of three specific papers, so it is questionable to what extent do they generalise to conjoint designs used in political science." I'm not aware of the guidance cited in this paragraph being followed widely by political scientists.** ok, We can remove and just say we have no clue if results are precise or not. 

- R2: **And even a basic two-condition experiment can exhibit the problem of there being many estimates of interest, as there are often many different outcomes that are measured and hence many different ATEs can be estimated. This issue and its implications for statistical testing are often addressed through multiple testing corrections. It was interesting to me that there is no mention or consideration of such methods anywhere in this paper.** I agree, we might want to include a tukey test.  [TO DO: OK, this should be included]

-  R2:  **On a side note, I don't believe it is ever clearly stated what their test statistic and estimation procedures actually are, which makes things a bit difficult to follow. I assume it is some estimator for individual AMCEs. This needs to be clarified.** I think we did quite clearly. 

- R2: **In addition, conjoint designs often include attributes that contain levels on an interval scale (e.g. age of candidates, percentage tax cuts in policy packages, etc.). In those cases, researchers are often interested not in single AMCEs but rather in multi-AMCE gradients, and hence small AMCE estimates at the beginning of those interval scales (e.g. difference between 0% and 5% tax cuts) are not as concerning when considered in the broader context (e.g. when also looking at the estimates with respect to 10%, 20%, 30%, etc. tax cuts). These are very standard characteristics of conjoint experimental design in political science (and beyond) that applied researchers must consider, and the study's treatment of all AMCEs as essentially independent, equally important estimands seems a bit misguided from a practical perspective.** Not sure i see where the editor is going 


- R2: **On p. 5, I don't understand the approximation formula attributed to the Sawtooth Software. A required sample size should be a minimum size, so the directionality of the inequality does not make sense. The formula also does not take any statistical power considerations into account (e.g. effect size, variance, error tolerance). Am I missing something here?** No

- R2: **In terms of the results presented in figure 1, specifically the distribution of estimates presented, are these all based on forced-choice conjoint dependent variables? It would not make sense to compare effects in terms of their absolute magnitude for DVs that are coded differently, if this also includes results where the DVs are rating scales or something else.** Sure but no. [TO DO: Put a footnote to convert probabilities to choen D and say we are not using standardized effects]

- R2: **On p. 6 and elsewhere, the term "effect size" is often used in the power analysis literature to specifically refer to standardized effects (i.e. the effect of interest divided by a reference standard deviation, often the pooled standard deviation of the outcome). However, you seem to be using it to refer to effects in their raw, absolute terms. This should be clarified throughout the paper, since it will be highly confusing for people more familiar with power analysis.** ture that. 

- R2: **The notation used is a bit confusing. For instance, using X_1 to refer to the first attribute but X_p1 as the full set of attributes for a profile, is a bit difficult to follow.** Ok. 

- R2: **What do you mean by a latent probability of choosing a profile? Are you talking about the probability that one chooses a particular profile over a randomly chosen other profile.** Need to clarify 

- R2: **On the following: "As can be immediately seen, these latent underlying probabilities do not sum to 1, hence it is not immediately possible to form an expectation about profile selection. One of the approaches, identified by Gall (2020), proposes the following: start with a 50% probability of selecting either of the profiles and adjust this probability by the causal effect divided by two (for two profile alternatives): e.g. the probability of selecting Profile 1 given its gender (assuming ”1 = 2%) would increase to 51% = 50% + 2%/2. This adjustment guarantees that the resulting probabilities sum to 1." It is unclear what the exposition is try to get at here, and how this connects at all to AMCEs and the estimation of AMCEs, which are formalized in a completely different manner.** Ok, let's clarify 



# To do list

1. implement different # of attributes & levels *DONE*
2. write a function to check whether profile A is not compared with profile A, or that the same combination of profiles does not show twice to the same person  *DONE* [for now it is excluded as in H.]
1. implement functions for simulation studies (Type M, Type S, etc.) [DONE, need to be checked]
1. rewrite the coefficients generating function from static to 
    cx1 <- rnorm(1, 0.09, 0.05) *ALBERTO: GONNA DO NEXT* [DONE]
2. add truncation to generating probabilities before rbinom [I think it is not necessary anymore]
    if prob < 0, then prob = 0.001
    ​if prob > 1, then prob = 0.999
3. research what values are conventionally used/found in SocSci (polsci & sociology) [still TO DO]
    a. coefficients—i.e. effect sizes
    b. # of people & # trials per person

# New Script 20/04/20

1. I've reviewed the work of Robert Kubinec. He adds many things that are not necessary for our paper idea. Most importantly, his script uses a continuous outcome, an option that is not commonly used. [DISCUSS]
2. I've reworked the script almost completely fixing the coefs and taking into account the clustered nature of the DGP. Now truncation should not be necessary any more [TAKE A LOOK TOGETHER]
3. Simulation should work now. We need to run the PA

# Meeting 21/04/20
Martin
1. Generalise # of levels per variable in the generate_design() -- DONE!
2. Check the math behind % generation for profile selection
- switch from multiplicaiton to addition -- DONE!
- use rbinom() -- DONE!
- be coherent in prob vs. log odds ratios -- DONE!

# Meeting 28/04/20

Alberto 
1. Collect articles and data on published cj
	- *American Political Science Review* CHECKED 
	- *American Journal of Political Science* CHECKED 
	- We now have 31 articles. Shall we go on?
2. Setting up docker 3.5.1 R server on aws 

Martin 
1. Polish the script           
2. Think about truncation  

# Meeting 05/05/20

Alberto 
1. Robust standard error 
2. Lit review 

Martin 
1. Polish the script 
2. Run the simulation! 

# Meeting 07/05/20

Alberto 
1. Lit review 

Martin 
1. no loop for break/next
2. as.factor() for dummy

# Meeting 14/05/20

Alberto 
1. Lit review 
	- Check rest of the journal 
	- Fix as.factor() [DONE]
	- Try the simulation locally [DONE]

Martin 
1. Try the simulation locally 
2. deploy it on the most powerful server that we have inhouse (aka Martin's laptop)

# Structure for the paper 

1. Intro 
	1. How important is CJ nowadays 
	2. Why we need power analysis for CJ
2. Intrucing the data generating process 
	1. Montecarlo simulation 
	2. Equation
3. More lit review
	- *American Political Science Review* CHECKED 
	- *American Journal of Political Science* CHECKED 
	- *The Journal of Politics* CHECKED
	- *British Journal of Poilitical Science* CHEKED 
	- *European Journal of Poilitical Research* CHECKED 
	- *Political Research Quarterly* [No articles]
	- *Perspectives on Politics* CHECKED 
	- *Political Behavior (journal)* CHECKED 
	- *Public Opinion Quarterly* CHECKED
	- *European Political Science Review* CHECKED
	- *Electoral Studies* CHECKED
	- *Journal of Experimental Political Science* CHECKED
	- *Political Science Research and Methods* CHECKED
	- *Research & Politics* 
2. First section: TRADE-OFF between task and respondents 
	1. FIX N-Levels to a constant  
3. Second Section: TRADE-OFF Number of levels Sample sixe 
	1. FIX effect size to a constant  
3. Third Section: Systemic implications (published study)
	1. Meta-review 
	1. Type M  
	2. TYpe S 
4. Conclusions
	1. Importance of the findings 
	2. Shiny App 





# Interesting references
- https://www.ue.katowice.pl/fileadmin/_migrated/content_uploads/7_O.Vilikus_Optimalization_of_Sample_Size....pdf
- https://www.sawtoothsoftware.com/download/techpap/samplesz.pdf
- https://www.tandfonline.com/doi/full/10.1080/10584609.2018.1493009
- https://www.sawtoothsoftware.com/download/techpap/introsim.pdf

# New references
- http://benjaminlauderdale.net/files/papers/ConjointPaperHanrettyLauderdaleVivyanAJPS.pdf



