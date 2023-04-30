Download Link: https://assignmentchef.com/product/solved-homework-2-mixed-effects-models-sta-442-methods-of-applied-statistics
<br>
<h1>Math (10 marks)</h1>

<strong>data</strong>(“MathAchieve”, package = “MEMSS”) <strong>head</strong>(MathAchieve)

School Minority   Sex  SES MathAch MEANSES 1 1224 No Female -1.528    5.876 -0.428

<ul>

 <li>1224 No Female -0.588 19.708 -0.428</li>

 <li>1224 No   Male -0.528 20.349 -0.428 4   1224 No Male -0.668    781 -0.428 5 1224 No   Male -0.158 17.898 -0.428</li>

</ul>

6  1224     No  Male 0.022  4.583 -0.428

From Maindonald and Braun, ch 10 q 5. In the data set MathAchieve (MEMSS package), the factors Minority (levels yes and no), and the variable SES (socio-economic status) are clearly fixed effects. Carry out an analysis that treats School as a random effect. Does it appear that there are substantial differences between schools, or are differences within schools nearly as big as differences between students from different schools? Write a short report ( a single page of text plus a few graphs).

<h1>Q3: Drugs (20 marks)</h1>

<a href="http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/35074">http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/35074</a>

The Treatment Episode Data Set – Discharges (TEDS-D) is a national census data system of annual discharges from substance abuse treatment facilities. TEDS-D provides annual data on the number and characteristics of persons discharged from public and private substance abuse treatment programs that receive public funding.

<strong>download.file</strong>(“http://pbrown.ca/teaching/appliedstats/data/drugs.rds”,

“drugs.rds”) xSub = <strong>readRDS</strong>(“drugs.rds”) <strong>table</strong>(xSub$SUB1)

<ul>

 <li>MARIJUANA/HASHISH (2) ALCOHOL</li>

</ul>

188406                      97013

<ul>

 <li>HEROIN (7) OTHER OPIATES AND SYNTHETICS</li>

</ul>

58511                      45609

(10) METHAMPHETAMINE           (3) COCAINE/CRACK

21606                      11333

<strong>table</strong>(xSub$STFIPS)[1:5]

(1) ALABAMA   (2) ALASKA  (4) ARIZONA (5) ARKANSAS (6) CALIFORNIA

616         1360         4479         1508        48065

<strong>table</strong>(xSub$TOWN)[1:2]

ABILENE, TX AKRON, OH

42      1078

Each row of the dataset corresponds to an individual admitted to a drug or alcohol addiction treatment facility. The variables above are:

<ul>

 <li>completed is TRUE if the individual in question completed their treatment and FALSE</li>

 <li>SUB1 is the substance which was the individual’s primary addiction.</li>

 <li>GENDER, AGE, raceEthnicity are the individuals age, gender and ethnicity, known to be important confounders.</li>

 <li>STFIPS, TOWN, the US state and town in which the treatment was given.</li>

</ul>

Write a short report addressing the hypothesis that chance of a young person completing their drug treatment depends on the substance the individual is addicted to, with ‘hard’ drugs (Heroin, Opiates, Methamphetamine, Cocaine) being more difficult to treat than alcohol or marijuana. A secondary hypothesis is that some American states have particularly effective treatment programs whereas other states have programs which are highly problematic with very low completion rates.

The report should be on the order of four paragraphs: introduction, methods, results, conclusions. Not more than two pages of text, closer to one page is better.

Some code below may or may not be helpful.

forInla = <strong>na.omit</strong>(xSub)

forInla$y = <strong>as.numeric</strong>(forInla$completed)

<strong>library</strong>(“INLA”)

ires = <strong>inla</strong>(y ~ SUB1 + GENDER + raceEthnicity + homeless


<strong>f</strong>(STFIPS, hyper=<strong>list</strong>(prec=<strong>list</strong>( prior=’pc.prec’, param=<strong>c</strong>(0.1, 0.05))))


<strong>f</strong>(TOWN),

data=forInla, family=’binomial’,

control.inla = <strong>list</strong>(strategy=’gaussian’, int.strategy=’eb’))

sdState = Pmisc::<strong>priorPostSd</strong>(ires) <strong>do.call</strong>(matplot, sdState$STFIPS$matplot) <strong>do.call</strong>(legend, sdState$legend)

Figure 1: State-level standard deviation

toPrint = <strong>as.data.frame</strong>(<strong>rbind</strong>(<strong>exp</strong>(ires$summary.fixed[,

<strong>c</strong>(4, 3, 5)]), sdState$summary[, <strong>c</strong>(4, 3, 5)]))

sss = “^(raceEthnicity|SUB1|GENDER|homeless|SD)(.[[:digit:]]+.[[:space:]]+| for )?” toPrint = <strong>cbind</strong>(variable = <strong>gsub</strong>(<strong>paste0</strong>(sss, “.*”),

“\1”, <strong>rownames</strong>(toPrint)), category = <strong>substr</strong>(<strong>gsub</strong>(sss,

“”, <strong>rownames</strong>(toPrint)), 1, 25), toPrint)

Pmisc::<strong>mdTable</strong>(toPrint, digits = 3, mdToTex = TRUE, guessGroup = TRUE, caption = “Posterior means and quantiles for model parameters.”)

ires$summary.random$STFIPS$ID = <strong>gsub</strong>(“[[:punct:]]|[[:digit:]]”,

“”, ires$summary.random$STFIPS$ID) ires$summary.random$STFIPS$ID = <strong>gsub</strong>(“DISTRICT OF COLUMBIA”,

“WASHINGTON DC”, ires$summary.random$STFIPS$ID) toprint = <strong>cbind</strong>(ires$summary.random$STFIPS[1:26, <strong>c</strong>(1, 2, 4, 6)], ires$summary.random$STFIPS[-(1:26),

<strong>c</strong>(1, 2, 4, 6)])

<strong>colnames</strong>(toprint) = <strong>gsub</strong>(“uant”, “”, <strong>colnames</strong>(toprint)) knitr::<strong>kable</strong>(toprint, digits = 1, format = “latex”) Table 1: Posterior means and quantiles for model parameters.

<table width="539">

 <tbody>

  <tr>

   <td width="291"> </td>

   <td width="75">0.5quant</td>

   <td width="91">0.025quant</td>

   <td width="83">0.975quant</td>

  </tr>

  <tr>

   <td width="291"><strong>(Intercept) </strong>(Intercept)</td>

   <td width="75">0.682</td>

   <td width="91">0.562</td>

   <td width="83">0.826</td>

  </tr>

  <tr>

   <td width="291"><strong>SUB1 </strong>ALCOHOL</td>

   <td width="75">1.642</td>

   <td width="91">1.608</td>

   <td width="83">1.677</td>

  </tr>

  <tr>

   <td width="291">HEROIN</td>

   <td width="75">0.898</td>

   <td width="91">0.875</td>

   <td width="83">0.921</td>

  </tr>

  <tr>

   <td width="291">OTHER OPIATES AND SYNTHET</td>

   <td width="75">0.924</td>

   <td width="91">0.898</td>

   <td width="83">0.952</td>

  </tr>

  <tr>

   <td width="291">METHAMPHETAMINE</td>

   <td width="75">0.982</td>

   <td width="91">0.944</td>

   <td width="83">1.022</td>

  </tr>

  <tr>

   <td width="291">COCAINE/CRACK</td>

   <td width="75">0.876</td>

   <td width="91">0.834</td>

   <td width="83">0.920</td>

  </tr>

  <tr>

   <td width="291"><strong>GENDER </strong>FEMALE</td>

   <td width="75">0.895</td>

   <td width="91">0.880</td>

   <td width="83">0.910</td>

  </tr>

  <tr>

   <td width="291"><strong>raceEthnicity </strong>Hispanic</td>

   <td width="75">0.829</td>

   <td width="91">0.810</td>

   <td width="83">0.849</td>

  </tr>

  <tr>

   <td width="291">BLACK OR AFRICAN AMERICAN</td>

   <td width="75">0.685</td>

   <td width="91">0.669</td>

   <td width="83">0.702</td>

  </tr>

  <tr>

   <td width="291">AMERICAN INDIAN (OTHER TH</td>

   <td width="75">0.730</td>

   <td width="91">0.680</td>

   <td width="83">0.782</td>

  </tr>

  <tr>

   <td width="291">OTHER SINGLE RACE</td>

   <td width="75">0.864</td>

   <td width="91">0.810</td>

   <td width="83">0.920</td>

  </tr>

  <tr>

   <td width="291">TWO OR MORE RACES</td>

   <td width="75">0.851</td>

   <td width="91">0.790</td>

   <td width="83">0.917</td>

  </tr>

  <tr>

   <td width="291">ASIAN</td>

   <td width="75">1.133</td>

   <td width="91">1.038</td>

   <td width="83">1.236</td>

  </tr>

  <tr>

   <td width="291">NATIVE HAWAIIAN OR OTHER</td>

   <td width="75">0.847</td>

   <td width="91">0.750</td>

   <td width="83">0.955</td>

  </tr>

  <tr>

   <td width="291">ASIAN OR PACIFIC ISLANDER</td>

   <td width="75">1.451</td>

   <td width="91">1.225</td>

   <td width="83">1.720</td>

  </tr>

  <tr>

   <td width="291">ALASKA NATIVE (ALEUT, ESK</td>

   <td width="75">0.844</td>

   <td width="91">0.623</td>

   <td width="83">1.143</td>

  </tr>

  <tr>

   <td width="291"><strong>homeless </strong>TRUE</td>

   <td width="75">1.015</td>

   <td width="91">0.983</td>

   <td width="83">1.048</td>

  </tr>

  <tr>

   <td width="291"><strong>SD</strong>STFIPS</td>

   <td width="75">0.581</td>

   <td width="91">0.482</td>

   <td width="83">0.698</td>

  </tr>

  <tr>

   <td width="291">TOWN</td>

   <td width="75">0.537</td>

   <td width="91">0.482</td>

   <td width="83">0.597</td>

  </tr>

 </tbody>

</table>




<table width="547">

 <tbody>

  <tr>

   <td width="98">ID</td>

   <td width="52">mean</td>

   <td width="60">0.025q</td>

   <td width="60">0.975q</td>

   <td width="105">ID</td>

   <td width="52">mean</td>

   <td width="60">0.025q</td>

   <td width="60">0.975q</td>

  </tr>

  <tr>

   <td width="98">ALABAMA</td>

   <td width="52">0.2</td>

   <td width="60">-0.3</td>

   <td width="60">0.7</td>

   <td width="105">MONTANA</td>

   <td width="52">-0.2</td>

   <td width="60">-1.0</td>

   <td width="60">0.6</td>

  </tr>

  <tr>

   <td width="98">ALASKA</td>

   <td width="52">0.0</td>

   <td width="60">-0.8</td>

   <td width="60">0.8</td>

   <td width="105">NEBRASKA</td>

   <td width="52">0.8</td>

   <td width="60">0.4</td>

   <td width="60">1.2</td>

  </tr>

  <tr>

   <td width="98">ARIZONA</td>

   <td width="52">0.0</td>

   <td width="60">-1.1</td>

   <td width="60">1.1</td>

   <td width="105">NEVADA</td>

   <td width="52">-0.1</td>

   <td width="60">-0.8</td>

   <td width="60">0.5</td>

  </tr>

 </tbody>

</table>


