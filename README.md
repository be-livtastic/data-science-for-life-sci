# Statistics and R for Life Sciences 🧬📊

How do we move from raw biological measurements to evidence-backed scientific conclusions?

This repository documents a hands-on learning journey through **statistical thinking and data analysis in R**, using real biological datasets. The focus is on understanding *why* statistical methods work, not just how to run them.

Rather than treating statistics as abstract math, this project approaches it as a practical toolkit for answering biological questions with data.

---

## 📚 Course Overview

This work follows the structure and materials of a self-paced course on **statistics and R programming for genomics and life science research**.

It introduces:

* Probability distributions and randomness
* Statistical inference from samples to populations
* Simulation-based hypothesis testing
* Reproducible data analysis workflows in R

Concepts are reinforced through code that:

1. Imports and cleans biological datasets
2. Explores patterns using visualization
3. Tests scientific hypotheses using statistical models

---

## 🧪 Core Concepts Demonstrated

* **Exploratory Data Analysis (EDA)**
  Summarizing and visualizing data to understand structure, spread, and outliers.

* **Statistical Inference**
  Estimating population properties (like mean bodyweight) from limited samples.

* **Null Hypothesis Testing**
  Using simulation, permutation tests, and t-tests to evaluate whether observed differences are likely due to chance.

* **Central Limit Theorem**
  Showing how averages of samples become approximately normally distributed as sample size increases.

* **Confidence Intervals**
  Quantifying uncertainty around estimated means.

* **Power Analysis**
  Investigating how sample size affects the ability to detect real biological effects.

* **Monte Carlo Simulation**
  Repeated random sampling to approximate theoretical statistical distributions.

---

## 🧬 Example Biological Question

> Does a high-fat diet significantly change the average bodyweight of mice compared to a standard chow diet?

The analysis:

1. Separates mice into control and treatment groups
2. Computes the observed difference in mean bodyweight
3. Builds a **null distribution** by random reassignment of diet labels
4. Calculates a **p-value** to measure statistical significance

# Statistics and R for Life Sciences 🧬📊

How do we move from raw biological measurements to evidence-backed scientific conclusions?

This repository documents a hands-on learning journey through **statistical thinking and data analysis in R**, using real biological datasets. The focus is on understanding *why* statistical methods work, not just how to run them.

Rather than treating statistics as abstract math, this project approaches it as a practical toolkit for answering biological questions with data.

---

## 📚 Course Overview

This work follows the structure and materials of a self-paced course on **statistics and R programming for genomics and life science research**.

It introduces:

* Probability distributions and randomness
* Statistical inference from samples to populations
* Simulation-based hypothesis testing
* Reproducible data analysis workflows in R

Concepts are reinforced through code that:

1. Imports and cleans biological datasets
2. Explores patterns using visualization
3. Tests scientific hypotheses using statistical models

**Instructor**

* **Rafael A. Irizarry, PhD** – Professor of Biostatistics, Harvard T.H. Chan School of Public Health

---

## 🧪 Core Concepts Demonstrated

* **Exploratory Data Analysis (EDA)**
  Summarizing and visualizing data to understand structure, spread, and outliers.

* **Statistical Inference**
  Estimating population properties (like mean bodyweight) from limited samples.

* **Null Hypothesis Testing**
  Using simulation, permutation tests, and t-tests to evaluate whether observed differences are likely due to chance.

* **Central Limit Theorem**
  Showing how averages of samples become approximately normally distributed as sample size increases.

* **Confidence Intervals**
  Quantifying uncertainty around estimated means.

* **Power Analysis**
  Investigating how sample size affects the ability to detect real biological effects.

* **Monte Carlo Simulation**
  Repeated random sampling to approximate theoretical statistical distributions.

---

## 🧬 Example Biological Question

> Does a high-fat diet significantly change the average bodyweight of mice compared to a standard chow diet?

The analysis:

1. Separates mice into control and treatment groups
2. Computes the observed difference in mean bodyweight
3. Builds a **null distribution** by random reassignment of diet labels
4. Calculates a **p-value** to measure statistical significance

<img width="867" height="516" alt="image" src="https://github.com/user-attachments/assets/33fd37eb-2d1b-4ec3-b76a-48af6f85ec08" />


---

## 📈 From Small Samples to Reliable Estimates

By repeatedly sampling from a known population, the code demonstrates:

* Small samples → highly variable estimates
* Larger samples → tighter, more reliable averages

<img width="867" height="516" alt="image" src="https://github.com/user-attachments/assets/91299eb4-d82f-4c07-b91b-df551b0772b0" />


This provides an intuitive demonstration of the **Central Limit Theorem** in action.

---

## 🔍 Checking Model Assumptions

Before applying parametric tests, the distribution of bodyweights is assessed using:

* Histograms
* Q–Q plots against a theoretical normal distribution

<img width="867" height="516" alt="image" src="https://github.com/user-attachments/assets/5ca7d131-4255-42e7-abc0-f26afab8898b" />


This step ensures that statistical tests based on normality are reasonable approximations.

---

## 🧮 Comparing Groups with a t-test

The project computes **t-statistics** in three ways:

1. From simulation under the null hypothesis
2. Manually from sample statistics
3. Using R’s built-in `t.test()` function

All approaches converge on similar conclusions about the diet effect.

---

## 📏 Confidence Intervals in Practice

Repeated sampling shows that about 95% of constructed intervals contain the true population mean, illustrating what a *95% confidence level* really means.

<img width="867" height="516" alt="image" src="https://github.com/user-attachments/assets/d3431c3b-5a86-470d-9ca9-d2911332184a" />

---

## ⚖️ Statistical Power and Sample Size

Simulations estimate how often a true diet effect would be detected for different sample sizes.

Key takeaway:

> Increasing sample size dramatically improves the probability of detecting real biological differences.

<img width="867" height="516" alt="image" src="https://github.com/user-attachments/assets/fa8d1203-7f00-4cca-b743-8fa67fb93924" />


---

## 🔁 Simulation-Based Inference

Two non-parametric approaches are implemented:

* **Permutation tests**: shuffle labels to test for group differences without distributional assumptions
* **Monte Carlo simulations**: generate empirical t-statistic distributions and compare them to theoretical normal and t distributions

<img width="867" height="516" alt="image" src="https://github.com/user-attachments/assets/f87b81c8-d215-4c50-81df-016fb355d3cd" />


---

## 📦 Data Package

* **Package:** `dagdata`
* **Title:** Data Analysis for Genomics datasets (PH525x series)
* **Version:** 1.1
* **Authors:** Michael Love & Rafael Irizarry
* **License:** GPL-3

Datasets include:

* Mouse bodyweights under different diets
* Simulated and real phenotype measurements

---

## 🛠 Tools & Resources

* **R** (statistical computing environment)
* **RStudio** (IDE for reproducible analysis)
* **Key R packages:** `dplyr`, `rafalib`, `downloader`, `devtools`
* **Textbook:** *Data Analysis for the Life Sciences*
* **Course labs & reference code:**

  * [https://github.com/genomicsclass/labs](https://github.com/genomicsclass/labs)
  * [https://genomicsclass.github.io/book/](https://genomicsclass.github.io/book/)

---

## 🎯 Project Goal

To build a practical, reproducible workflow for analyzing biological data using R, while developing an intuitive understanding of:

* variability
* uncertainty
* statistical significance
* and experimental design

This repository serves as both a study notebook and a technical reference for applying statistical reasoning to life science data.

---

## 🚀 Next Steps / Ideas for Extension

* Apply the same workflow to gene expression or sequencing data
* Compare parametric vs non-parametric methods on noisier datasets
* Package the analysis into an R Markdown report for full reproducibility

---

If you were designing a biological experiment from scratch, how large would your sample need to be to confidently detect the effect you care about?

---
