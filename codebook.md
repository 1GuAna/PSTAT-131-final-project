# Codebook

**Project:** Signal Attenuation in Generative Data Synthesis
**Course:** PSTAT 131/231 — Introduction to Machine Learning

This codebook documents every column in both datasets used in this project. The
two datasets share an identical variable structure apart from their identifier
column, which is dropped before any analysis.

---

## Datasets

| Dataset | File | Rows | Role |
|---|---|---|---|
| Synthetic (CTGAN) | `train.csv` | 630,000 (sampled to 20,000) | Primary modeling dataset |
| Original (formula) | `Exam_Score_Prediction.csv` | 20,000 | Comparison arm |

Both datasets are synthetic. The original dataset was produced from a weighted
formula over the predictors listed below; the competition dataset was generated
by a deep generative model (CTGAN) trained on that original dataset. Neither
contains real student records or personally identifiable information.

---

## Variables

### Identifier (dropped before analysis)

| Variable | Type | Values | Description |
|---|---|---|---|
| `id` | Integer | 0 – 629,999 | Row identifier in the synthetic dataset. Removed in `01_load_sample.R`. |
| `student_id` | Integer | 1 – 20,001 | Row identifier in the original dataset. Removed in `01_load_sample.R`. |

### Numeric predictors

| Variable | Type | Range | Mean | Description |
|---|---|---|---|---|
| `age` | Integer | 17 – 24 | 20.55 | Student age in years. |
| `study_hours` | Continuous | 0.08 – 7.91 | 4.00 | Self-reported average daily study time, in hours. |
| `class_attendance` | Continuous | 40.6 – 99.4 | 71.99 | Percentage of scheduled classes attended. |
| `sleep_hours` | Continuous | 4.1 – 9.9 | 7.07 | Average nightly sleep duration, in hours. |

### Categorical predictors

| Variable | Type | Levels | Description |
|---|---|---|---|
| `gender` | Nominal | `female`, `male`, `other` | Self-reported gender. |
| `course` | Nominal | `b.com`, `b.sc`, `b.tech`, `ba`, `bba`, `bca`, `diploma` | Degree program in which the student is enrolled. |
| `internet_access` | Binary | `no`, `yes` | Whether the student has internet access at home. |
| `sleep_quality` | Ordered categories | `poor` < `average` < `good` | Self-rated quality of sleep. Stored as an unordered factor with levels in this order (see note below). |
| `study_method` | Nominal | `self-study`, `online videos`, `group study`, `mixed`, `coaching` | Primary method of study. |
| `facility_rating` | Ordered categories | `low` < `medium` < `high` | Student's rating of institutional facilities. Stored as an unordered factor with levels in this order. |
| `exam_difficulty` | Ordered categories | `easy` < `moderate` < `hard` | Perceived difficulty of the examination paper. Stored as an unordered factor with levels in this order. |

> **Note on ordered variables.** `sleep_quality`, `facility_rating` and
> `exam_difficulty` have a natural ordering, and their factor levels are stored
> in that order so that plots and tables display sensibly. They are deliberately
> **not** declared as `ordered` factors in R: `recipes::step_dummy()` applies
> polynomial contrasts to ordered factors, which would produce linear and
> quadratic terms rather than the indicator variables intended here.

### Outcome

| Variable | Type | Range | Mean | Description |
|---|---|---|---|---|
| `exam_score` | Continuous | 19.599 – 100.0 | 62.51 | Final examination score. **Censored at both bounds:** values are clipped at a lower limit of 19.599 and an upper limit of 100, producing point masses at each end of the distribution. |

---

## Missing data

Neither dataset contains any missing values in any column. This is verified in
`02_eda.R` and reported in §2.2 of the report. The complete absence of missingness
is itself a property of synthetic generation rather than of real educational data,
and is discussed as such in the report.

---

## Derived variables

No derived or engineered variables enter the primary modeling pipeline. All
feature transformations are handled inside the `recipes` object in
`03_split_recipe.R`:

| Step | Applied to | Purpose |
|---|---|---|
| `step_dummy()` | All nominal predictors | Indicator (k − 1) encoding for categorical variables. |
| `step_normalize()` | All numeric predictors | Centering and scaling, required by elastic net and k-nearest neighbors. |

---

## Sources

Kaggle. *Playground Series — Season 6, Episode 1: Predicting Student Test Scores.*
Kaggle, 2026. https://www.kaggle.com/competitions/playground-series-s6e1

Bedmutha, K. S. *Exam Score Prediction Dataset.* Kaggle, 2025.
https://www.kaggle.com/datasets/kundanbedmutha/exam-score-prediction-dataset
