# East Java Investment Case — Skills Development for Adolescent Girls

Cost-effectiveness and investment case analysis for UNICEF Indonesia, supporting
the East Java provincial government's **Double Track / Tambahan Keterampilan**
programme and its **Skills4Girls** component. The analysis runs Diagnosis →
Theory of Change → Effectiveness → Cost-effectiveness, with child marriage and
early pregnancy — the two dominant exclusion pathways identified in the ToR —
as a central diagnostic focus.

## Slides

The full deck lives at [`code/06_slides_en.qmd`](code/06_slides_en.qmd); the
rendered outputs (after running the pipeline below) are at
`output/06_slides_en.html` and `output/06_slides_en.pdf`.

## What this repo produces

A reveal.js slide deck built entirely from reproducible R scripts, covering:

1. **Diagnostic** — the NEET gender gap, its concentration by district and
   wealth quintile, the digital divide, and the child-marriage / early-pregnancy
   exclusion pathways (with formal survey-weighted hypothesis tests).
2. **Evidence → Theory of Change** — a curated evidence base (51 studies) and
   a φ-transportability method for bringing global RCT effects to East Java.
3. **Projections** — returns to schooling estimated two independent ways
   (IFLS5 wages; SUSENAS 2024 consumption), both showing convex returns.
4. **Costing & ROI** — a sourced, editable cost-benefit model with an honest,
   range-based BCR (not a false-precision point estimate).

## Data sources

| Source | Use |
|---|---|
| **SUSENAS Maret 2024** (BPS), East Java (province 35) | Core diagnostic: NEET, schooling, child marriage, early pregnancy, wealth quintiles, digital access |
| **IFLS5** (2014/15) | Wage-based Mincer return (SUSENAS collects no wage data) |

SUSENAS files are **not included** in this repo (BPS microdata licensing) —
place them under `data/Susenas 2024/` as described below before running the
pipeline.

```
data/Susenas 2024/
  ssn202403_kor_ind1.dta      # individual KOR — demographics, education, employment, ICT
  ssn202403_kor_ind2.dta      # maternal/pregnancy block (Blok XV)
  ssn24_mar_kp_blok43.dta     # KAPITA — per-capita expenditure
  SUSENAS_24.dta              # merged household file (626 cols), used as a fallback lookup
```

## Pipeline

Run in order from the project root:

```bash
# 1. Clean & construct the analysis sample
Rscript code/01c_clean_susenas24.R

# 2. Descriptive diagnostics
Rscript code/02_descriptive.R
Rscript code/03b_figures_en.R
Rscript code/03g_key_gaps_en.R          # f1a — the headline transition-gap chart
Rscript code/03h_marriage_pathway_en.R  # f9  — child marriage → NEET/school/employment
Rscript code/03i_marriage_poverty_tests.R  # f11 + formal tests — poverty → marriage
Rscript code/03j_pregnancy_pathway.R    # f12/f13 + formal tests — early pregnancy

# 3. Returns to education
Rscript code/01b_clean_ifls.R
Rscript code/04_mincer_ifls.R           # f8  — wage-based Mincer (IFLS5)
Rscript code/06_mincer_susenas.R        # f10/f14 — consumption-based Mincer (SUSENAS)

# 4. Costing & cost-benefit
Rscript code/08_bcr_estimation.R

# 5. Render the deck
quarto render code/06_slides_en.qmd
cp code/06_slides_en.html output/06_slides_en.html

# 6. Export a PDF (Chrome headless — the ?print-pdf query string only
#    works when rendered by a real browser engine, hence Chrome not wkhtmltopdf)
"/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" \
  --headless=new --disable-gpu --no-pdf-header-footer \
  --run-all-compositor-stages-before-draw --virtual-time-budget=90000 \
  --print-to-pdf="output/06_slides_en.pdf" \
  "file://$PWD/code/06_slides_en.html?print-pdf"
```

## Repo structure

```
code/     analysis scripts (numbered by pipeline stage) + the slide deck (.qmd)
R/        shared helpers: utils.R (save_fig, ensure_dirs, weighted quantiles),
          theme.R (brand colour palette), variable_mapping.R (SUSENAS codebook)
data/     SUSENAS + IFLS5 microdata (not committed — see Data sources)
output/
  figures/      all f*.png charts referenced by the deck
  tables/       intermediate descriptive tables
  models/       regression outputs (Mincer, marriage/pregnancy formal tests)
  projections/  cost-benefit assumptions, costing table, BCR scenarios
  06_slides_en.html / .pdf   the rendered deck
```

## Key methodological notes

- **NEET** is built from main activity (`R704`) and school attendance (`R610`).
- **Child marriage / early pregnancy → outcome** regressions are
  survey-weighted linear-probability models with PSU-clustered SEs,
  controlling for age, urban/rural, and (where the outcome isn't poverty
  itself) per-capita-expenditure poverty. See the Mincer/regression
  specification slide in the deck for the full formal write-up.
- **Returns to schooling** are estimated on two independent outcomes —
  IFLS5 wages and SUSENAS per-capita consumption — using the same
  quadratic-in-schooling specification, run separately by sex, so the two
  results are directly comparable and both show convex returns.
- **The BCR is a range, not a point.** The Low scenario carries the
  φ-adjusted effect's null (CI includes zero) deliberately, so the model
  does not overstate certainty the underlying evidence doesn't support.

## Data Source

SUSENAS and IFLS5 microdata are used under their respective institutional
licenses (BPS; RAND/SurveyMETER) and are not redistributed here. Code and
non-microdata outputs (figures, tables, the deck) are the author's own work.
