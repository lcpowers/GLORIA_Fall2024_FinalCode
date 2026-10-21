# GLORIA Alpine Plant Monitoring — Scientific Reports Revision

Revision of a Scientific Reports paper on GLORIA alpine plant monitoring across 5 US
Rocky Mountain parks. Code uses GNP/YNP/RMN/GSD/PEC; figures and text use
GLAC/YELL/ROMO/GRSD/PECO (see "Park encoding" below).

This file is a working-session handoff, not permanent documentation — prune or
rewrite sections as they go stale. Last updated 2026-10-20.

## Where things live
- Repo: `~/Library/CloudStorage/OneDrive-UCB-O365/GLORIA/SciReports/Revisions/GLORIA_Fall2024_FinalCode`
  (back on OneDrive; the earlier move to `~/Projects` was undone). Remote:
  `git@github.com:lcpowers/GLORIA_Fall2024_FinalCode.git`, branch main. Only this
  folder is a git repo; other local copies were de-gitted.
- Heavy runs (climate-variable loops, dredges) happen on the lab server via RStudio
  Server (128 cores, 252 GB RAM, usually idle). Use `makeCluster(spec = 20)` or so.
  The server copy of the scripts must be synced/pulled before running after local edits.
- Word docs in `Revisions/` are backed up every 15 min by a launchd job
  (`~/Backups/backup_revisions_docs.sh`, `~/Library/LaunchAgents/com.lipo6619.backup-revisions-docs.plist`,
  log `~/Backups/backup.log`). **Unverified**: it needs Full Disk Access for
  `/bin/bash` to read OneDrive; check the log shows copies.

## How this user likes to work (read before editing anything)
- **Explain, then ask, then edit.** Show the proposed change (diff in chat or via the
  Edit tool) and wait for an explicit OK. One file at a time when they're checking
  results ("just the vasc plant cover one for now so I can check it").
- **Simplest version of what reviewers ask for.** Don't add analyses (e.g.
  ordinations) beyond what's needed; this user pushes back on scope creep.
- **Match their code style**: `lmer(..., data = ..., REML = TRUE)`, `%>%` and `|>`
  both used, inline code, **no short helper functions**, comment density like the
  surrounding code.
- **Verify computationally** before stating anything quantitative; run against real
  data in the scratchpad rather than reasoning from code.
- The user edits files in RStudio while we talk — re-read before editing; RStudio
  strips trailing whitespace on save, which breaks exact-match edits.
- Plain, direct writing. "The notes" = this file.

## Decisions made (don't relitigate)
- **Random effects**: keep correlated `(x|sciname)` RE in species-level models (as
  originally); add a methods caveat that several fits are singular and
  species-level slopes should be interpreted cautiously. Fixed effects barely change
  with `||`. Reply email to Dan drafted at `~/Desktop/reply_to_Dan.txt`.
- **Exclusions**: annuals (`perennating == "annual"`, 2 Eriogonum spp.) and trees
  (3 conifers) are excluded from all models (species-level `_B`, richness, cover).
- **Vascular cover** comes from summed species cover
  (`Scripts/1_DataPrep/VegetationData/surfaceCoverfromSpecCover_final.R`, averaged
  to summit-aspect), NOT GLORIA top-cover (that data is bad; `BAD_surfaceCover_final.R` deleted).
- **"Simple models" promised to reviewers** for richness and total cover = the
  models from the full model-selection workflow (steps 1–4). The by-park
  `~ 0 + park` direction tests are reported in addition.
- `.cwd` excluded from the climate PCA on purpose (CWD = PET − AET). `gdd` is a day
  count on purpose (rename in text, keep code).
- Cover vs abundance: same thing; the drafted reviewer response already handles it.
- Ignore the `pchangecover < 100` filter in `Manu figures.Rmd` (leave as is).

## Current state of the two community-level model files
Both in `Scripts/2_ModelSelection/`, same structure. Data chunk → reviewer
direction-test chunks → model selection steps 1–4 (RE structure with REML →
climPC dredge with ML → 1,119-model explicit climate-variable loop with ML → RE
recheck, save best model) → plotting chunk. Chunks addressing reviewer points have
a comment naming the reviewer/point.

### `ModelSelection_SpeciesRichness.Rmd`
- Data: `mod.dats` = one row per quadrat × transition (497 rows), built from
  `Data/IntermediateData/all_1x1veg_w0s_final.csv`. Columns include total, forb and
  graminoid richness (start/end/change), `gains`, `losses`, `shared`, `turnover`
  (Jaccard), `bc.dissim` (Bray–Curtis on cover). 7 rows NA for turnover/bc (no
  species either survey).
- Best RE: `(1|transition)`; quadrat RE is singular (variance 0).
- Best explicit-climate model (saved `Results/climVar_models_richness_2026-10-20.rds`):
  `end.nspp ~ start.nspp*(sitelat + mean.tmean + min.wy.ppt + max.gs.ppt + mean.snowdays) + (1 | transition)`.
  9 models within ΔAICc 2: mean.tmean and min.wy.ppt in all; snow/GDD metric and
  gs.ppt metric not well separated → note in methods.
- Reviewer chunks: `direction_test_richness` (total), `direction_test` (forb/gram),
  `turnover` (gains, losses, Jaccard, Bray–Curtis by park).
- Plot chunk: predicted change (predicted end − start, unscaled refit) at actual data
  points vs start richness, Manu-figures park colours. **Forest plot not yet copied
  here** (waiting on user's OK of the cover version).
- Bottom: `## SCRATCH` NMDS ordination (for learning only, not manuscript).

### `ModelSelection_VascPlantCover.Rmd`
- Data: summit-aspect × transition (135 rows after na.omit; 2 PEC rows lack climate),
  from `Data/AnalysisData/vascplantcover_final.csv`, which now also has forb and
  graminoid cover columns (rerun chain: `surfaceCoverfromSpecCover_final.R` →
  `PlantCover_AnalysisDataPrep.Rmd`).
- Best RE `(1|transition)`. Best model (saved `Results/climVar_models_plantCover_2026-10-20.rds`):
  `end.plantCover ~ start.plantCover*(sitelat + mean.wy.ppt + mean.gs.ppt + max.snowdays) + (1|transition)`.
  Chunk 4's formulas are hand-typed — update if the loop picks different variables.
- Reviewer chunks: `direction_test_totalcover`, `direction_test` (forb/gram cover).
- Plot chunk: predicted-change-at-data-points plot (park colours, `geom_smooth`) +
  forest plot of scaled coefficients (`broom.mixed::tidy`, per 1 SD).
  **Pending**: add `scale_y_discrete(expand = c(0,1))` so the top label isn't clipped,
  then copy the forest plot to the richness file.

### Results so far (for writing)
- Total richness: no overall change; declines at GRSD and PECO (~−1 sp/quadrat).
- Forb richness down at GRSD & PECO; graminoid richness no change anywhere.
- Total cover: no overall change; PECO declined (CI −25.8 to −7.6 %).
- Forb cover down at ROMO & PECO; YELL borderline up.
- Turnover (Jaccard and Bray–Curtis) lowest at GLAC — supports the Discussion's
  "weaker turnover at GLAC".
- Cover best model: wy.ppt positive, gs.ppt and snow days negative (main effects and
  × start); start × sitelat not significant. Predicted change splits into a flat
  branch and a steeply declining branch above ~15 % start cover — likely
  transition-specific climate; check by colouring by `tr.num` or `max.snowdays`.

## Plotting lessons (apply to Manu figures too)
- Don't draw prediction lines that hold all other predictors at their medians while
  varying one climate/latitude variable — latitude and climate covary, so the lines
  extrapolate to unrealistic combinations and contradict the points. Predict at the
  actual data points instead. **The forb wy.ppt figure in `Manu figures.Rmd`
  (Fig 13 area, ~line 700) has this problem**: lines disagree with points (High line
  to +2.3 where points max ~0.7). Line ~732 there already has an alternative approach.
- Refit on unscaled data with `lmer(formula(mod), data = <unscaled>, REML = TRUE)`;
  `update()` fails because the saved call references `best.ExplClimVar.mod.str`.

## Park encoding (from `Scripts/3_Results/Manu figures.Rmd`, lines 28–54)
`park2` via case_when GNP→GLAC, YNP→YELL, RMN→ROMO, GSD→GRSD, PEC→PECO;
levels `rev(c("GLAC","YELL","ROMO","GRSD","PECO"))`;
`park.pal = c(GLAC='#B07AA1',YELL='#1170AA',ROMO='#59A14F',GRSD='#E6AB02',PECO='#C85200')`.

## Reviewer response status (turnover / richness / cover points)
- R1 minor (richness) & R2 general (richness, gains/losses, community dynamics):
  covered by richness/cover model selection + direction tests + turnover chunk.
  Response text should say models were "built with the same model selection procedure
  as the species-level models", not "at each park".
- R1 #3 (turnover): gains/losses + Bray–Curtis by park. No ordination in manuscript.
  Consider rewording "turnover" where the text means rare-up/common-down shifts.
- R2 line 246 / CP12.1: acknowledge regression to the mean (low-cover species can't
  fall far) in a sentence.
- Still "COME BACK" in the response doc: abstract numbers, Discussion restructure,
  R1 #6, #7, #8 (ask Dan), #11, Figs S12–S14, line 357, 394–398, R2 line 359.

## Older open items (from before 2026-10-20; status not rechecked)
- `PlantCover_AnalysisDataPrep.Rmd` PEC block reads nonexistent
  `allparks_dailyT_final.csv` (may have been fixed since — the file ran today).
- `ModelSelection_1x1m_SpeciesLevel.Rmd`: phase 1 RE search now compares `|` and `||`
  (63 models); phase 2 and `_B` still use `||` — switch to `|` per RE decision?
  Chunk 4/5 hardcoded variable names (`"min.gdd"` in `fixed=`) may be stale.
- `Manu figures.Rmd`: dated readRDS paths, `gdd.forbs`→`snow.forbs`, snowdays x label,
  jitter height.
- `ModelSelection_SpeciesofInterest.Rmd` not reviewed.
- Uncommitted changes — nothing has been committed this session.
