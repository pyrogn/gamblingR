# gamblingR

## Open Report File

1.  Clone `git clone --filter=tree:0 https://github.com/pyrogn/gamblingR.git` (only last commit, because data diff may be huge).
2.  Open [report/CSR.html](report/CSR.html) to read it (locally in webbrowser, not in GitHub)

## If you want to run it

1.  Install dependencies from [renv.lock](renv.lock). [How-to](https://rstudio.github.io/renv/reference/restore.html)
2.  Run [R/simulation.R](R/simulation.R) to generate new random data. Edit [R/constants.R](R/constants.R) to change simulation parameters.
3.  Render [report/CSR.qmd](report/CSR.qmd) into html

## Bonus (WIP)

**warning**: large stdout!!! Be on guard.

1.  Run `Rscript bonus/bonus.R`

## TODO

Important:

-   [x] Write 5 strategies with decent logic
-   [x] Write functions for plots
-   [x] Make readable html
-   [x] Add some comments, make code a bit more readable (so far so good)

Less important:

-   [ ] Add validators to strategies
-   [ ] Pass strategy names to plots
-   [ ] Add strategy based on Bayesian statistics
