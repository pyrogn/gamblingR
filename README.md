# gamblingR (WIP)

## Open Report File

1.  Clone `git clone --filter=tree:0 https://github.com/pyrogn/gamblingR.git` (only last commit, because data diff may be huge).
2.  Open [report/CSR.html](report/CSR.html) to read it (locally, not in GitHub)

## If you want to run it

1.  Install dependencies from [renv.lock](renv.lock). [How-to](https://rstudio.github.io/renv/reference/restore.html)
2.  Run [R/simulation.R](R/simulation.R) to generate new random data
3.  Render [report/CSR.qmd](report/CSR.qmd) into html

## TODO

Important:

-   [x] Write 5 strategies with decent logic
-   [ ] Write functions for plots
-   [ ] Make readable html
-   [ ] Add some comments, make code a bit more readable

Less important:

-   [ ] Add validators to strategies
-   [ ] Pass strategy names to plots
-   [ ] Add strategy based on Bayesian statistics
