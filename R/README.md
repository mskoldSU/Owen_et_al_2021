# R scripts

- `draw_figs.R` Draws and saves manuscriot figures (figure S4 is drawn in `prepare_data.R`)
- `functions.R` Contains assorted helper functions for `draw_figs.R` and `tables.R`
- `impute_data.R` Imputes missing data for the main stations (1032, 1036, 1041) and saves in `clean_data/imputed_data.csv`. This is loaded and prepared for trend analysis by the `make_indices` function in `functions.R`.
- `prepare_data.R` This cleans and aggregates raw data (not included in repo) and saves into `clean_data/daily_data.csv`. Also draws figure S4 that requires data at a finer scale.
- `tables.R` Prepares the tables.

The output from `prepare_data.R` and `impute_data.R` is already available in `clean_data/`, it is sufficient to source `draw_figs.R` and `tables.R` to reproduce figures and tables.