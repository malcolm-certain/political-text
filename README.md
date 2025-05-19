# Political Text Replication Package

This replication package is for internal use on the political text research project. Its purpose is to generate basic descriptive summaries of RD data on city council political speech for a given event.

## Running the Code

Take the following steps to run this replication package.

- Open *Political Text Replication Package.RProj* in RStudio.
- Use ```renv::restore()``` to install the necessary packages from the lockfile.
- Open *RD_data_figures_tables.R* and run it.
- Load *meetings_full.parquet* into RStudio as a data.table. 
- Pipe your data table into the ```run_all``` function along with your desired parameters.

This will output a balance table, a table of t-tests for each of your target words, and a binned scatterplot of each of your target words over time.

## Example

Below is example code using George Floyd as the event.

```
    dt <- setDT(read_parquet("meetings_full.parquet"))
    dt |> run_all(
        start = "2020-04-25"
        , end = "2020-06-25"
        , treatment = "2020-05-25"
        , targets = c("black", "race", "police", "cops", "george", "floyd")
        , topic = "george_floyd"
    )
```