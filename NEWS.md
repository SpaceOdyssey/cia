# cia 1.1.0

# cia 1.0.1

## Minor changes

* InitPartition is the new way to initialise a state to be used in SampleChains.
* PlotScoreTrace now returns a ggplot object.
* R_hat is calculated for each chain.
* Added ability to change summary output. Now you can pass to the 'stat_names' parameter a character vector including Mean, Median, SD, quartiles (e.g. 'Q25'), R_hat, and N_eff.
* Included experimental CoupledPartitionMCMC.
