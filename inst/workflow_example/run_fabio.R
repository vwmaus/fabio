
# package creation (temp)
library(devtools)
devtools::document()
devtools::check()
devtools::load_all()

# load package ------------------------------------------------------------
library(fabio)

# read data ---------------------------------------------------------------
FAO_MRIO_0_read_raw_FAO_data()

# data preparation --------------------------------------------------------
FAO_MRIO_1a_data_preparation()
FAO_MRIO_1b_data_preparation()
FAO_MRIO_1c_estimate_missing_CBS()
FAO_MRIO_1d_consolidate_CBS_and_BTD()
FAO_MRIO_1e_start_values_BTD()
FAO_MRIO_1f_BTD_balancing()

# supply and use ----------------------------------------------------------
FAO_MRIO_2a_supply()
FAO_MRIO_2b_use()
FAO_MRIO_2c_trade_linking_SUP()
FAO_MRIO_2d_trade_linking_USE()

# MRIO analysis -----------------------------------------------------------
FAO_MRIO_3a_mriot()
FAO_MRIO_3b_A()
FAO_MRIO_3c_L_series()

# footprints --------------------------------------------------------------
# hier kommt fabio_footprint()

# See sections 'The INDEX file' and 'Package subdirectories' in the
# 'Writing R Extensions' manual.