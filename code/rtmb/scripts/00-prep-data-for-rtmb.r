
# Nicholas Ducharme-Barth
# 2026/02/06
# R code to prepare baseline data for RTMB model
#
# OUTPUTS:
# - catch-data.csv: Quarterly catch data by fleet with uncertainty (SE)
# - cpue-data.csv: Normalized CPUE data for index fleet (fleet 15)
# - selex_l.csv: Selectivity-at-length curves from MFCL for all fisheries
# - sel-config.csv: Selectivity length bin configuration parameters
#
# DATA STRUCTURE:
# All output data tables use a standardized format with year/month/timestep identifiers.
# The catch data maintains MFCL fleet
# definitions and unit distinctions (metric tons vs. thousands of fish).
#
# MFCL FILES USED:
# - bet.frq:         Frequency file containing catch-at-length distributions by quarter
# - bet.ini:         Initialization file with model structural parameters (seasons=4)
# - 10.par:          Parameter file with estimated biological and selectivity parameters
# - plot-10.par.rep: Report file with diagnostic information
#
# Copyright (c) 2026 Nicholas Ducharme-Barth
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

#_____________________________________________________________________________________________________________________________
# Load required packages
	library(data.table)
	library(magrittr)
	library(FLR4MFCL)
    library(frqit)

#_____________________________________________________________________________________________________________________________
# Define relative project paths
	proj_dir = this.path::this.proj()
	dir_model = file.path(proj_dir,"model-files")
    dir_base_mfcl = file.path(dir_model,"mfcl","v11")
    dir_base_rtmb = file.path(dir_model,"rtmb","base-data")
    dir_helper_fns_mfcl = file.path(proj_dir,"code","mfcl","helper-fns")

#________________________________________________________________________________________________________________________________________________________________________________________________________
# Source helper functions from MFCL module
    sapply(file.path(dir_helper_fns_mfcl,(list.files(dir_helper_fns_mfcl))),source)

#_____________________________________________________________________________________________________________________________
# Read baseline MFCL model files
# These objects form the foundation for all downstream RTMB data preparation.
# - base_frq: Frequency object (S4 class) containing quarterly catch and catch-at-length distributions
# - base_ini: Initialization object with structural parameters (model dimensions, seasons, etc.)
# - base_par: Parameter object with estimated values for biological and selectivity parameters
# - base_rep: Report object with diagnostic quantities (fits, residuals, convergence info)
    base_frq = parse_frq(file.path(dir_base_mfcl,"bet.frq"))
    base_ini = read.MFCLIni(file.path(dir_base_mfcl,"bet.ini"), nseasons=4)
    base_par = read.MFCLPar(file.path(dir_base_mfcl,"10.par"), first.yr=1952)
    base_rep = read.MFCLRep(file.path(dir_base_mfcl,"plot-10.par.rep"))

#_____________________________________________________________________________________________________________________________
# Prepare catch data for RTMB
#
# Workflow:
# 1. Create a time-series table mapping calendar years/months to model timesteps
#    - MFCL operates on quarterly cycles (months 2, 5, 8, 11 = Q1-Q4)
#    - Each quarter is assigned a sequential timestep (ts) for integration with RTMB dynamics
# 2. Extract catch by fishery/fleet from the MFCL frequency file using frqit::cateffpen()
# 3. Standardize units: convert catch in numbers (units==2) to thousands of fish
# 4. Merge with timestep table to add calendar-to-timestep mapping
# 5. Add metadata columns (metric="catch", standard SE=0.01)
# 6. Remove fleet 15 (MFCL index/survey fleet)
#
# Output columns:
# - year:     Calendar year (1952-2018)
# - month:    Fishing quarter month (2, 5, 8, 11 = Jan-Mar, Apr-Jun, Jul-Sep, Oct-Dec approx.)
# - ts:       Model timestep number (1-268 for 67 years * 4 quarters)
# - fishery:  MFCL fleet index (1-14, excluding 15 which is survey)
# - metric:   Data type identifier ("catch")
# - units:    Unit code (1=metric tons, 2=thousands of fish)
# - value:    Catch quantity in specified units
# - se:       Standard error in log-space (sqrt(log(1 + cv^2))); approximately CV for small SEs (currently fixed at 0.01 for all observations)
#
# Catch units by fishery:
# Fisheries 1-7, 15: Units=2 (thousands of fish)
# Fisheries 8-14:    Units=1 (metric tons)
# This reflects MFCL's mixed-unit data structure from different fishery monitoring systems.
#
# Time series table:
    ts_dt = data.table(expand.grid(year=min(range(cateffpen(base_frq)$year,na.rm=TRUE)):max(range(cateffpen(base_frq)$year,na.rm=TRUE)),month=c(2,5,8,11))) %>%
            .[order(year,month)] %>%
            .[,ts:=1:.N]
    
    
# Catch data table:
    catch_dt = as.data.table(cateffpen(base_frq)) %>%
                   .[,units:=c(2,2,2,2,2,2,2,1,1,1,1,1,1,1,2)[fishery]] %>%
                   .[units==2,catch:=catch/1000] %>%
                   merge(.,ts_dt,by=c("year","month")) %>%
                   .[,metric := "catch"] %>%
                   setnames(.,c("catch"),c("value")) %>%
                   .[,se:=0.01] %>%
                   .[,.(year,month,ts,fishery,metric,units,value,se)] %>%
                   .[fishery!=15] %>% # remove fleet 15 (index fleet)
                   .[order(fishery,year,month)]
    fwrite(catch_dt,file.path(dir_base_rtmb,"catch-data.csv"))

#_____________________________________________________________________________________________________________________________
# Prepare CPUE (catch-per-unit-effort) data for RTMB
#
# Workflow:
# 1. Extract fishery 15 (MFCL index/survey fleet) from the baseline frequency file
# 2. Calculate CPUE as catch-per-unit-effort (catch / effort)
# 3. Normalize CPUE to mean across all time periods (obs = cpue / mean(cpue))
# 4. Convert penalty column to coefficient of variation (CV)
# 5. Calculate log-space standard error (se_log) from CV
# 6. Merge with timestep table to maintain consistent temporal indexing
# 7. Standardize to output column structure matching catch data
#
# Output columns (matching catch data structure):
# - year:     Year component of timestep (same as ts from catch data for consistency)
# - month:    Month component
# - ts:       Model timestep number
# - fishery:  Fleet identifier (15 = survey/index fleet)
# - metric:   Data type identifier ("cpue")
# - units:    Unit code (1=metric tons, 2=thousands of fish)
# - value:    Normalized CPUE observation (cpue / mean(cpue))
# - se:       Standard error in log-space (sqrt(log(1 + cv^2))); approximately CV for small SEs
#
# Note: MFCL penalty column in frequency file represents 1/(2*CV^2) relationship.
#
# CPUE data table:
    cpue_dt = as.data.table(cateffpen(base_frq)) %>%
                   .[fishery==15] %>% # select survey fleet only
                   .[,cpue:=catch/effort] %>%
                   .[,cv:=1/sqrt(2*penalty)] %>%
                   .[,se_log:=sqrt(log(1+cv^2))] %>%
                   .[,obs:=cpue/mean(cpue,na.rm=TRUE)] %>%
                   merge(.,ts_dt,by=c("year","month")) %>%
                   .[,metric:="cpue"] %>%
                   .[,units:=2] %>%
                   setnames(.,c("obs"),c("value")) %>%
                   setnames(.,c("se_log"),c("se")) %>%
                   .[,.(year,month,ts,fishery,metric,units,value,se)] %>%
                   .[order(year,month)]
    fwrite(cpue_dt,file.path(dir_base_rtmb,"cpue-data.csv"))

#_____________________________________________________________________________________________________________________________
# Prepare length frequency composition data for RTMB
#
# Workflow:
# 1. Extract length composition data from MFCL frequency file using lnfrq()
#    - Returns wide-format data with length bins as columns (10, 12, 14, ... 198 cm)
#    - Observations are grouped by fishery-year-month
# 2. Reshape to long format (one row per fishery-year-month-length bin combination)
# 3. Merge with timestep table to add model timestep indexing consistent with catch/CPUE
# 4. Save as CSV in long format for RTMB model input
#
# Output columns in long format:
# - year:        Calendar year (1952-2018)
# - month:       Fishing quarter month (2, 5, 8, 11 = Jan-Mar, Apr-Jun, Jul-Sep, Oct-Dec approx.)
# - ts:          Model timestep number (1-268 for 67 years * 4 quarters)
# - fishery:     MFCL fleet index (1-15)
# - bin:         Length bin lower edge (cm)
# - value:       Observations in this length bin
# - week:        Temporal indicator within quarter (typically 1 for aggregated data)
#
# Notes:
# - MFCL data uses 2cm length bins (10, 12, 14, ... 198 cm)
# - Data from lnfrq() represents observed length frequency
# - Sum of values across bins per fishery-year-month instance gives total input sample size.
# - Divide bin values by total sample size to get proportions if needed for model input (currently left as counts for RTMB to handle).

#
# Length composition data table (reshaped to long format):
    len_comp_wide = as.data.table(lnfrq(base_frq)) %>%
                    na.omit(.)
    
    len_comp_dt = melt(len_comp_wide,
                       id.vars = c("year", "month", "week", "fishery"),
                       variable.name = "bin",
                       value.name = "value") %>%
                  as.data.table(.) %>%
                  .[,bin := as.numeric(as.character(bin))] %>%
                  merge(., ts_dt, by = c("year", "month")) %>%
                  .[, .(year, month, ts, fishery, bin, value, week)] %>%
                  .[order(fishery, year, month, bin)]
    
    fwrite(len_comp_dt, file.path(dir_base_rtmb, "comp_len_time.csv"))

#_____________________________________________________________________________________________________________________________
# Prepare weight frequency composition data for RTMB
#
# Workflow:
# 1. Extract weight composition data from MFCL frequency file using wtfrq()
#    - Returns wide-format data with weight bins as columns (1, 2, 3, ... 200 kg)
#    - Observations are grouped by fishery-year-month
# 2. Reshape to long format (one row per fishery-year-month-weight bin combination)
# 3. Merge with timestep table to add model timestep indexing consistent with catch/CPUE
# 4. Save as CSV in long format for RTMB model input
#
# Output columns in long format:
# - year:        Calendar year (1952-2018)
# - month:       Fishing quarter month (2, 5, 8, 11 = Jan-Mar, Apr-Jun, Jul-Sep, Oct-Dec approx.)
# - ts:          Model timestep number (1-268 for 67 years * 4 quarters)
# - fishery:     MFCL fleet index (1-15)
# - bin:         Weight bin (kg, integer 1-200)
# - value:       Observations in this weight bin
# - week:        Temporal indicator within quarter (typically 1 for aggregated data)
#
# Notes:
# - MFCL data uses 1 kg weight bins (1, 2, 3, ... 200 kg)
# - Data from wtfrq() represents observed weight frequency
# - Sum of values across bins per fishery-year-month instance gives total input sample size.
# - Divide bin values by total sample size to get proportions if needed for model input (currently left as counts for RTMB to handle).

#
# Weight composition data table (reshaped to long format):
    weight_comp_wide = as.data.table(wtfrq(base_frq)) %>%
                    na.omit(.)

    wt_comp_dt = melt(weight_comp_wide,
                      id.vars = c("year", "month", "week", "fishery"),
                      variable.name = "bin",
                      value.name = "value") %>%
                 as.data.table(.) %>%
                 .[,bin := as.numeric(as.character(bin))] %>%
                 merge(., ts_dt, by = c("year", "month")) %>%
                 .[, .(year, month, ts, fishery, bin, value, week)] %>%
                 .[order(fishery, year, month, bin)]

    fwrite(wt_comp_dt, file.path(dir_base_rtmb, "comp_wt_time.csv"))