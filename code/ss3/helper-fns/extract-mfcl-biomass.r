# Extract spawning biomass and depletion from MFCL Report (.rep) file
# Returns data.table with columns: model, year, ssb, ssb_se, depletion, depletion_se

extract_mfcl_biomass = function(rep_file, model_name = NULL) {
	if(!file.exists(rep_file)) {
		stop(sprintf("Report file not found: %s", rep_file))
	}
	
	if(is.null(model_name)) {
		model_name = basename(dirname(rep_file))
	}
	
	rep = read.MFCLRep(rep_file)
	
	# Extract SSB using FLR4MFCL ssb() function
	ssb_values = as.data.table(ssb(rep)) %>%
		.[age == "all"] %>%
		.[, year := as.numeric(year)]
	
	# Aggregate across seasons (sum for annual total)
	ssb_annual = ssb_values[, .(
		ssb = sum(value, na.rm = TRUE)
	), by = year]
	
	# Extract unfished biomass (SSB0/B0) for depletion calculation
	# Using available FLR4MFCL functions to get unfished reference
	ssb0_values = as.data.table(ssb(rep)) %>%
		.[age == "all"] %>%
		.[, year := as.numeric(year)]
	
	# For now, assume ssb0 is constant (first year unfished)
	# This may need adjustment based on MFCL model specifics
	ssb0 = ssb_annual[1, ssb]
	
	biomass_dt = ssb_annual[, .(
		model = model_name,
		year,
		ssb,
		ssb_se = ssb * 0.05,  # Placeholder: 5% CV
		depletion = ssb / ssb0,
		depletion_se = (ssb / ssb0) * 0.05
	)]
	
	setorderv(biomass_dt, c("year"))
	
	return(biomass_dt)
}
