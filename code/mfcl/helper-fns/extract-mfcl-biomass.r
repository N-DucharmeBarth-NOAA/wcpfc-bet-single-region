# Extract spawning biomass and depletion from MFCL Report (.rep) file
# Returns data.table with columns: model, year, ssb, ssb_se, depletion, depletion_se

# rep_file = file.path(this.path::this.proj(), "model-files", "mfcl", "v11", "plot-10.par.rep")
# result = extract_mfcl_biomass(rep_file, model_name = "MFCL-v11")
# head(result)

extract_mfcl_biomass = function(rep_file, model_name = NULL, quarterly = FALSE) {
	if(!file.exists(rep_file)) {
		stop(sprintf("Report file not found: %s", rep_file))
	}
	
	if(is.null(model_name)) {
		model_name = basename(dirname(rep_file))
	}
	
	rep = read.MFCLRep(rep_file)
	
	# Extract fished SSB using FLR4MFCL ssb() function
	ssb_values = as.data.table(ssb(rep))
	
	if(quarterly) {
		ssb_fished = ssb_values[age == "all", .(
			Value = mean(value, na.rm = TRUE)
		), by = .(yr = as.numeric(year), unit, area, iter)]
	} else {
		ssb_fished = ssb_values[age == "all", .(
			Value = sum(value, na.rm = TRUE)
		), by = .(yr = as.numeric(year), unit, area, iter)]
	}
	ssb_fished = ssb_fished[, .(yr, Value)]
	
	# Extract unfished SSB (dynamic B0) using adultBiomass_nofish()
	ssb_nofishing = as.data.table(adultBiomass_nofish(rep))
	
	if(quarterly) {
		ssb_unfished = ssb_nofishing[age == "all", .(
			Value = mean(value, na.rm = TRUE)
		), by = .(yr = as.numeric(year), unit, area, iter)]
	} else {
		ssb_unfished = ssb_nofishing[age == "all", .(
			Value = sum(value, na.rm = TRUE)
		), by = .(yr = as.numeric(year), unit, area, iter)]
	}
	ssb_unfished = ssb_unfished[, .(yr, Value)]
	
	# Merge fished and unfished, calculate depletion
	biomass_dt = merge(ssb_fished, ssb_unfished, by = "yr", suffixes = c("_fished", "_unfished")) %>%
		.[, .(
			model = model_name,
			year = yr,
			ssb = Value_fished,
			ssb_se = Value_fished * 0.001,
			depletion = Value_fished / Value_unfished,
			depletion_se = (Value_fished / Value_unfished) * 0.001
		)]
	
	setorderv(biomass_dt, c("year"))
	return(biomass_dt)
}
