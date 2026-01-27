# Extract spawning biomass and depletion from SS3 Report.sso
# Returns data.table with columns: model, year, ssb, ssb_se, depletion, depletion_se

extract_ss3_biomass = function(model_dir) {
	if(!file.exists(file.path(model_dir, "Report.sso"))) {
		stop(sprintf("Report.sso not found in %s", model_dir))
	}
	
	rep = SS_output(dir = model_dir, verbose = FALSE, printstats = FALSE)
	model_name = basename(model_dir)
	
	if(is.null(rep$derived_quants)) {
		stop("No derived_quants found in Report.sso")
	}
	
	quants = as.data.table(rep$derived_quants) %>%
		.[grep("^SSB_|^Bratio_", Label)] %>%
		.[, c("type", "year") := .(
			sapply(Label, function(x) strsplit(x, "_")[[1]][1]),
			sapply(Label, function(x) as.numeric(strsplit(x, "_")[[1]][2]))
		)] %>%
		.[!is.na(year)]
	
	ssb = quants[type == "SSB", .(model = model_name, year, ssb = Value, ssb_se = StdDev)]
	depletion = quants[type == "Bratio", .(model = model_name, year, depletion = Value, depletion_se = StdDev)]
	
	out = ssb[depletion, on = c("model", "year"), nomatch = 0]
	setorderv(out, c("year"))
	
	return(out)
}
