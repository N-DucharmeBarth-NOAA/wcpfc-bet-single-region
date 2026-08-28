#V3.24AB
#_data_and_control_files: test_data.ss // YFT_IO.ctl
#_SS-V3.24AB-safe;_12/20/2016;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_11.2x64
1  #_N_Growth_Patterns
1 #_N_Morphs_Within_GrowthPattern 
#_Cond 1 #_Morph_between/within_stdev_ratio (no read if N_morphs=1)
#_Cond  1 #vector_Morphdist_(-1_in_first_val_gives_normal_approx)
#
#_Cond 0  #  N recruitment designs goes here if N_GP*nseas*area>1
#_Cond 0  #  placeholder for recruitment interaction request
#_Cond 1 1 1  # example recruitment design element for GP=1, seas=1, area=1
#
#_Cond 0 # N_movement_definitions goes here if N_areas > 1
#_Cond 1.0 # first age that moves (real age at begin of season, not integer) also cond on do_migration>0
#_Cond 1 1 1 2 4 10 # example move definition for seas=1, morph=1, source=1 dest=2, age1=4, age2=10
#
0 #_Nblock_Patterns
#_Cond 0 #_blocks_per_pattern 
# begin and end years of blocks
#
0.5 #_fracfemale 
3 #_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate
 #_Age_natmort_by gender x growthpattern
 1e-009 1.3432 1.182 1.0208 0.8596 0.6984 0.5372 0.5372 0.5372 0.5372 0.5372 0.564 0.6424 0.712 0.766 0.7976 0.8036 0.7848 0.746 0.6972 0.6492 0.6088 0.5796 0.5604 0.5492 0.5428 0.5396 0.5384 0.5376
1 # GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=age_speciific_K; 4=not implemented
1 #_Growth_Age_for_L1
28 #_Growth_Age_for_L2 (999 to use as Linf)
0 #_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)
0 #_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)
3 #_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity by GP; 4=read age-fecundity by GP; 5=read fec and wt from wtatage.ss; 6=read length-maturity by GP
#_Age_Maturity by growth pattern for females
 0 0 0 0 0 0.1 0.15 0.2 0.3 0.5 0.7 0.9 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
1 #_First_Mature_Age
1 #_fecundity option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b; (4)eggs=a+b*L; (5)eggs=a+b*W
0 #_hermaphroditism option:  0=none; 1=age-specific fxn
1 #_parameter_offset_approach (1=none, 2= M, G, CV_G as offset from female-GP1, 3=like SS2 V1.x)
1 #_env/block/dev_adjust_method (1=standard; 2=logistic transform keeps in base parm bounds; 3=standard w/ no bound check)
#
#_growth_parms
#_LO HI INIT PRIOR PR_type SD PHASE env-var use_dev dev_minyr dev_maxyr dev_stddev Block Block_Fxn
 10 45 22 26.7812 0 10 -4 0 0 0 0 0.5 0 0 # L_at_Amin_Fem_GP_1
 40 250 145 192.446 0 10 -2 0 0 0 0 0.5 0 0 # L_at_Amax_Fem_GP_1
 0.05 1 0.455 0.0676951 0 0.8 -4 0 0 0 0 0.5 0 0 # VonBert_K_Fem_GP_1
 0.1 4 0.1 0.0834877 0 0.8 -3 0 0 0 0 0.5 0 0 # CV_young_Fem_GP_1
 0.1 50 0.1 1.81575 0 0.8 -3 0 0 0 0 0.5 0 0 # CV_old_Fem_GP_1
 -3 3 2.459e-005 3.661e-005 0 0.8 -3 0 0 0 0 0.5 0 0 # Wtlen_1_Fem
 -3 3 2.9667 2.90182 0 0.8 -3 0 0 0 0 0.5 0 0 # Wtlen_2_Fem
 -10000 10000 70 55 0 0.8 -3 0 0 0 0 0.5 0 0 # Mat50%_Fem
 -3 3 -0.1 -0.25 0 0.8 -3 0 0 0 0 0.5 0 0 # Mat_slope_Fem
 -3 3 1 1 0 0.8 -3 0 0 0 0 0.5 0 0 # Eggs/kg_inter_Fem
 -3 3 0 0 0 0.8 -3 0 0 0 0 0.5 0 0 # Eggs/kg_slope_wt_Fem
 -4 4 0 0 -1 99 -3 0 0 0 0 0.5 0 0 # RecrDist_GP_1
 -4 4 0 0 -1 99 -3 0 0 0 0 0.5 0 0 # RecrDist_Area_1
 -4 4 4 0 -1 99 -3 0 0 0 0 0.5 0 0 # RecrDist_Seas_1
 1 1 1 1 -1 99 -3 0 0 0 0 0.5 0 0 # CohortGrowDev
#
#_Cond 0  #custom_MG-env_setup (0/1)
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no MG-environ parameters
#
#_Cond 0  #custom_MG-block_setup (0/1)
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no MG-block parameters
#_Cond No MG parm trends 
#
#_seasonal_effects_on_biology_parms
 0 0 0 0 0 0 0 0 0 0 #_femwtlen1,femwtlen2,mat1,mat2,fec1,fec2,Malewtlen1,malewtlen2,L1,K
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no seasonal MG parameters
#
#_Cond -4 #_MGparm_Dev_Phase
#
#_Spawner-Recruitment
3 #_SR_function: 2=Ricker; 3=std_B-H; 4=SCAA; 5=Hockey; 6=B-H_flattop; 7=survival_3Parm; 8=Shepard_3Parm
#_LO HI INIT PRIOR PR_type SD PHASE
 3 20 11.6658 9.3 -1 10 1 # SR_LN(R0)
 0.2 1 0.8 1 -1 0.2 -4 # SR_BH_steep
 0 2 0.6 0.6 -1 0.8 -3 # SR_sigmaR
 -5 5 0 0 -1 1 -3 # SR_envlink
 -5 5 0 0 -1 1 -1 # SR_R1_offset
 0 0 0 0 -1 99 -99 # SR_autocorr
0 #_SR_env_link
0 #_SR_env_target_0=none;1=devs;_2=R0;_3=steepness
1 #do_recdev:  0=none; 1=devvector; 2=simple deviations
1 # first year of main recr_devs; early devs can preceed this era
256 # last year of main recr_devs; forecast devs start in following year
1 #_recdev phase 
1 # (0/1) to read 13 advanced options
 0 #_recdev_early_start (0=none; neg value makes relative to recdev_start)
 -2 #_recdev_early_phase
 0 #_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)
 1 #_lambda for Fcast_recr_like occurring before endyr+1
 -29.7274 #_last_early_yr_nobias_adj_in_MPD
 156.69 #_first_yr_fullbias_adj_in_MPD
 246.83 #_last_yr_fullbias_adj_in_MPD
 253.292 #_first_recent_yr_nobias_adj_in_MPD
 0.7107 #_max_bias_adj_in_MPD (-1 to override ramp and set biasadj=1.0 for all estimated recdevs)
 0 #_period of cycles in recruitment (N parms read below)
 -15 #min rec_dev
 15 #max rec_dev
 0 #_read_recdevs
#_end of advanced SR options
#
#_placeholder for full parameter lines for recruitment cycles
# read specified recr devs
#_Yr Input_value
#
# all recruitment deviations
#DisplayOnly 0.429616 # Main_RecrDev_1
#DisplayOnly 0.0602303 # Main_RecrDev_2
#DisplayOnly -0.0840702 # Main_RecrDev_3
#DisplayOnly 0.113334 # Main_RecrDev_4
#DisplayOnly 0.727653 # Main_RecrDev_5
#DisplayOnly 0.213631 # Main_RecrDev_6
#DisplayOnly 0.106998 # Main_RecrDev_7
#DisplayOnly 0.461906 # Main_RecrDev_8
#DisplayOnly 0.125317 # Main_RecrDev_9
#DisplayOnly -0.215755 # Main_RecrDev_10
#DisplayOnly -0.370592 # Main_RecrDev_11
#DisplayOnly -0.272797 # Main_RecrDev_12
#DisplayOnly 0.251774 # Main_RecrDev_13
#DisplayOnly 1.03391 # Main_RecrDev_14
#DisplayOnly -0.0403122 # Main_RecrDev_15
#DisplayOnly -0.0555376 # Main_RecrDev_16
#DisplayOnly 0.0412472 # Main_RecrDev_17
#DisplayOnly 0.199387 # Main_RecrDev_18
#DisplayOnly -0.0238311 # Main_RecrDev_19
#DisplayOnly -0.270374 # Main_RecrDev_20
#DisplayOnly 0.125645 # Main_RecrDev_21
#DisplayOnly 0.11638 # Main_RecrDev_22
#DisplayOnly -0.180827 # Main_RecrDev_23
#DisplayOnly -0.203259 # Main_RecrDev_24
#DisplayOnly -0.0688403 # Main_RecrDev_25
#DisplayOnly -0.260061 # Main_RecrDev_26
#DisplayOnly 0.337448 # Main_RecrDev_27
#DisplayOnly 0.209636 # Main_RecrDev_28
#DisplayOnly 0.192661 # Main_RecrDev_29
#DisplayOnly 0.11351 # Main_RecrDev_30
#DisplayOnly -0.152102 # Main_RecrDev_31
#DisplayOnly -0.0680383 # Main_RecrDev_32
#DisplayOnly -0.0892516 # Main_RecrDev_33
#DisplayOnly 0.112206 # Main_RecrDev_34
#DisplayOnly 0.38809 # Main_RecrDev_35
#DisplayOnly -0.0333757 # Main_RecrDev_36
#DisplayOnly -0.0132182 # Main_RecrDev_37
#DisplayOnly -0.256704 # Main_RecrDev_38
#DisplayOnly -0.181881 # Main_RecrDev_39
#DisplayOnly -0.0739148 # Main_RecrDev_40
#DisplayOnly -0.157217 # Main_RecrDev_41
#DisplayOnly 0.272515 # Main_RecrDev_42
#DisplayOnly 0.0738415 # Main_RecrDev_43
#DisplayOnly -0.274634 # Main_RecrDev_44
#DisplayOnly -0.365881 # Main_RecrDev_45
#DisplayOnly -0.0687008 # Main_RecrDev_46
#DisplayOnly 0.367441 # Main_RecrDev_47
#DisplayOnly -0.19019 # Main_RecrDev_48
#DisplayOnly -0.224346 # Main_RecrDev_49
#DisplayOnly -0.0687527 # Main_RecrDev_50
#DisplayOnly 0.300601 # Main_RecrDev_51
#DisplayOnly 0.212288 # Main_RecrDev_52
#DisplayOnly 0.323585 # Main_RecrDev_53
#DisplayOnly 0.240673 # Main_RecrDev_54
#DisplayOnly -0.0883785 # Main_RecrDev_55
#DisplayOnly -0.196682 # Main_RecrDev_56
#DisplayOnly -0.0584583 # Main_RecrDev_57
#DisplayOnly -0.0950661 # Main_RecrDev_58
#DisplayOnly -0.301641 # Main_RecrDev_59
#DisplayOnly -0.378408 # Main_RecrDev_60
#DisplayOnly -0.0453992 # Main_RecrDev_61
#DisplayOnly -0.0180895 # Main_RecrDev_62
#DisplayOnly -0.249803 # Main_RecrDev_63
#DisplayOnly -0.37506 # Main_RecrDev_64
#DisplayOnly -0.456762 # Main_RecrDev_65
#DisplayOnly -0.272413 # Main_RecrDev_66
#DisplayOnly 0.144866 # Main_RecrDev_67
#DisplayOnly 0.083675 # Main_RecrDev_68
#DisplayOnly -0.238463 # Main_RecrDev_69
#DisplayOnly -0.0522762 # Main_RecrDev_70
#DisplayOnly 0.691173 # Main_RecrDev_71
#DisplayOnly 0.110185 # Main_RecrDev_72
#DisplayOnly -0.0785953 # Main_RecrDev_73
#DisplayOnly -0.342537 # Main_RecrDev_74
#DisplayOnly -0.400821 # Main_RecrDev_75
#DisplayOnly -0.245729 # Main_RecrDev_76
#DisplayOnly -0.269511 # Main_RecrDev_77
#DisplayOnly 0.285083 # Main_RecrDev_78
#DisplayOnly 0.364862 # Main_RecrDev_79
#DisplayOnly -0.0229697 # Main_RecrDev_80
#DisplayOnly -0.207323 # Main_RecrDev_81
#DisplayOnly -0.363053 # Main_RecrDev_82
#DisplayOnly -0.304594 # Main_RecrDev_83
#DisplayOnly 0.0377743 # Main_RecrDev_84
#DisplayOnly -0.16042 # Main_RecrDev_85
#DisplayOnly 0.0827275 # Main_RecrDev_86
#DisplayOnly 0.571639 # Main_RecrDev_87
#DisplayOnly 0.00117545 # Main_RecrDev_88
#DisplayOnly -0.362554 # Main_RecrDev_89
#DisplayOnly -0.338071 # Main_RecrDev_90
#DisplayOnly 0.0394699 # Main_RecrDev_91
#DisplayOnly 0.0508891 # Main_RecrDev_92
#DisplayOnly 0.098645 # Main_RecrDev_93
#DisplayOnly 0.324732 # Main_RecrDev_94
#DisplayOnly -0.0493577 # Main_RecrDev_95
#DisplayOnly -0.236106 # Main_RecrDev_96
#DisplayOnly -0.188585 # Main_RecrDev_97
#DisplayOnly -0.143205 # Main_RecrDev_98
#DisplayOnly -0.306096 # Main_RecrDev_99
#DisplayOnly -0.0497288 # Main_RecrDev_100
#DisplayOnly 0.928922 # Main_RecrDev_101
#DisplayOnly 0.136039 # Main_RecrDev_102
#DisplayOnly 0.30301 # Main_RecrDev_103
#DisplayOnly -0.192206 # Main_RecrDev_104
#DisplayOnly -0.155476 # Main_RecrDev_105
#DisplayOnly 0.0392793 # Main_RecrDev_106
#DisplayOnly -0.233897 # Main_RecrDev_107
#DisplayOnly -0.599737 # Main_RecrDev_108
#DisplayOnly -0.417003 # Main_RecrDev_109
#DisplayOnly 0.570278 # Main_RecrDev_110
#DisplayOnly -0.33934 # Main_RecrDev_111
#DisplayOnly -0.0983397 # Main_RecrDev_112
#DisplayOnly 0.690548 # Main_RecrDev_113
#DisplayOnly 0.136037 # Main_RecrDev_114
#DisplayOnly -0.0444787 # Main_RecrDev_115
#DisplayOnly 0.677817 # Main_RecrDev_116
#DisplayOnly -0.382949 # Main_RecrDev_117
#DisplayOnly -0.428643 # Main_RecrDev_118
#DisplayOnly 0.066021 # Main_RecrDev_119
#DisplayOnly 0.385667 # Main_RecrDev_120
#DisplayOnly 0.0550875 # Main_RecrDev_121
#DisplayOnly -0.347893 # Main_RecrDev_122
#DisplayOnly 0.105165 # Main_RecrDev_123
#DisplayOnly -0.493784 # Main_RecrDev_124
#DisplayOnly -0.0888751 # Main_RecrDev_125
#DisplayOnly 0.489724 # Main_RecrDev_126
#DisplayOnly -0.150171 # Main_RecrDev_127
#DisplayOnly -0.29506 # Main_RecrDev_128
#DisplayOnly 0.105018 # Main_RecrDev_129
#DisplayOnly 0.0690728 # Main_RecrDev_130
#DisplayOnly -0.279908 # Main_RecrDev_131
#DisplayOnly -0.0489699 # Main_RecrDev_132
#DisplayOnly -0.54911 # Main_RecrDev_133
#DisplayOnly -0.751336 # Main_RecrDev_134
#DisplayOnly -0.455224 # Main_RecrDev_135
#DisplayOnly 0.253165 # Main_RecrDev_136
#DisplayOnly 0.462036 # Main_RecrDev_137
#DisplayOnly 0.225468 # Main_RecrDev_138
#DisplayOnly 0.00712627 # Main_RecrDev_139
#DisplayOnly -0.356473 # Main_RecrDev_140
#DisplayOnly -0.654367 # Main_RecrDev_141
#DisplayOnly 0.185134 # Main_RecrDev_142
#DisplayOnly -0.216941 # Main_RecrDev_143
#DisplayOnly -0.245118 # Main_RecrDev_144
#DisplayOnly -0.226911 # Main_RecrDev_145
#DisplayOnly 0.150563 # Main_RecrDev_146
#DisplayOnly 0.613476 # Main_RecrDev_147
#DisplayOnly 0.156324 # Main_RecrDev_148
#DisplayOnly -0.10686 # Main_RecrDev_149
#DisplayOnly 0.126209 # Main_RecrDev_150
#DisplayOnly -0.158048 # Main_RecrDev_151
#DisplayOnly -0.390285 # Main_RecrDev_152
#DisplayOnly -0.433351 # Main_RecrDev_153
#DisplayOnly -0.0991689 # Main_RecrDev_154
#DisplayOnly 0.0600243 # Main_RecrDev_155
#DisplayOnly 0.725367 # Main_RecrDev_156
#DisplayOnly -0.163119 # Main_RecrDev_157
#DisplayOnly 0.15639 # Main_RecrDev_158
#DisplayOnly 0.0981269 # Main_RecrDev_159
#DisplayOnly 0.109201 # Main_RecrDev_160
#DisplayOnly 0.441442 # Main_RecrDev_161
#DisplayOnly 0.0503657 # Main_RecrDev_162
#DisplayOnly -0.423735 # Main_RecrDev_163
#DisplayOnly -0.528956 # Main_RecrDev_164
#DisplayOnly -0.177342 # Main_RecrDev_165
#DisplayOnly -0.390795 # Main_RecrDev_166
#DisplayOnly -0.175548 # Main_RecrDev_167
#DisplayOnly -0.0145544 # Main_RecrDev_168
#DisplayOnly -0.41086 # Main_RecrDev_169
#DisplayOnly -0.601117 # Main_RecrDev_170
#DisplayOnly -0.0025063 # Main_RecrDev_171
#DisplayOnly -0.133514 # Main_RecrDev_172
#DisplayOnly 0.0130619 # Main_RecrDev_173
#DisplayOnly 0.117908 # Main_RecrDev_174
#DisplayOnly 0.521748 # Main_RecrDev_175
#DisplayOnly -0.0362987 # Main_RecrDev_176
#DisplayOnly 0.399081 # Main_RecrDev_177
#DisplayOnly 0.595711 # Main_RecrDev_178
#DisplayOnly -0.169262 # Main_RecrDev_179
#DisplayOnly 0.243299 # Main_RecrDev_180
#DisplayOnly -0.132757 # Main_RecrDev_181
#DisplayOnly 0.105456 # Main_RecrDev_182
#DisplayOnly 0.0713291 # Main_RecrDev_183
#DisplayOnly -0.0200494 # Main_RecrDev_184
#DisplayOnly -0.246297 # Main_RecrDev_185
#DisplayOnly 0.6707 # Main_RecrDev_186
#DisplayOnly -0.0391319 # Main_RecrDev_187
#DisplayOnly -0.229946 # Main_RecrDev_188
#DisplayOnly 0.181036 # Main_RecrDev_189
#DisplayOnly -0.109753 # Main_RecrDev_190
#DisplayOnly 0.355793 # Main_RecrDev_191
#DisplayOnly -0.126071 # Main_RecrDev_192
#DisplayOnly -0.409611 # Main_RecrDev_193
#DisplayOnly 0.0747909 # Main_RecrDev_194
#DisplayOnly -0.0488069 # Main_RecrDev_195
#DisplayOnly 0.245128 # Main_RecrDev_196
#DisplayOnly 0.219329 # Main_RecrDev_197
#DisplayOnly 0.456931 # Main_RecrDev_198
#DisplayOnly 0.0136375 # Main_RecrDev_199
#DisplayOnly 0.293193 # Main_RecrDev_200
#DisplayOnly 0.27111 # Main_RecrDev_201
#DisplayOnly -0.0706693 # Main_RecrDev_202
#DisplayOnly 0.198375 # Main_RecrDev_203
#DisplayOnly 0.172803 # Main_RecrDev_204
#DisplayOnly -0.233237 # Main_RecrDev_205
#DisplayOnly -0.431319 # Main_RecrDev_206
#DisplayOnly 0.309485 # Main_RecrDev_207
#DisplayOnly 0.298635 # Main_RecrDev_208
#DisplayOnly -0.00338634 # Main_RecrDev_209
#DisplayOnly -0.0979152 # Main_RecrDev_210
#DisplayOnly 0.135936 # Main_RecrDev_211
#DisplayOnly -0.349511 # Main_RecrDev_212
#DisplayOnly -0.293311 # Main_RecrDev_213
#DisplayOnly 0.228067 # Main_RecrDev_214
#DisplayOnly 0.0494448 # Main_RecrDev_215
#DisplayOnly 0.0823827 # Main_RecrDev_216
#DisplayOnly 0.515258 # Main_RecrDev_217
#DisplayOnly 0.280729 # Main_RecrDev_218
#DisplayOnly 0.23335 # Main_RecrDev_219
#DisplayOnly 0.285512 # Main_RecrDev_220
#DisplayOnly -0.430785 # Main_RecrDev_221
#DisplayOnly -0.214448 # Main_RecrDev_222
#DisplayOnly -0.0176218 # Main_RecrDev_223
#DisplayOnly -8.65797e-006 # Main_RecrDev_224
#DisplayOnly 0.335598 # Main_RecrDev_225
#DisplayOnly -0.203215 # Main_RecrDev_226
#DisplayOnly -0.444458 # Main_RecrDev_227
#DisplayOnly -0.485776 # Main_RecrDev_228
#DisplayOnly -0.035751 # Main_RecrDev_229
#DisplayOnly 0.00548448 # Main_RecrDev_230
#DisplayOnly 0.345623 # Main_RecrDev_231
#DisplayOnly -0.138118 # Main_RecrDev_232
#DisplayOnly -0.556312 # Main_RecrDev_233
#DisplayOnly 0.333427 # Main_RecrDev_234
#DisplayOnly -0.364232 # Main_RecrDev_235
#DisplayOnly -0.175651 # Main_RecrDev_236
#DisplayOnly -0.00807531 # Main_RecrDev_237
#DisplayOnly 0.260564 # Main_RecrDev_238
#DisplayOnly -0.231392 # Main_RecrDev_239
#DisplayOnly 0.523108 # Main_RecrDev_240
#DisplayOnly 0.146863 # Main_RecrDev_241
#DisplayOnly 0.122042 # Main_RecrDev_242
#DisplayOnly 0.147271 # Main_RecrDev_243
#DisplayOnly 0.0725885 # Main_RecrDev_244
#DisplayOnly -0.135446 # Main_RecrDev_245
#DisplayOnly 0.628596 # Main_RecrDev_246
#DisplayOnly 0.667144 # Main_RecrDev_247
#DisplayOnly 0.524708 # Main_RecrDev_248
#DisplayOnly -0.0779835 # Main_RecrDev_249
#DisplayOnly 0.354746 # Main_RecrDev_250
#DisplayOnly -0.217029 # Main_RecrDev_251
#DisplayOnly -0.19542 # Main_RecrDev_252
#DisplayOnly -0.110919 # Main_RecrDev_253
#DisplayOnly -0.0978379 # Main_RecrDev_254
#DisplayOnly -0.0607294 # Main_RecrDev_255
#DisplayOnly -0.0523775 # Main_RecrDev_256
#
#Fishing Mortality info 
0.1 # F ballpark for annual F (=Z-M) for specified year
160 # F ballpark year (neg value to disable)
3 # F_Method:  1=Pope; 2=instan. F; 3=hybrid (hybrid is recommended)
5 # max F or harvest rate, depends on F_Method
# no additional F input needed for Fmethod 1
# if Fmethod=2; read overall start F value; overall phase; N detailed inputs to read
# if Fmethod=3; read N iterations for tuning for Fmethod 3
5  # N iterations for tuning F in hybrid method (recommend 3 to 7)
#
#_initial_F_parms
#_LO HI INIT PRIOR PR_type SD PHASE
 0 1 0 0.01 0 99 -1 # InitF_1fishing_gi_1
 0 1 0 0.01 0 99 -1 # InitF_2fishing_gi_4
 0 1 0 0.01 0 99 -1 # InitF_3fishing_hd_1
 0 1 0 0.01 0 99 -1 # InitF_4fishing_ll_1
 0 1 0 0.01 0 99 -1 # InitF_5fishing_ll_2
 0 1 0 0.01 0 99 -1 # InitF_6fishing_ll_3
 0 1 0 0.01 0 99 -1 # InitF_7fishing_ll_4
 0 1 0 0.01 0 99 -1 # InitF_8fishing_other_1
 0 1 0 0.01 0 99 -1 # InitF_9fishing_other_4
 0 1 0 0.01 0 99 -1 # InitF_10fishing_bb_1
 0 1 0 0.01 0 99 -1 # InitF_11fishing_ps_1
 0 1 0 0.01 0 99 -1 # InitF_12fishing_ps_2
 0 1 0 0.01 0 99 -1 # InitF_13fishing_ps_4
 0 1 0 0.01 0 99 -1 # InitF_14fishing_trol_1
 0 1 0 0.01 0 99 -1 # InitF_15fishing_trol_2
 0 1 0 0.01 0 99 -1 # InitF_16fishing_trol_4
#
#_Q_setup
 # Q_type options:  <0=mirror, 0=float_nobiasadj, 1=float_biasadj, 2=parm_nobiasadj, 3=parm_w_random_dev, 4=parm_w_randwalk, 5=mean_unbiased_float_assign_to_parm
#_for_env-var:_enter_index_of_the_env-var_to_be_linked
#_Den-dep  env-var  extra_se  Q_type
 0 0 0 0 # 1 fishing_gi_1
 0 0 0 0 # 2 fishing_gi_4
 0 0 0 0 # 3 fishing_hd_1
 0 0 0 0 # 4 fishing_ll_1
 0 0 0 0 # 5 fishing_ll_2
 0 0 0 0 # 6 fishing_ll_3
 0 0 0 0 # 7 fishing_ll_4
 0 0 0 0 # 8 fishing_other_1
 0 0 0 0 # 9 fishing_other_4
 0 0 0 0 # 10 fishing_bb_1
 0 0 0 0 # 11 fishing_ps_1
 0 0 0 0 # 12 fishing_ps_2
 0 0 0 0 # 13 fishing_ps_4
 0 0 0 0 # 14 fishing_trol_1
 0 0 0 0 # 15 fishing_trol_2
 0 0 0 0 # 16 fishing_trol_4
 0 0 1 0 # 17 llcpue
#
#_Cond 0 #_If q has random component, then 0=read one parm for each fleet with random q; 1=read a parm for each year of index
#_Q_parms(if_any);Qunits_are_ln(q)
# LO HI INIT PRIOR PR_type SD PHASE
 -0.5 0.5 0 0.01 -1 99 -3 # Q_extraSD_17_llcpue
#
#_size_selex_types
#discard_options:_0=none;_1=define_retention;_2=retention&mortality;_3=all_discarded_dead
#_Pattern Discard Male Special
 0 0 0 0 # 1 fishing_gi_1
 0 0 0 0 # 2 fishing_gi_4
 0 0 0 0 # 3 fishing_hd_1
 0 0 0 0 # 4 fishing_ll_1
 0 0 0 0 # 5 fishing_ll_2
 0 0 0 0 # 6 fishing_ll_3
 0 0 0 0 # 7 fishing_ll_4
 0 0 0 0 # 8 fishing_other_1
 0 0 0 0 # 9 fishing_other_4
 0 0 0 0 # 10 fishing_bb_1
 0 0 0 0 # 11 fishing_ps_1
 0 0 0 0 # 12 fishing_ps_2
 0 0 0 0 # 13 fishing_ps_4
 0 0 0 0 # 14 fishing_trol_1
 0 0 0 0 # 15 fishing_trol_2
 0 0 0 0 # 16 fishing_trol_4
 0 0 0 0 # 17 llcpue
#
#_age_selex_types
#_Pattern ___ Male Special
 20 0 0 0 # 1 fishing_gi_1
 20 0 0 0 # 2 fishing_gi_4
 20 0 0 0 # 3 fishing_hd_1
 20 0 0 0 # 4 fishing_ll_1
 20 0 0 0 # 5 fishing_ll_2
 20 0 0 0 # 6 fishing_ll_3
 20 0 0 0 # 7 fishing_ll_4
 20 0 0 0 # 8 fishing_other_1
 20 0 0 0 # 9 fishing_other_4
 20 0 0 0 # 10 fishing_bb_1
 20 0 0 0 # 11 fishing_ps_1
 20 0 0 0 # 12 fishing_ps_2
 20 0 0 0 # 13 fishing_ps_4
 20 0 0 0 # 14 fishing_trol_1
 15 0 0 14 # 15 fishing_trol_2
 15 0 0 14 # 16 fishing_trol_4
 12 0 0 0 # 17 llcpue
#_LO HI INIT PRIOR PR_type SD PHASE env-var use_dev dev_minyr dev_maxyr dev_stddev Block Block_Fxn
 1 28 7.89379 7 0 3 2 0 0 0 0 0 0 0 # AgeSel_1P_1_fishing_gi_1
 -20 20 -10.3542 -3 0 1000 3 0 0 0 0 0 0 0 # AgeSel_1P_2_fishing_gi_1
 -20 20 -6.49055 -1 0 3 4 0 0 0 0 0 0 0 # AgeSel_1P_3_fishing_gi_1
 -20 20 2.83368 3 0 1 3 0 0 0 0 0 0 0 # AgeSel_1P_4_fishing_gi_1
 -20 20 -6 -6 0 1000 -5 0 0 0 0 0 0 0 # AgeSel_1P_5_fishing_gi_1
 -9 5 -999 -2 0 1 -5 0 0 0 0 0 0 0 # AgeSel_1P_6_fishing_gi_1
 1 28 7.02866 7 0 3 2 0 0 0 0 0 0 0 # AgeSel_2P_1_fishing_gi_4
 -20 20 -11.7392 -3 0 1000 3 0 0 0 0 0 0 0 # AgeSel_2P_2_fishing_gi_4
 -20 20 -9.35268 -1 0 3 4 0 0 0 0 0 0 0 # AgeSel_2P_3_fishing_gi_4
 -20 20 3.86367 3 0 1 3 0 0 0 0 0 0 0 # AgeSel_2P_4_fishing_gi_4
 -20 20 -6 -6 0 1000 -5 0 0 0 0 0 0 0 # AgeSel_2P_5_fishing_gi_4
 -9 5 -999 -2 0 1 -5 0 0 0 0 0 0 0 # AgeSel_2P_6_fishing_gi_4
 1 28 6.97456 7 0 3 2 0 0 0 0 0 0 0 # AgeSel_3P_1_fishing_hd_1
 -20 20 -12.7567 -3 0 1000 3 0 0 0 0 0 0 0 # AgeSel_3P_2_fishing_hd_1
 -20 20 -9.66132 -1 0 3 4 0 0 0 0 0 0 0 # AgeSel_3P_3_fishing_hd_1
 -20 20 3.90274 3 0 1 3 0 0 0 0 0 0 0 # AgeSel_3P_4_fishing_hd_1
 -20 20 -6 -6 0 1000 -5 0 0 0 0 0 0 0 # AgeSel_3P_5_fishing_hd_1
 -9 5 -999 -2 0 1 -5 0 0 0 0 0 0 0 # AgeSel_3P_6_fishing_hd_1
 1 28 10.5213 7 0 3 2 0 0 0 0 0 0 0 # AgeSel_4P_1_fishing_ll_1
 -20 20 5.21531 -3 0 1000 3 0 0 0 0 0 0 0 # AgeSel_4P_2_fishing_ll_1
 -20 20 2.13205 -1 0 3 4 0 0 0 0 0 0 0 # AgeSel_4P_3_fishing_ll_1
 -20 20 9.21297 3 0 1 3 0 0 0 0 0 0 0 # AgeSel_4P_4_fishing_ll_1
 -20 20 -6 -6 0 1000 -5 0 0 0 0 0 0 0 # AgeSel_4P_5_fishing_ll_1
 -9 5 -999 -2 0 1 -5 0 0 0 0 0 0 0 # AgeSel_4P_6_fishing_ll_1
 1 28 7.9898 7 0 3 2 0 0 0 0 0 0 0 # AgeSel_5P_1_fishing_ll_2
 -20 20 5.37167 -3 0 1000 3 0 0 0 0 0 0 0 # AgeSel_5P_2_fishing_ll_2
 -20 20 -10.7319 -1 0 3 4 0 0 0 0 0 0 0 # AgeSel_5P_3_fishing_ll_2
 -20 20 9.46349 3 0 1 3 0 0 0 0 0 0 0 # AgeSel_5P_4_fishing_ll_2
 -20 20 -6 -6 0 1000 -5 0 0 0 0 0 0 0 # AgeSel_5P_5_fishing_ll_2
 -9 5 -999 -2 0 1 -5 0 0 0 0 0 0 0 # AgeSel_5P_6_fishing_ll_2
 1 28 8.76053 7 0 3 2 0 0 0 0 0 0 0 # AgeSel_6P_1_fishing_ll_3
 -20 20 -9.73997 -3 0 1000 3 0 0 0 0 0 0 0 # AgeSel_6P_2_fishing_ll_3
 -20 20 1.4652 -1 0 3 4 0 0 0 0 0 0 0 # AgeSel_6P_3_fishing_ll_3
 -20 20 4.48151 3 0 1 3 0 0 0 0 0 0 0 # AgeSel_6P_4_fishing_ll_3
 -20 20 -6 -6 0 1000 -5 0 0 0 0 0 0 0 # AgeSel_6P_5_fishing_ll_3
 -9 5 -999 -2 0 1 -5 0 0 0 0 0 0 0 # AgeSel_6P_6_fishing_ll_3
 1 28 11.9256 7 0 3 2 0 0 0 0 0 0 0 # AgeSel_7P_1_fishing_ll_4
 -20 20 5.11224 -3 0 1000 3 0 0 0 0 0 0 0 # AgeSel_7P_2_fishing_ll_4
 -20 20 2.41854 -1 0 3 4 0 0 0 0 0 0 0 # AgeSel_7P_3_fishing_ll_4
 -20 20 9.03527 3 0 1 3 0 0 0 0 0 0 0 # AgeSel_7P_4_fishing_ll_4
 -20 20 -6 -6 0 1000 -5 0 0 0 0 0 0 0 # AgeSel_7P_5_fishing_ll_4
 -9 5 -999 -2 0 1 -5 0 0 0 0 0 0 0 # AgeSel_7P_6_fishing_ll_4
 1 28 7.46493 7 0 3 2 0 0 0 0 0 0 0 # AgeSel_8P_1_fishing_other_1
 -20 20 -10.4515 -3 0 1000 3 0 0 0 0 0 0 0 # AgeSel_8P_2_fishing_other_1
 -20 20 0.825061 -1 0 3 4 0 0 0 0 0 0 0 # AgeSel_8P_3_fishing_other_1
 -20 20 5.25897 3 0 1 3 0 0 0 0 0 0 0 # AgeSel_8P_4_fishing_other_1
 -20 20 -6 -6 0 1000 -5 0 0 0 0 0 0 0 # AgeSel_8P_5_fishing_other_1
 -9 5 -999 -2 0 1 -5 0 0 0 0 0 0 0 # AgeSel_8P_6_fishing_other_1
 1 28 9.43008 7 0 3 2 0 0 0 0 0 0 0 # AgeSel_9P_1_fishing_other_4
 -20 20 1.53305 -3 0 1000 3 0 0 0 0 0 0 0 # AgeSel_9P_2_fishing_other_4
 -20 20 2.35744 -1 0 3 4 0 0 0 0 0 0 0 # AgeSel_9P_3_fishing_other_4
 -20 20 -2.2042 3 0 1 3 0 0 0 0 0 0 0 # AgeSel_9P_4_fishing_other_4
 -20 20 -6 -6 0 1000 -5 0 0 0 0 0 0 0 # AgeSel_9P_5_fishing_other_4
 -9 5 -999 -2 0 1 -5 0 0 0 0 0 0 0 # AgeSel_9P_6_fishing_other_4
 1 28 4.49649 7 0 3 2 0 0 0 0 0 0 0 # AgeSel_10P_1_fishing_bb_1
 -20 20 -9.1208 -3 0 1000 3 0 0 0 0 0 0 0 # AgeSel_10P_2_fishing_bb_1
 -20 20 -2.18874 -1 0 3 4 0 0 0 0 0 0 0 # AgeSel_10P_3_fishing_bb_1
 -20 20 3.57138 3 0 1 3 0 0 0 0 0 0 0 # AgeSel_10P_4_fishing_bb_1
 -20 20 -6 -6 0 1000 -5 0 0 0 0 0 0 0 # AgeSel_10P_5_fishing_bb_1
 -9 5 -999 -2 0 1 -5 0 0 0 0 0 0 0 # AgeSel_10P_6_fishing_bb_1
 1 28 8.10873 7 0 3 2 0 0 0 0 0 0 0 # AgeSel_11P_1_fishing_ps_1
 -20 20 -9.27496 -3 0 1000 3 0 0 0 0 0 0 0 # AgeSel_11P_2_fishing_ps_1
 -20 20 0.778829 -1 0 3 4 0 0 0 0 0 0 0 # AgeSel_11P_3_fishing_ps_1
 -20 20 6.75414 3 0 1 3 0 0 0 0 0 0 0 # AgeSel_11P_4_fishing_ps_1
 -20 20 -6 -6 0 1000 -5 0 0 0 0 0 0 0 # AgeSel_11P_5_fishing_ps_1
 -9 5 -999 -2 0 1 -5 0 0 0 0 0 0 0 # AgeSel_11P_6_fishing_ps_1
 1 28 6.0611 7 0 3 2 0 0 0 0 0 0 0 # AgeSel_12P_1_fishing_ps_2
 -20 20 -12.6742 -3 0 1000 3 0 0 0 0 0 0 0 # AgeSel_12P_2_fishing_ps_2
 -20 20 -8.02686 -1 0 3 4 0 0 0 0 0 0 0 # AgeSel_12P_3_fishing_ps_2
 -20 20 5.30928 3 0 1 3 0 0 0 0 0 0 0 # AgeSel_12P_4_fishing_ps_2
 -20 20 -6 -6 0 1000 -5 0 0 0 0 0 0 0 # AgeSel_12P_5_fishing_ps_2
 -9 5 -999 -2 0 1 -5 0 0 0 0 0 0 0 # AgeSel_12P_6_fishing_ps_2
 1 28 6.95427 7 0 3 2 0 0 0 0 0 0 0 # AgeSel_13P_1_fishing_ps_4
 -20 20 5.49534 -3 0 1000 3 0 0 0 0 0 0 0 # AgeSel_13P_2_fishing_ps_4
 -20 20 -7.8331 -1 0 3 4 0 0 0 0 0 0 0 # AgeSel_13P_3_fishing_ps_4
 -20 20 9.93782 3 0 1 3 0 0 0 0 0 0 0 # AgeSel_13P_4_fishing_ps_4
 -20 20 -6 -6 0 1000 -5 0 0 0 0 0 0 0 # AgeSel_13P_5_fishing_ps_4
 -9 5 -999 -2 0 1 -5 0 0 0 0 0 0 0 # AgeSel_13P_6_fishing_ps_4
 1 28 6.18523 7 0 3 2 0 0 0 0 0 0 0 # AgeSel_14P_1_fishing_trol_1
 -20 20 -2.71376 -3 0 1000 3 0 0 0 0 0 0 0 # AgeSel_14P_2_fishing_trol_1
 -20 20 -4.14793 -1 0 3 4 0 0 0 0 0 0 0 # AgeSel_14P_3_fishing_trol_1
 -20 20 3.04543 3 0 1 3 0 0 0 0 0 0 0 # AgeSel_14P_4_fishing_trol_1
 -20 20 -6 -6 0 1000 -5 0 0 0 0 0 0 0 # AgeSel_14P_5_fishing_trol_1
 -9 5 -999 -2 0 1 -5 0 0 0 0 0 0 0 # AgeSel_14P_6_fishing_trol_1
 8 18 8.13685 14 0 2 2 0 0 0 0 0 0 0 # AgeSel_17P_1_llcpue
 2 6 3.62789 4 0 1 3 0 0 0 0 0 0 0 # AgeSel_17P_2_llcpue
#_Cond 0 #_custom_sel-env_setup (0/1) 
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no enviro fxns
#_Cond 0 #_custom_sel-blk_setup (0/1) 
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no block usage
#_Cond No selex parm trends 
#_Cond -4 # placeholder for selparm_Dev_Phase
#_Cond 0 #_env/block/dev_adjust_method (1=standard; 2=logistic trans to keep in base parm bounds; 3=standard w/ no bound check)
#
# Tag loss and Tag reporting parameters go next
0  # TG_custom:  0=no read; 1=read if tags exist
#_Cond -6 6 1 1 2 0.01 -4 0 0 0 0 0 0 0  #_placeholder if no parameters
#
1 #_Variance_adjustments_to_input_values
#_fleet: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 
  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 #_add_to_survey_CV
  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 #_add_to_discard_stddev
  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 #_add_to_bodywt_CV
  2.64 0.9 1.28 1.2 1.06 0.83 2.02 0.78 0.58 1.05 2.36 2.19 2.39 1 1 1.08 1 #_mult_by_lencomp_N
  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 #_mult_by_agecomp_N
  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 #_mult_by_size-at-age_N
#
1 #_maxlambdaphase
1 #_sd_offset
#
2 # number of changes to make to default Lambdas (default value is 1.0)
# Like_comp codes:  1=surv; 2=disc; 3=mnwt; 4=length; 5=age; 6=SizeFreq; 7=sizeage; 8=catch; 9=init_equ_catch; 
# 10=recrdev; 11=parm_prior; 12=parm_dev; 13=CrashPen; 14=Morphcomp; 15=Tag-comp; 16=Tag-negbin; 17=F_ballpark
#like_comp fleet/survey  phase  value  sizefreq_method
 11 1 1 0 1
 6 17 1 0.51 1
#
# lambdas (for info only; columns are phases)
#  0 #_CPUE/survey:_1
#  0 #_CPUE/survey:_2
#  0 #_CPUE/survey:_3
#  0 #_CPUE/survey:_4
#  0 #_CPUE/survey:_5
#  0 #_CPUE/survey:_6
#  0 #_CPUE/survey:_7
#  0 #_CPUE/survey:_8
#  0 #_CPUE/survey:_9
#  0 #_CPUE/survey:_10
#  0 #_CPUE/survey:_11
#  0 #_CPUE/survey:_12
#  0 #_CPUE/survey:_13
#  0 #_CPUE/survey:_14
#  0 #_CPUE/survey:_15
#  0 #_CPUE/survey:_16
#  1 #_CPUE/survey:_17
#  1 #_lencomp:_1
#  1 #_lencomp:_2
#  1 #_lencomp:_3
#  1 #_lencomp:_4
#  1 #_lencomp:_5
#  1 #_lencomp:_6
#  1 #_lencomp:_7
#  1 #_lencomp:_8
#  1 #_lencomp:_9
#  1 #_lencomp:_10
#  1 #_lencomp:_11
#  1 #_lencomp:_12
#  1 #_lencomp:_13
#  1 #_lencomp:_14
#  0 #_lencomp:_15
#  1 #_lencomp:_16
#  0 #_lencomp:_17
#  0.51 #_sizefreq:_1
#  1 #_init_equ_catch
#  1 #_recruitments
#  0 #_parameter-priors
#  1 #_parameter-dev-vectors
#  1 #_crashPenLambda
#  1 # F_ballpark_lambda
0 # (0/1) read specs for more stddev reporting 
 # 0 1 -1 5 1 5 1 -1 5 # placeholder for selex type, len/age, year, N selex bins, Growth pattern, N growth ages, NatAge_area(-1 for all), NatAge_yr, N Natages
 # placeholder for vector of selex bins to be reported
 # placeholder for vector of growth ages to be reported
 # placeholder for vector of NatAges ages to be reported
999

