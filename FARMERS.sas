
libname custom "D:\NHIS-2018-1-308_고상백\공용\맞춤형DB";
libname sample "D:\NHIS-2018-1-308_고상백\공용\표본코호트";
libname farmer "D:\NHIS-2018-1-308_고상백\이솔암\farmer";
libname control "D:\NHIS-2018-1-308_고상백\이솔암\control";


/* Identifying potential candidates using BFC 2010 */

/* 1434867 individuals from custom db */

proc sql;
	select count(*) as INDI from custom.bfc_2010;

/* 1357616 individuals aged 40-89 */

proc sql;
	create table farmer.bfc_2010_4089 as
		select * from custom.bfc_2010 where yend_std_age >= 40 and yend_std_age < 90;

proc sql;
	select count(*) as INDI_AGED_40_89 from farmer.bfc_2010_4089;


/* 844684 individuals who had GE during 2010-2011 */

proc sql;
	create table farmer.g1eq_1011 as
		select * from custom.g1eq_2010 union select * from custom.g1eq_2011
			order by indi_dscm_no;

proc sql;
	/* remain only the latest GE record during 2010-2011 in an individual */ 
	create table farmer.g1eq_distinct as
		select distinct indi_dscm_no, max(hme_dt) as hme_dt
			from farmer.g1eq_1011
				group by indi_dscm_no;

proc sql;
	create table farmer.g1eq_latest as
		select a.*, b.*
			from farmer.g1eq_distinct as a, farmer.g1eq_1011 as b
				where a.indi_dscm_no = b.indi_dscm_no and a.hme_dt = b.hme_dt;

proc sql;
	/* merge bfc2010 aged 40-89 and the latest GE record */
	create table farmer.ge as
		select a.*, b.*
			from farmer.bfc_2010_4089 as a, farmer.g1eq_latest as b
				where a.indi_dscm_no = b.indi_dscm_no;

proc sql;
	select count(*) as INDI_AGED_40_89_GE from farmer.ge;


/* T20: Flagging ICD-10 for MI, stroke */

%macro _t20_flag_();

	%do year = 10 %to 16;
		proc sql;
			create table farmer.t20_flag_&year. as
				select *,
					/* I21: MI */
					case
						when substr(sick_sym1,1,3) = 'I21' or
							 substr(sick_sym2,1,3) = 'I21' or
							 substr(sick_sym3,1,3) = 'I21' or
							 substr(sick_sym4,1,3) = 'I21' or
							 substr(sick_sym5,1,3) = 'I21'
								then 1
						else 0
					end as mi_icd,
				    /* I60, I61, I62, I63, I64: stroke */
					case
						when substr(sick_sym1,1,3) in ( 'I60', 'I61', 'I62', 'I63', 'I64' ) or
							 substr(sick_sym2,1,3) in ( 'I60', 'I61', 'I62', 'I63', 'I64' ) or
							 substr(sick_sym3,1,3) in ( 'I60', 'I61', 'I62', 'I63', 'I64' ) or
							 substr(sick_sym4,1,3) in ( 'I60', 'I61', 'I62', 'I63', 'I64' ) or
							 substr(sick_sym5,1,3) in ( 'I60', 'I61', 'I62', 'I63', 'I64' ) 
								then 1
						else 0
					end as stroke_icd
				from custom.t20_&year.;
	%end;

%mend _t20_flag_;

%_t20_flag_();

/* T30: Flagging procedures and medications associated with MI, stroke */

%macro _t30_flag_();

	%do year = 10 %to 16;
		proc sql;
			create table farmer.t30_flag_&year. as
				select 
					*,
					/* Tests associated with MI */
					case
						when mcare_div_cd_adj in ( 'B2640', 'C3941', 'C3942', 'CY277', 'CY278', 'CY279' ) then 1
						else 0
					end as mi_test,

					/* Procedures associated with MI */
					case
						when mcare_div_cd_adj in ( 'HA670', 'HA680', 'HA681', 'HA682', 'M6551', 'M6552',
												   'M6561', 'M6562', 'M6563', 'M6564', 'M6571', 'M6572',
												   'M6633', 'M6634', 'O1641', 'O1642', 'O1647', 'OA641',
												   'OA642', 'OA647') then 1
						else 0
					end as mi_proc,

					/* Tests associated with stroke */
					case
						when mcare_div_cd_adj in ( 'HE101', 'HE102', 'HE135', 'HE136', 'HE501', 'HE502',
												   'HE535', 'HE201', 'HE235', 'HE236', 'HE301', 'HE302',
												   'HF101', 'HF102', 'HF103', 'HF104', 'HF105', 'HF106',
												   'HF107', 'HF201', 'HF202', 'HF203', 'HF305', 'HF306', 
												   'HA441', 'HA451', 'HA461', 'HA471', 'HA851', 'HA601',
												   'HA602', 'HA603', 'HA604', 'HA605' ) then 1
						else 0
					end as stroke_test,

					/* Procedures associated with stroke */
					case
						when mcare_div_cd_adj in ( 'M6631', 'M6632', 'M6633' ) then 1
						else 0
					end as stroke_proc,

					/* Medications associated with MI and/or stroke */
					case
						when mcare_div_cd_adj in ( '110701ATB', '110702ATB', '110801ATB', '110802ATB', '110902BIJ', '111001ACE',
												   '111001ACH', '111001ATB', '111001ATE', '111002ATE', '111003ACE', '111003ATE', 
												   '253000ATB', '253200ATB', '254300ACH', '256800ATB', '259100ACH', '263300ACH', 
												   '517900ACH',	'133201ACR', '133201ATB', '133202ATB', '133203ATR',	'136901ATB', 
												   '492501ATB', '498801ATB', '501501ATB', '517900ACH', '239201ATB', '239202ATB', 
												   '498900ATB',	'244101ACE', '244101ACH', '244102ACH', '168601BIJ', '168602BIJ', 
												   '168603BIJ', '168605BIJ', '168606BIJ', '168607BIJ', '168630BIJ', '168631BIJ',
												   '465801BIJ', '465802BIJ', '465830BIJ', '465831BIJ', '446301BIJ', '446302BIJ', 
												   '446303BIJ', '446304BIJ', '152101BIJ', '152102BIJ', '152103BIJ', '152104BIJ',
												   '152105BIJ', '152106BIJ', '152130BIJ', '152131BIJ', '152132BIJ', '152133BIJ', 
												   '152134BIJ', '152135BIJ', '140201BIJ', '140202BIJ', '140203BIJ', '140230BIJ',
												   '140231BIJ', '140232BIJ', '140233BIJ', '140234BIJ', '450101BIJ', '450130BIJ',
												   '198401BIJ', '198402BIJ', '198403BIJ', '198407BIJ', '198430BIJ', '198432BIJ',  
												   '198433BIJ', '447501BIJ', '447502BIJ', '447503BIJ', '447530BIJ', '447531BIJ', 
												   '447532BIJ',	'511401ATB', '511402ATB', '511403ATB', '511404ATB', '613701ACH', 
												   '613702ACH', '617001ATB', '617002ATB', '249103ATB', '249104ATB', '249105ATB', 
												   '315300BIJ', '226101ATB', '226103ATR', '174701ATB', '430301ACH', '263300ACH', 
												   '220501BIJ', '223502BIJ' ) then 1
						else 0
					end as medication
				from custom.t30_&year.;
	%end;

%mend _t30_flag_;

%_t30_flag_();

/* T30: Remain distinct cmn_key by summarizing flag variables */

%macro _t30_condense_();

	%do year = 10 %to 16;
		proc sql;
			create table farmer.t30_condense_&year. as
				select cmn_key,
					max(mi_test) as mi_test,
					max(mi_proc) as mi_proc,
					max(stroke_test) as stroke_test,
					max(stroke_proc) as stroke_proc,
					max(medication) as medication
					from farmer.t30_flag_&year.
					group by cmn_key;
	%end;

%mend _t30_condense_;

%_t30_condense_();

/* T2030: Join T20 and T30 on cmn_key so that T2030 have flag for ICD-10, tests, procedures, and medications associated with MI/stroke */

%macro _t2030_();

	%do year = 10 %to 16;
		proc sql;
			create table farmer.t2030_&year. as
				select a.cmn_key, a.indi_dscm_no, a.mdcare_strt_dt, 
					   a.mi_icd, b.mi_test, b.mi_proc, a.stroke_icd, b.stroke_test, b.stroke_proc, b.medication
					from farmer.t20_flag_&year. as a left join farmer.t30_condense_&year. as b
					on a.cmn_key = b.cmn_key;
	%end;

%mend _t2030_;

%_t2030_();

/*

&dz.: mi or stroke

MI: ICD-10 + test + procedure
Stroke: ICD-10 + test + procedure or medication

*/

%macro _diagnosis_();

	%do year = 10 %to 16;

		proc sql;
			create table farmer.mi_&year. as
				select distinct indi_dscm_no, min(mdcare_strt_dt) as diag_dt
					from farmer.t2030_&year.
						where mi_icd = 1 and mi_test = 1 and mi_proc = 1
							group by indi_dscm_no;

		proc sql;
			create table farmer.stroke_&year. as
				select distinct indi_dscm_no, min(mdcare_strt_dt) as diag_dt
					from farmer.t2030_&year.
						where stroke_icd = 1 and stroke_test = 1 and ( stroke_proc = 1 or medication = 1 )
							group by indi_dscm_no;

	%end;

%mend _diagnosis_;

%_diagnosis_();

/* Merge diagnosis during 2010-2016 into a single table */

%macro _diagnosis_merge_(dz);

	proc sql;
		create table farmer.&dz._1016 as
			select * from farmer.&dz._10
			union select * from farmer.&dz._11
			union select * from farmer.&dz._12
			union select * from farmer.&dz._13
			union select * from farmer.&dz._14
			union select * from farmer.&dz._15
			union select * from farmer.&dz._16;
		create table farmer.&dz. as
			select distinct indi_dscm_no, min(diag_dt) as diag_dt
				from farmer.&dz._1016
					group by indi_dscm_no;

%mend _diagnosis_merge_;

/* Merge GE table and MI/stroke identification table */

%macro _t2030ge_(dz);

	proc sql;
		create table work.merge as
			select a.*, b.diag_dt as &dz.
				from work.merge as a left join farmer.&dz. as b
					on a.indi_dscm_no = b.indi_dscm_no;

%mend _t2030ge_;

%_t2030ge_(mi);
%_t2030ge_(stroke);

proc sql;
	create table farmer.t2030ge as
		select *,
			/* The table is based on BFC 2010. An individuals' age should be added with +1 in case GE record of 2011 is recruited. */
			case
				when substr ( HME_DT, 1, 4 ) = '2010' then yend_std_age
				else yend_std_age + 1
			end as age
				from work.merge;

proc sql;
	create table farmer.initial as 
		select indi_dscm_no,
				case
					when sex_type = '1' then 0
					else 1
				end as sex,
				age,
				hme_dt as index,
				mi, stroke,
				g1e_hght as height, g1e_wght as weight, g1e_bmi as bmi, g1e_wstc as wc,
				g1e_bp_sys as sbp, g1e_bp_dia as dbp,
				g1e_hgb as hb, g1e_sgot as ast, g1e_sgpt as alt, g1e_ggt as ggt, g1e_crtn as cr,
				g1e_fbs as fbs, g1e_tot_chol as tchol, g1e_tg as tg, g1e_hdl as hdl, g1e_ldl as ldl,
				q_phx_dx_stk as hx_stroke, q_phx_dx_htdz as hx_htdz, q_phx_dx_htn as hx_htn, q_phx_dx_dm as hx_dm, q_phx_dx_dld as hx_dys,
				q_phx_tx_stk as tx_stroke, q_phx_tx_htdz as tx_htdz, q_phx_tx_htn as tx_htn, q_phx_tx_dm as tx_dm, q_phx_tx_dld as tx_dys,
				q_smk_pre_drt as pre_smoke_duration, q_smk_pre_amt as pre_smoke_amount, q_smk_now_drt as now_smoke_duration, q_smk_now_amt_v09n as now_smoke_amount,
				q_drk_frq_v09n as drink_frequency, q_drk_amt_v09n as drink_amount,
				q_pa_vd, q_pa_md, q_pa_walk,
				( ( 8.0 * 30 * q_pa_vd ) + ( 4.0 * 30 * q_pa_md ) + ( 3.3 * 30 * q_pa_walk ) ) as met,
				case
					when q_smk_yn = 1 then 0
					else 1
				end as ever_smoke,
				case
					when q_smk_yn = 3 then 1
					else 0
				end as current_smoke,
				case
					when q_drk_frq_v09n > 0 then 1
					else 0
				end as current_drink,
				case
					when calculated sex = 0 then
						case
							when wc >= 90 then 1
							else 0
						end
					else
						case
							when wc >= 85 then 1
							else 0
						end
				end as meta_wc,
				case
					when sbp >= 130 or dbp >= 85 or tx_htn = 1 then 1
					else 0
				end as meta_bp,
				case
					when fbs >= 100 or tx_dm = 1 then 1
					else 0
				end as meta_glc,
				case
					when calculated sex = 0 then
						case
							when hdl <= 40 or tx_dys = 1 then 1
							else 0
						end
					else
						case
							when hdl <= 50 or tx_dys = 1 then 1
							else 0
						end
				end as meta_hdl,
				case
					when tg >= 150 or tx_dys = 1 then 1
					else 0
				end as meta_tg,
			    ( calculated meta_wc + calculated meta_bp + calculated meta_glc + calculated meta_hdl + calculated meta_tg ) as meta_no,
				case
					when calculated meta_no >= 3 then 1
					else 0
				end as meta
			from farmer.t2030ge;

/* 844684 baseline individuals selected from population identification (1) */

proc sql;
	select count(*) as INDI_AGED_40_89_GE from farmer.initial;

proc sql;
	create table work.filtering as
		select * from farmer.initial
			where hx_htdz <> 1 and hx_stroke <> 1 and tx_htdz <> 1 and tx_stroke <> 1;


/* 789790 individuals without any prior diagnosis or treatment associated with heart disease and stroke */

proc sql;
	select count(*) as INDI_AGED_40_89_GE_NO_HX_TX from work.filtering;

/* Left join with death table */

proc sql;
	create table work.filtering as
		select a.*, b.dth_assmd_dt as death 
			from work.filtering as a left join custom.dth as b
				on a.indi_dscm_no = b.indi_dscm_no;

/* 789265 individuals after excluding individuals who were diagnosed as MI or stroke before index date (GE date) */

proc sql;
	create table work.filtering as
		select *
			from work.filtering
				where ( mi >= index or mi is missing ) and ( stroke >= index or stroke is missing );

proc sql;
	select count(*) as only_diagnosis_after_index from work.filtering;


/* 735311 individuals after excluding individuals  */

proc sql;
	create table work.filtering as
		select *
			from work.filtering
				where
					case
						when death is missing then 1
					    when ( mdy ( input(substr(death, 5, 2), 2.), input(substr(death, 7, 2), 2.), input(substr(death, 1, 4), 4.) ) - 
			     			   mdy ( input(substr(index, 5, 2), 2.), input(substr(index, 7, 2), 2.), input(substr(index, 1, 4), 4.) ) ) >= 365.25 * 5 then 1
						when mi is not missing and mi <= death then 1
						when stroke is not missing and stroke <= death then 1
						else 0
					end;

proc sql;
	select count(*) as only_diagnosis_after_index from work.filtering;


/* Time to event caluation and binary flagging for 5-year logistic regression */

proc sql;
	create table farmer.analysis as
		select *,
			   ( mdy ( input(substr(mi, 5, 2), 2.), input(substr(mi, 7, 2), 2.), input(substr(mi, 1, 4), 4.) ) - 
			     mdy ( input(substr(index, 5, 2), 2.), input(substr(index, 7, 2), 2.), input(substr(index, 1, 4), 4.) ) )			as mi_day,
			   	case
					when calculated mi_day <= 365.25 * 5 and calculated mi_day is not missing then 1
					else 0
				end as mi_5yr,
			   ( mdy ( input(substr(stroke, 5, 2), 2.), input(substr(stroke, 7, 2), 2.), input(substr(stroke, 1, 4), 4.) ) - 
			     mdy ( input(substr(index, 5, 2), 2.), input(substr(index, 7, 2), 2.), input(substr(index, 1, 4), 4.) ) )			as stroke_day,
			   	case
					when calculated stroke_day <= 365.25 * 5 and calculated stroke_day is not missing then 1
					else 0
				end as stroke_5yr

			  	from work.filtering;


data farmer.analysis;

	set farmer.analysis;

	age_group = int ( age / 10 );

	mi_fu = mdy ( input(substr(mi, 5, 2), 2.), input(substr(mi, 7, 2), 2.), input(substr(mi, 1, 4), 4.) ) -
	  		mdy ( input(substr(index, 5, 2), 2.), input(substr(index, 7, 2), 2.), input(substr(index, 1, 4), 4.) );

	stroke_fu = mdy ( input(substr(stroke, 5, 2), 2.), input(substr(stroke, 7, 2), 2.), input(substr(stroke, 1, 4), 4.) ) -
	  		    mdy ( input(substr(index, 5, 2), 2.), input(substr(index, 7, 2), 2.), input(substr(index, 1, 4), 4.) );

	death_fu = mdy ( input(substr(death, 5, 2), 2.), input(substr(death, 7, 2), 2.), input(substr(death, 1, 4), 4.) ) -
	  		   mdy ( input(substr(index, 5, 2), 2.), input(substr(index, 7, 2), 2.), input(substr(index, 1, 4), 4.) );
	
	if death_fu = . or death_fu >= 365.25 * 5 then
		do;
			death_fu = 365.25 * 5;
			death_event = 0;
		end;
	else
		death_event = 1;

	if mi_fu = . or mi_fu >= 365.25 * 5 then
		do;
			mi_fu = 365.25 * 5;
			mi_event = 0;
		end;
	else
		mi_event = 1;

	if stroke_fu = . or stroke_fu >= 365.25 * 5 then
		do;
			stroke_fu = 365.25 * 5;
			stroke_event = 0;
		end;
	else
		stroke_event = 1;

	mi_fu = min ( death_fu, mi_fu );
	stroke_fu = min ( death_fu, stroke_fu );

run;

			
/* Analysis ready: 735311 individuals */

proc sql;
	select count(*) as ready_to_analyze from farmer.analysis;

/* Identifying potential candidates using BFC 2010 */



/* 1038462 individuals from sample db */

proc sql;
	select count(*) as INDI from sample.nsc2_bnc where std_yyyy = '2009';


/* 473014 individuals aged 40-89 */

proc sql;
	create table control.bfc_2009_4089 as
		select a.*, b.bth_yyyy, ( input(a.std_yyyy, 4.) - input(b.bth_yyyy, 4.) ) as age 
			from sample.nsc2_bnc as a left join sample.nsc2_bnd as b
				on a.rn_indi = b.rn_indi
					where a.std_yyyy = '2009' and calculated age >=40 and calculated age < 90;
				
proc sql;
	select count(*) as INDI_AGED_40_89 from control.bfc_2009_4089;



/* 254883 individuals who had GE during 2009-2010 */

proc sql;
	create table control.g1eq_0910 as
		select * from sample.nsc2_g1e_0915
			where exmd_bz_yyyy = '2009' or exmd_bz_yyyy = '2010'
				order by rn_indi;

proc sql;
	/* remain only the latest GE record during 2010-2011 in an individual */ 
	create table control.g1eq_distinct as
		select distinct rn_indi, max(exmd_bz_yyyy) as exmd_bz_yyyy
			from control.g1eq_0910
				group by rn_indi;

proc sql;
	create table control.g1eq_latest as
		select a.*, b.*
			from control.g1eq_distinct as a, control.g1eq_0910 as b
				where a.rn_indi = b.rn_indi and a.exmd_bz_yyyy = b.exmd_bz_yyyy;

proc sql;
	/* merge bfc2009 aged 40-89 and the latest GE record */
	create table control.ge as
		select a.*, b.*
			from control.bfc_2009_4089 as a, control.g1eq_latest as b
				where a.rn_indi = b.rn_indi;

proc sql;
	select count(*) as INDI_AGED_40_89_GE from control.ge;


/* T20: Flagging ICD-10 for MI, stroke */

proc sql;
	create table control.t20_flag as
		select *,
			/* I21: MI */
			case
				when substr(sick_sym1,1,3) = 'I21' or
					 substr(sick_sym2,1,3) = 'I21'
						then 1
				else 0
			end as mi_icd,
		    /* I60, I61, I62, I63, I64: stroke */
			case
				when substr(sick_sym1,1,3) in ( 'I60', 'I61', 'I62', 'I63', 'I64' ) or
					 substr(sick_sym2,1,3) in ( 'I60', 'I61', 'I62', 'I63', 'I64' )
						then 1
				else 0
			end as stroke_icd
		from sample.nsc2_m20
			where input(std_yyyy, 4.) >= 2009 and input(std_yyyy, 4.) <= 2015;



/* T30: Flagging procedures and medications associated with MI, stroke */

proc sql;
	create table control.t30_flag as
		select 
			*,
			/* Tests associated with MI */
			case
				when mcare_div_cd in ( 'B2640', 'C3941', 'C3942', 'CY277', 'CY278', 'CY279' ) then 1
				else 0
			end as mi_test,

			/* Procedures associated with MI */
			case
				when mcare_div_cd in ( 'HA670', 'HA680', 'HA681', 'HA682', 'M6551', 'M6552',
										   'M6561', 'M6562', 'M6563', 'M6564', 'M6571', 'M6572',
										   'M6633', 'M6634', 'O1641', 'O1642', 'O1647', 'OA641',
										   'OA642', 'OA647') then 1
				else 0
			end as mi_proc,

			/* Tests associated with stroke */
			case
				when mcare_div_cd in ( 'HE101', 'HE102', 'HE135', 'HE136', 'HE501', 'HE502',
										   'HE535', 'HE201', 'HE235', 'HE236', 'HE301', 'HE302',
										   'HF101', 'HF102', 'HF103', 'HF104', 'HF105', 'HF106',
										   'HF107', 'HF201', 'HF202', 'HF203', 'HF305', 'HF306', 
										   'HA441', 'HA451', 'HA461', 'HA471', 'HA851', 'HA601',
										   'HA602', 'HA603', 'HA604', 'HA605' ) then 1
				else 0
			end as stroke_test,

			/* Procedures associated with stroke */
			case
				when mcare_div_cd in ( 'M6631', 'M6632', 'M6633' ) then 1
				else 0
			end as stroke_proc,

			/* Medications associated with MI and/or stroke */
			case
				when mcare_div_cd in ( '110701ATB', '110702ATB', '110801ATB', '110802ATB', '110902BIJ', '111001ACE',
										   '111001ACH', '111001ATB', '111001ATE', '111002ATE', '111003ACE', '111003ATE', 
										   '253000ATB', '253200ATB', '254300ACH', '256800ATB', '259100ACH', '263300ACH', 
										   '517900ACH',	'133201ACR', '133201ATB', '133202ATB', '133203ATR',	'136901ATB', 
										   '492501ATB', '498801ATB', '501501ATB', '517900ACH', '239201ATB', '239202ATB', 
										   '498900ATB',	'244101ACE', '244101ACH', '244102ACH', '168601BIJ', '168602BIJ', 
										   '168603BIJ', '168605BIJ', '168606BIJ', '168607BIJ', '168630BIJ', '168631BIJ',
										   '465801BIJ', '465802BIJ', '465830BIJ', '465831BIJ', '446301BIJ', '446302BIJ', 
										   '446303BIJ', '446304BIJ', '152101BIJ', '152102BIJ', '152103BIJ', '152104BIJ',
										   '152105BIJ', '152106BIJ', '152130BIJ', '152131BIJ', '152132BIJ', '152133BIJ', 
										   '152134BIJ', '152135BIJ', '140201BIJ', '140202BIJ', '140203BIJ', '140230BIJ',
										   '140231BIJ', '140232BIJ', '140233BIJ', '140234BIJ', '450101BIJ', '450130BIJ',
										   '198401BIJ', '198402BIJ', '198403BIJ', '198407BIJ', '198430BIJ', '198432BIJ',  
										   '198433BIJ', '447501BIJ', '447502BIJ', '447503BIJ', '447530BIJ', '447531BIJ', 
										   '447532BIJ',	'511401ATB', '511402ATB', '511403ATB', '511404ATB', '613701ACH', 
										   '613702ACH', '617001ATB', '617002ATB', '249103ATB', '249104ATB', '249105ATB', 
										   '315300BIJ', '226101ATB', '226103ATR', '174701ATB', '430301ACH', '263300ACH', 
										   '220501BIJ', '223502BIJ' ) then 1
				else 0
			end as medication
			from sample.nsc2_m30
				where input(substr(mdcare_strt_dt, 1, 4), 4.) >= 2009 and input(substr(mdcare_strt_dt, 1, 4), 4.) <= 2015;


/* T30: Remain distinct cmn_key by summarizing flag variables */

proc sql;
	create table control.t30_condense as
		select rn_key,
			max(mi_test) as mi_test,
			max(mi_proc) as mi_proc,
			max(stroke_test) as stroke_test,
			max(stroke_proc) as stroke_proc,
			max(medication) as medication
				from control.t30_flag
					group by rn_key;


/* T2030: Join T20 and T30 on cmn_key so that T2030 have flag for ICD-10, tests, procedures, and medications associated with MI/stroke */

proc sql;
	create table control.t2030 as
		select a.rn_key, a.rn_indi, a.mdcare_strt_dt, 
			   a.mi_icd, b.mi_test, b.mi_proc, a.stroke_icd, b.stroke_test, b.stroke_proc, b.medication
			from control.t20_flag as a left join control.t30_condense as b
				on a.rn_key = b.rn_key;


/*

&dz.: mi or stroke

MI: ICD-10 + test + procedure
Stroke: ICD-10 + test + procedure or medication

*/

proc sql;
	create table control.mi as
		select distinct rn_indi, min(mdcare_strt_dt) as diag_dt
			from control.t2030
				where mi_icd = 1 and mi_test = 1 and mi_proc = 1
					group by rn_indi;

proc sql;
	create table control.stroke as
		select distinct rn_indi, min(mdcare_strt_dt) as diag_dt
			from control.t2030
				where stroke_icd = 1 and stroke_test = 1 and ( stroke_proc = 1 or medication = 1 )
					group by rn_indi;


/* Merge diagnosis in 2009-2015 in a single table */

%macro _diagnosis_merge_(dz);

	proc sql;
		create table control.&dz. as
			select distinct rn_indi, min(diag_dt) as diag_dt
				from control.&dz.
					group by rn_indi;

%mend _diagnosis_merge_;

%_diagnosis_merge_(mi);
%_diagnosis_merge_(stroke);


proc sql;
	create table work.merge as
		select * from control.ge;

/* Merge GE table and MI/stroke identification table  */

%macro _t2030ge_(dz, def);

	proc sql;
		create table work.merge as
			select a.*, b.diag_dt as &dz.
				from work.merge as a left join control.&dz. as b
					on a.rn_indi = b.rn_indi;

%mend _t2030ge_;

%_t2030ge_(mi);
%_t2030ge_(stroke);

proc sql;
	create table control.t2030ge as
		select *,
			/* The table is based on BFC 2009. An individuals' age should be added with +1 in case GE record of 2010 is recruited. */
			case
				when substr ( hme_yyyymm, 1, 4 ) = '2009' then age
				else age + 1
			end as age
				from work.merge;

proc sql;
	create table control.initial as 
		select 	rn_indi,
				case
					when sex = '1' then 0
					else 1
				end as sex,
				age,
				cats(hme_yyyymm, '15') as index,
				mi, stroke,
				g1e_hght as height, g1e_wght as weight, g1e_bmi as bmi, g1e_wstc as wc,
				g1e_bp_sys as sbp, g1e_bp_dia as dbp,
				g1e_hgb as hb, g1e_sgot as ast, g1e_sgpt as alt, g1e_ggt as ggt, g1e_crtn as cr,
				g1e_fbs as fbs, g1e_tot_chol as tchol, g1e_tg as tg, g1e_hdl as hdl, g1e_ldl as ldl,
				q_phx_dx_stk as hx_stroke, q_phx_dx_htdz as hx_htdz, q_phx_dx_htn as hx_htn, q_phx_dx_dm as hx_dm, q_phx_dx_dld as hx_dys,
				q_phx_tx_stk as tx_stroke, q_phx_tx_htdz as tx_htdz, q_phx_tx_htn as tx_htn, q_phx_tx_dm as tx_dm, q_phx_tx_dld as tx_dys,
				q_smk_pre_drt as pre_smoke_duration, q_smk_pre_amt as pre_smoke_amount, q_smk_now_drt as now_smoke_duration, q_smk_now_amt_v09n as now_smoke_amount,
				q_drk_frq_v09n as drink_frequency, q_drk_amt_v09n as drink_amount,
				q_pa_vd, q_pa_md, q_pa_walk,
				( ( 8.0 * 30 * q_pa_vd ) + ( 4.0 * 30 * q_pa_md ) + ( 3.3 * 30 * q_pa_walk ) ) as met,
				case
					when q_smk_yn = 1 then 0
					else 1
				end as ever_smoke,
				case
					when q_smk_yn = 3 then 1
					else 0
				end as current_smoke,
				case
					when q_drk_frq_v09n > 0 then 1
					else 0
				end as current_drink,
				case
					when calculated sex = 0 then
						case
							when wc >= 90 then 1
							else 0
						end
					else
						case
							when wc >= 85 then 1
							else 0
						end
				end as meta_wc,
				case
					when sbp >= 130 or dbp >= 85 or tx_htn = 1 then 1
					else 0
				end as meta_bp,
				case
					when fbs >= 100 or tx_dm = 1 then 1
					else 0
				end as meta_glc,
				case
					when calculated sex = 0 then
						case
							when hdl <= 40 or tx_dys = 1 then 1
							else 0
						end
					else
						case
							when hdl <= 50 or tx_dys = 1 then 1
							else 0
						end
				end as meta_hdl,
				case
					when tg >= 150 or tx_dys = 1 then 1
					else 0
				end as meta_tg,
			    ( calculated meta_wc + calculated meta_bp + calculated meta_glc + calculated meta_hdl + calculated meta_tg ) as meta_no,
				case
					when calculated meta_no >= 3 then 1
					else 0
				end as meta
			from control.t2030ge;


/* 254883 baseline individuals selected from population identification (1) */

proc sql;
	select count(*) as INDI_AGED_40_89_GE from control.initial;

proc sql;
	create table work.filtering as
		select * from control.initial
			where hx_htdz <> 1 and hx_stroke <> 1 and tx_htdz <> 1 and tx_stroke <> 1;


/* 245034 individuals without any prior diagnosis or treatment associated with heart disease and stroke */

proc sql;
	select count(*) as INDI_AGED_40_89_GE_NO_HX_TX from work.filtering;

/* Left join with death table */

proc sql;
	create table work.filtering as
		select a.*,
			case
				when b.dth_yyyymm is not missing then cats(b.dth_yyyymm, '15')
				else b.dth_yyyymm
			end as death 
			from work.filtering as a left join sample.nsc2_bnd as b
				on a.rn_indi = b.rn_indi;

/* 244959 individuals after excluding individuals who were diagnosed as MI or stroke before index date (GE date) */

proc sql;
	create table work.filtering as
		select *
			from work.filtering
				where ( mi >= index or mi is missing ) and ( stroke >= index or stroke is missing );

proc sql;
	select count(*) as only_diagnosis_after_index from work.filtering;


/* 238339 individuals after excluding individuals  */

proc sql;
	create table work.filtering as
		select *
			from work.filtering
				where
					case
						when death is missing then 1
					    when ( mdy ( input(substr(death, 5, 2), 2.), input(substr(death, 7, 2), 2.), input(substr(death, 1, 4), 4.) ) - 
			     			   mdy ( input(substr(index, 5, 2), 2.), input(substr(index, 7, 2), 2.), input(substr(index, 1, 4), 4.) ) ) >= 365.25 * 5 then 1
						when mi is not missing and mi <= death then 1
						when stroke is not missing and stroke <= death then 1
						else 0
					end;

proc sql;
	select count(*) as only_diagnosis_after_index from work.filtering;


/* Time to event caluation and binary flagging for 5-year logistic regression */

proc sql;
	create table control.analysis as
		select *,
			   ( mdy ( input(substr(mi, 5, 2), 2.), input(substr(mi, 7, 2), 2.), input(substr(mi, 1, 4), 4.) ) - 
			     mdy ( input(substr(index, 5, 2), 2.), input(substr(index, 7, 2), 2.), input(substr(index, 1, 4), 4.) ) )			as mi_day,
			   	case
					when calculated mi_day <= 365.25 * 5 and calculated mi_day is not missing then 1
					else 0
				end as mi_5yr,
			   ( mdy ( input(substr(stroke, 5, 2), 2.), input(substr(stroke, 7, 2), 2.), input(substr(stroke, 1, 4), 4.) ) - 
			     mdy ( input(substr(index, 5, 2), 2.), input(substr(index, 7, 2), 2.), input(substr(index, 1, 4), 4.) ) )			as stroke_day,
			   	case
					when calculated stroke_day <= 365.25 * 5 and calculated stroke_day is not missing then 1
					else 0
				end as stroke_5yr
			  	from work.filtering;

data control.analysis;

	set control.analysis;

	age_group = int ( age / 10 );

	mi_fu = mdy ( input(substr(mi, 5, 2), 2.), input(substr(mi, 7, 2), 2.), input(substr(mi, 1, 4), 4.) ) -
	  		mdy ( input(substr(index, 5, 2), 2.), input(substr(index, 7, 2), 2.), input(substr(index, 1, 4), 4.) );

	stroke_fu = mdy ( input(substr(stroke, 5, 2), 2.), input(substr(stroke, 7, 2), 2.), input(substr(stroke, 1, 4), 4.) ) -
	  		    mdy ( input(substr(index, 5, 2), 2.), input(substr(index, 7, 2), 2.), input(substr(index, 1, 4), 4.) );

	death_fu = mdy ( input(substr(death, 5, 2), 2.), input(substr(death, 7, 2), 2.), input(substr(death, 1, 4), 4.) ) -
	  		   mdy ( input(substr(index, 5, 2), 2.), input(substr(index, 7, 2), 2.), input(substr(index, 1, 4), 4.) );
	
	if death_fu = . or death_fu >= 365.25 * 5 then
		do;
			death_fu = 365.25 * 5;
			death_event = 0;
		end;
	else
		death_event = 1;

	if mi_fu = . or mi_fu >= 365.25 * 5 then
		do;
			mi_fu = 365.25 * 5;
			mi_event = 0;
		end;
	else
		mi_event = 1;

	if stroke_fu = . or stroke_fu >= 365.25 * 5 then
		do;
			stroke_fu = 365.25 * 5;
			stroke_event = 0;
		end;
	else
		stroke_event = 1;

	mi_fu = min ( death_fu, mi_fu );
	stroke_fu = min ( death_fu, stroke_fu );

run;



/* Analysis ready: 238339 individuals */

proc sql;
	select count(*) as ready_to_analyze from control.analysis;

%macro _person_year_(group);

data work.&group._py;

	set &group..analysis;

	mi_py = mi_fu / 365.25;
	stroke_py = stroke_fu / 365.25;
	death_py = death_fu / 365.25;
	
	mi_py = min ( death_py, mi_py );
	stroke_py = min ( death_py, stroke_py );

	keep age_group index mi_itpom stroke_itpom death mi_py mi_event stroke_py stroke_event death_py death_event;

run;

title &group.;

proc sql;
	select	age_group,
			count ( * ) as n,
			sum ( mi_py ) as mi_py,
			sum ( mi_event ) as mi_event,
			sum ( stroke_py ) as stroke_py,
			sum ( stroke_event ) as stroke_event,
			sum ( death_py ) as death_py,
			sum ( death_event ) as death_event
		from work.&group._py
			group by age_group;
			
			
%mend _person_year_;


%_person_year_(farmer);
%_person_year_(control);




/* 5-year logistic regression */

%macro _5yr_logistic_(group, dz);

	proc logistic data=&group..analysis descending;
		title &group. - &dz. : model 1;
		class sex hx_htn hx_dm hx_dys ever_smoke current_smoke meta_bp meta_wc meta_glc meta_hdl meta_tg meta / descending;
		model &dz._5yr = sex hx_htn hx_dm hx_dys ever_smoke current_smoke age bmi cr ggt meta_bp meta_wc meta_glc meta_hdl meta_tg;
	run;

	proc logistic data=&group..analysis descending;
		title &group. - &dz. : model 2;
		class sex hx_htn hx_dm hx_dys ever_smoke current_smoke meta_bp meta_wc meta_glc meta_hdl meta_tg meta / descending;
		model &dz._5yr = sex hx_htn hx_dm hx_dys ever_smoke current_smoke age bmi wc sbp dbp ggt cr fbs tchol tg hdl ldl meta_no;
	run;

	proc logistic data=&group..analysis descending;
		title &group. - &dz. : model 3;
		class sex hx_htn hx_dm hx_dys ever_smoke current_smoke meta_bp meta_wc meta_glc meta_hdl meta_tg meta / descending;
		model &dz._5yr = sex hx_htn hx_dm hx_dys ever_smoke current_smoke age bmi wc sbp dbp ggt cr fbs tchol tg hdl ldl 
					   meta_bp meta_wc meta_glc meta_hdl meta_tg;
	run;

%mend _5yr_logistic_;

%_5yr_logistic_(farmer, mi);
%_5yr_logistic_(farmer, stroke);
%_5yr_logistic_(control, mi);
%_5yr_logistic_(control, stroke);



%macro _cox_ph_(group, dz);

	proc phreg data=&group..analysis;
		title &group. - &dz. : model 1;
		class sex(ref='0') hx_htn(ref='0') hx_dm(ref='0') hx_dys(ref='0') ever_smoke(ref='0') current_smoke(ref='0') meta_bp(ref='0') meta_wc(ref='0') meta_glc(ref='0') meta_hdl(ref='0') meta_tg(ref='0') meta(ref='0');
		model &dz._fu * &dz._event(0) = sex hx_htn hx_dm hx_dys ever_smoke current_smoke age bmi cr ggt meta_bp meta_wc meta_glc meta_hdl meta_tg / RL;
	run;

	proc phreg data=&group..analysis;
		title &group. - &dz. : model 2;
		class sex(ref='0') hx_htn(ref='0') hx_dm(ref='0') hx_dys(ref='0') ever_smoke(ref='0') current_smoke(ref='0') meta_bp(ref='0') meta_wc(ref='0') meta_glc(ref='0') meta_hdl(ref='0') meta_tg(ref='0') meta(ref='0');
		model &dz._fu * &dz._event(0) = sex hx_htn hx_dm hx_dys ever_smoke current_smoke age bmi wc sbp dbp ggt cr fbs tchol tg hdl ldl / RL;
	run;

	proc phreg data=&group..analysis;
		title &group. - &dz. : model 3;
		class sex(ref='0') hx_htn(ref='0') hx_dm(ref='0') hx_dys(ref='0') ever_smoke(ref='0') current_smoke(ref='0') meta_bp(ref='0') meta_wc(ref='0') meta_glc(ref='0') meta_hdl(ref='0') meta_tg(ref='0') meta(ref='0');
		model &dz._fu * &dz._event(0) = sex hx_htn hx_dm hx_dys ever_smoke current_smoke age bmi wc sbp dbp ggt cr fbs tchol tg hdl ldl
					    			    meta_bp meta_wc meta_glc meta_hdl meta_tg / RL;
	run;

%mend _cox_ph_;

%_cox_ph_(farmer, mi);
%_cox_ph_(farmer, stroke);
%_cox_ph_(control, mi);
%_cox_ph_(control, stroke);


/* Train:test set 7:3 split */

proc surveyselect data=farmer.analysis rate=.7 outall out=work.farmer_split seed=390496001;
run;

proc sql;
	create table work.farmer_train as
		select * from work.farmer_split where selected = 1;
	create table work.farmer_test as
		select * from work.farmer_split where selected = 0;


%macro _5yr_prediction_model_(dz);

	/* 5-year MI logistic regression */

	title &dz. - control cohort;
	proc logistic data=control.analysis outest=control_mi_coeff descending;
		class sex hx_htn hx_dm hx_dys ever_smoke current_smoke meta_bp meta_wc meta_glc meta_hdl meta_tg meta;
		model &dz._5yr = sex hx_htn hx_dm hx_dys ever_smoke current_smoke current_drink age bmi wc sbp dbp ggt cr fbs tchol tg hdl ldl
					   meta_bp meta_wc meta_glc meta_hdl meta_tg meta;
	run;

	title &dz. - farmer train cohort;
	proc logistic data=work.farmer_train outest=farmer_mi_coeff descending;
		class sex hx_htn hx_dm hx_dys ever_smoke current_smoke meta_bp meta_wc meta_glc meta_hdl meta_tg meta;
		model &dz._5yr = sex hx_htn hx_dm hx_dys ever_smoke current_smoke current_drink age bmi wc sbp dbp ggt cr fbs tchol tg hdl ldl
					   meta_bp meta_wc meta_glc meta_hdl meta_tg meta;
	run;

	title &dz. - validation of control-based model for farmer test cohort;
	proc logistic data=work.farmer_test inest=control_mi_coeff descending;
		class sex hx_htn hx_dm hx_dys ever_smoke current_smoke meta_bp meta_wc meta_glc meta_hdl meta_tg meta;
		model &dz._5yr = sex hx_htn hx_dm hx_dys ever_smoke current_smoke current_drink age bmi wc sbp dbp ggt cr fbs tchol tg hdl ldl
					   meta_bp meta_wc meta_glc meta_hdl meta_tg meta / outroc=&dz._pred_control_model maxiter = 0;
	run;

	title &dz. - validation of farmer-based model for farmer test cohort;
	ods graphics on;
	proc logistic data=work.farmer_test inest=farmer_mi_coeff descending;
		class sex hx_htn hx_dm hx_dys ever_smoke current_smoke meta_bp meta_wc meta_glc meta_hdl meta_tg meta;
		model &dz._5yr = sex hx_htn hx_dm hx_dys ever_smoke current_smoke current_drink age bmi wc sbp dbp ggt cr fbs tchol tg hdl ldl
					   meta_bp meta_wc meta_glc meta_hdl meta_tg meta / outroc=&dz._pred_farmer_model maxiter = 0;
	run;


	ods pdf file="D:\NHIS-2018-1-308_고상백\이솔암\&dz._pred_control_model.pdf";
		ods layout start width=5in height=5in;
			symbol1 v=dot i=none c=black;
			proc gplot data=&dz._pred_control_model;
				plot _sensit_ * _1mspec_ = 1;
			run;
		ods layout end;
	ods pdf close;

	ods pdf file="D:\NHIS-2018-1-308_고상백\이솔암\&dz._pred_farmer_model.pdf";
		ods layout start width=5in height=5in;
			symbol1 v=dot i=none c=black;
			proc gplot data=&dz._pred_farmer_model;
				plot _sensit_ * _1mspec_ = 1;
			run;
		ods layout end;
	ods pdf close;


%mend _5yr_prediction_model;

%_5yr_prediction_model_(mi);
%_5yr_prediction_model_(stroke);

/* sex hx_htn hx_dm hx_dys un_htn un_dm un_dys ever_smoke current_smoke age bmi wc sbp dbp ggt cr fbs tchol tg hdl ldl met */

proc sql;
	create table work.demographics as
		select *, 1 as group from farmer.analysis
		union
		select *, 0 as group from control.analysis;
run;

proc ttest
		data = work.demographics
		plots = none
		alpha = 0.05
		h0 = 0
		ci = equal;
	class group;
	var age bmi wc sbp dbp ggt cr fbs tchol tg hdl ldl met meta_no;
run;

proc freq data = work.demographics;
	table sex * group / chisq cmh;
	table hx_htn * group / chisq cmh;
	table hx_dm * group / chisq cmh;
	table hx_dys * group / chisq cmh;
	table ever_smoke * group / chisq cmh;
	table current_smoke * group / chisq cmh;
	table current_drink * group / chisq cmh;
	table meta_bp * group / chisq cmh;
	table meta_wc * group / chisq cmh;
	table meta_glc * group / chisq cmh;
	table meta_hdl * group / chisq cmh;
	table meta_tg * group / chisq cmh;
	table meta_no * group / chisq cmh;
	table meta * group / chisq cmh;
run;


proc sql;
	create table mi_control_x_y as
		select _prob_ as c, _sensit_ as x, _1mspec_ as y
			from mi_pred_control_model;
	create table mi_farmer_x_y as
		select _prob_ as c, _sensit_ as x, _1mspec_ as y
			from mi_pred_farmer_model;
	create table stroke_control_x_y as
		select _prob_ as c, _sensit_ as x, _1mspec_ as y
			from stroke_pred_control_model;
	create table stroke_farmer_x_y as
		select _prob_ as c, _sensit_ as x, _1mspec_ as y
			from stroke_pred_farmer_model;


