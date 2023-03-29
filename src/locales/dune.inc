

(install
 (package camomile)
 (section (site (camomile locales)))
 (files
   af.mar
af_ZA.mar
am.mar
am_ET.mar
ar.mar
ar_AE.mar
ar_BH.mar
ar_DZ.mar
ar_EG.mar
ar_IN.mar
ar_IQ.mar
ar_JO.mar
ar_KW.mar
ar_LB.mar
ar_LY.mar
ar_MA.mar
ar_OM.mar
ar_QA.mar
ar_SA.mar
ar_SD.mar
ar_SY.mar
ar_TN.mar
ar_YE.mar
az.mar
az_AZ.mar
be.mar
be_BY.mar
bg.mar
bg_BG.mar
bn.mar
bn_IN.mar
ca.mar
ca_ES.mar
ca_ES_PREEURO.mar
cs.mar
cs_CZ.mar
da.mar
da_DK.mar
de.mar
de_AT.mar
de_AT_PREEURO.mar
de_BE.mar
de_CH.mar
de_DE.mar
de_DE_PREEURO.mar
de_LI.mar
de_LU.mar
de_LU_PREEURO.mar
de__PHONEBOOK.mar
div.mar
div_MV.mar
el.mar
el_GR.mar
el_GR_PREEURO.mar
en.mar
en_AS.mar
en_AU.mar
en_BE.mar
en_BE_PREEURO.mar
en_BW.mar
en_BZ.mar
en_CA.mar
en_GB.mar
en_GB_EURO.mar
en_GU.mar
en_HK.mar
en_IE.mar
en_IE_PREEURO.mar
en_IN.mar
en_JM.mar
en_MH.mar
en_MP.mar
en_MT.mar
en_NZ.mar
en_PH.mar
en_SG.mar
en_TT.mar
en_UM.mar
en_US.mar
en_US_POSIX.mar
en_VI.mar
en_ZA.mar
en_ZW.mar
eo.mar
es.mar
es_AR.mar
es_BO.mar
es_CL.mar
es_CO.mar
es_CR.mar
es_DO.mar
es_EC.mar
es_ES.mar
es_ES_PREEURO.mar
es_GT.mar
es_HN.mar
es_MX.mar
es_NI.mar
es_PA.mar
es_PE.mar
es_PR.mar
es_PY.mar
es_SV.mar
es_US.mar
es_UY.mar
es_VE.mar
es__TRADITIONAL.mar
et.mar
et_EE.mar
eu.mar
eu_ES.mar
eu_ES_PREEURO.mar
fa.mar
fa_IN.mar
fa_IR.mar
fi.mar
fi_FI.mar
fi_FI_PREEURO.mar
fo.mar
fo_FO.mar
fr.mar
fr_BE.mar
fr_BE_PREEURO.mar
fr_CA.mar
fr_CH.mar
fr_FR.mar
fr_FR_PREEURO.mar
fr_LU.mar
fr_LU_PREEURO.mar
fr_MC.mar
ga.mar
ga_IE.mar
ga_IE_PREEURO.mar
gl.mar
gl_ES.mar
gl_ES_PREEURO.mar
gu.mar
gu_IN.mar
gv.mar
gv_GB.mar
he.mar
he_IL.mar
hi.mar
hi_IN.mar
hi__DIRECT.mar
hr.mar
hr_HR.mar
hu.mar
hu_HU.mar
hy.mar
hy_AM.mar
hy_AM_REVISED.mar
id.mar
id_ID.mar
index.mar
is.mar
is_IS.mar
it.mar
it_CH.mar
it_IT.mar
it_IT_PREEURO.mar
iw.mar
iw_IL.mar
ja.mar
ja_JP.mar
ka.mar
ka_GE.mar
kk.mar
kk_KZ.mar
kl.mar
kl_GL.mar
kn.mar
kn_IN.mar
ko.mar
ko_KR.mar
kok.mar
kok_IN.mar
kw.mar
kw_GB.mar
ky.mar
ky_KG.mar
lt.mar
lt_LT.mar
lv.mar
lv_LV.mar
mk.mar
mk_MK.mar
mn.mar
mn_MN.mar
mr.mar
mr_IN.mar
ms.mar
ms_BN.mar
ms_MY.mar
mt.mar
mt_MT.mar
nb.mar
nb_NO.mar
nl.mar
nl_BE.mar
nl_BE_PREEURO.mar
nl_NL.mar
nl_NL_PREEURO.mar
nn.mar
nn_NO.mar
no.mar
no_NO.mar
no_NO_NY.mar
om.mar
om_ET.mar
om_KE.mar
pa.mar
pa_IN.mar
pl.mar
pl_PL.mar
pt.mar
pt_BR.mar
pt_PT.mar
pt_PT_PREEURO.mar
ro.mar
ro_RO.mar
root.mar
ru.mar
ru_RU.mar
ru_UA.mar
sa.mar
sa_IN.mar
sh.mar
sh_YU.mar
sk.mar
sk_SK.mar
sl.mar
sl_SI.mar
so.mar
so_DJ.mar
so_ET.mar
so_KE.mar
so_SO.mar
sq.mar
sq_AL.mar
sr.mar
sr_YU.mar
sv.mar
sv_FI.mar
sv_SE.mar
sw.mar
sw_KE.mar
sw_TZ.mar
syr.mar
syr_SY.mar
ta.mar
ta_IN.mar
te.mar
te_IN.mar
test_1.mar
th.mar
th_TH.mar
ti.mar
ti_ER.mar
ti_ET.mar
tr.mar
tr_TR.mar
tt.mar
tt_RU.mar
uk.mar
uk_UA.mar
ur.mar
ur_PK.mar
uz.mar
uz_UZ.mar
vi.mar
vi_VN.mar
zh.mar
zh_CN.mar
zh_HK.mar
zh_MO.mar
zh_SG.mar
zh_TW.mar
zh_TW_STROKE.mar
zh__PINYIN.mar
 ))

(alias
 (name database)
 (deps (glob_files ../database/*.mar)))


(rule
 (targets af.mar)
 (deps    (:x af.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets af_ZA.mar)
 (deps    (:x af_ZA.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets am.mar)
 (deps    (:x am.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets am_ET.mar)
 (deps    (:x am_ET.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ar.mar)
 (deps    (:x ar.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ar_AE.mar)
 (deps    (:x ar_AE.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ar_BH.mar)
 (deps    (:x ar_BH.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ar_DZ.mar)
 (deps    (:x ar_DZ.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ar_EG.mar)
 (deps    (:x ar_EG.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ar_IN.mar)
 (deps    (:x ar_IN.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ar_IQ.mar)
 (deps    (:x ar_IQ.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ar_JO.mar)
 (deps    (:x ar_JO.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ar_KW.mar)
 (deps    (:x ar_KW.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ar_LB.mar)
 (deps    (:x ar_LB.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ar_LY.mar)
 (deps    (:x ar_LY.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ar_MA.mar)
 (deps    (:x ar_MA.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ar_OM.mar)
 (deps    (:x ar_OM.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ar_QA.mar)
 (deps    (:x ar_QA.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ar_SA.mar)
 (deps    (:x ar_SA.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ar_SD.mar)
 (deps    (:x ar_SD.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ar_SY.mar)
 (deps    (:x ar_SY.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ar_TN.mar)
 (deps    (:x ar_TN.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ar_YE.mar)
 (deps    (:x ar_YE.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets az.mar)
 (deps    (:x az.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets az_AZ.mar)
 (deps    (:x az_AZ.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets be.mar)
 (deps    (:x be.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets be_BY.mar)
 (deps    (:x be_BY.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets bg.mar)
 (deps    (:x bg.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets bg_BG.mar)
 (deps    (:x bg_BG.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets bn.mar)
 (deps    (:x bn.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets bn_IN.mar)
 (deps    (:x bn_IN.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ca.mar)
 (deps    (:x ca.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ca_ES.mar)
 (deps    (:x ca_ES.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ca_ES_PREEURO.mar)
 (deps    (:x ca_ES_PREEURO.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets cs.mar)
 (deps    (:x cs.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets cs_CZ.mar)
 (deps    (:x cs_CZ.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets da.mar)
 (deps    (:x da.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets da_DK.mar)
 (deps    (:x da_DK.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets de.mar)
 (deps    (:x de.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets de_AT.mar)
 (deps    (:x de_AT.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets de_AT_PREEURO.mar)
 (deps    (:x de_AT_PREEURO.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets de_BE.mar)
 (deps    (:x de_BE.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets de_CH.mar)
 (deps    (:x de_CH.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets de_DE.mar)
 (deps    (:x de_DE.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets de_DE_PREEURO.mar)
 (deps    (:x de_DE_PREEURO.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets de_LI.mar)
 (deps    (:x de_LI.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets de_LU.mar)
 (deps    (:x de_LU.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets de_LU_PREEURO.mar)
 (deps    (:x de_LU_PREEURO.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets de__PHONEBOOK.mar)
 (deps    (:x de__PHONEBOOK.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets div.mar)
 (deps    (:x div.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets div_MV.mar)
 (deps    (:x div_MV.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets el.mar)
 (deps    (:x el.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets el_GR.mar)
 (deps    (:x el_GR.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets el_GR_PREEURO.mar)
 (deps    (:x el_GR_PREEURO.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets en.mar)
 (deps    (:x en.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets en_AS.mar)
 (deps    (:x en_AS.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets en_AU.mar)
 (deps    (:x en_AU.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets en_BE.mar)
 (deps    (:x en_BE.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets en_BE_PREEURO.mar)
 (deps    (:x en_BE_PREEURO.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets en_BW.mar)
 (deps    (:x en_BW.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets en_BZ.mar)
 (deps    (:x en_BZ.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets en_CA.mar)
 (deps    (:x en_CA.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets en_GB.mar)
 (deps    (:x en_GB.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets en_GB_EURO.mar)
 (deps    (:x en_GB_EURO.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets en_GU.mar)
 (deps    (:x en_GU.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets en_HK.mar)
 (deps    (:x en_HK.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets en_IE.mar)
 (deps    (:x en_IE.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets en_IE_PREEURO.mar)
 (deps    (:x en_IE_PREEURO.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets en_IN.mar)
 (deps    (:x en_IN.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets en_JM.mar)
 (deps    (:x en_JM.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets en_MH.mar)
 (deps    (:x en_MH.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets en_MP.mar)
 (deps    (:x en_MP.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets en_MT.mar)
 (deps    (:x en_MT.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets en_NZ.mar)
 (deps    (:x en_NZ.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets en_PH.mar)
 (deps    (:x en_PH.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets en_SG.mar)
 (deps    (:x en_SG.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets en_TT.mar)
 (deps    (:x en_TT.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets en_UM.mar)
 (deps    (:x en_UM.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets en_US.mar)
 (deps    (:x en_US.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets en_US_POSIX.mar)
 (deps    (:x en_US_POSIX.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets en_VI.mar)
 (deps    (:x en_VI.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets en_ZA.mar)
 (deps    (:x en_ZA.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets en_ZW.mar)
 (deps    (:x en_ZW.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets eo.mar)
 (deps    (:x eo.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets es.mar)
 (deps    (:x es.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets es_AR.mar)
 (deps    (:x es_AR.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets es_BO.mar)
 (deps    (:x es_BO.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets es_CL.mar)
 (deps    (:x es_CL.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets es_CO.mar)
 (deps    (:x es_CO.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets es_CR.mar)
 (deps    (:x es_CR.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets es_DO.mar)
 (deps    (:x es_DO.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets es_EC.mar)
 (deps    (:x es_EC.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets es_ES.mar)
 (deps    (:x es_ES.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets es_ES_PREEURO.mar)
 (deps    (:x es_ES_PREEURO.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets es_GT.mar)
 (deps    (:x es_GT.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets es_HN.mar)
 (deps    (:x es_HN.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets es_MX.mar)
 (deps    (:x es_MX.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets es_NI.mar)
 (deps    (:x es_NI.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets es_PA.mar)
 (deps    (:x es_PA.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets es_PE.mar)
 (deps    (:x es_PE.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets es_PR.mar)
 (deps    (:x es_PR.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets es_PY.mar)
 (deps    (:x es_PY.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets es_SV.mar)
 (deps    (:x es_SV.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets es_US.mar)
 (deps    (:x es_US.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets es_UY.mar)
 (deps    (:x es_UY.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets es_VE.mar)
 (deps    (:x es_VE.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets es__TRADITIONAL.mar)
 (deps    (:x es__TRADITIONAL.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets et.mar)
 (deps    (:x et.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets et_EE.mar)
 (deps    (:x et_EE.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets eu.mar)
 (deps    (:x eu.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets eu_ES.mar)
 (deps    (:x eu_ES.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets eu_ES_PREEURO.mar)
 (deps    (:x eu_ES_PREEURO.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets fa.mar)
 (deps    (:x fa.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets fa_IN.mar)
 (deps    (:x fa_IN.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets fa_IR.mar)
 (deps    (:x fa_IR.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets fi.mar)
 (deps    (:x fi.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets fi_FI.mar)
 (deps    (:x fi_FI.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets fi_FI_PREEURO.mar)
 (deps    (:x fi_FI_PREEURO.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets fo.mar)
 (deps    (:x fo.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets fo_FO.mar)
 (deps    (:x fo_FO.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets fr.mar)
 (deps    (:x fr.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets fr_BE.mar)
 (deps    (:x fr_BE.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets fr_BE_PREEURO.mar)
 (deps    (:x fr_BE_PREEURO.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets fr_CA.mar)
 (deps    (:x fr_CA.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets fr_CH.mar)
 (deps    (:x fr_CH.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets fr_FR.mar)
 (deps    (:x fr_FR.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets fr_FR_PREEURO.mar)
 (deps    (:x fr_FR_PREEURO.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets fr_LU.mar)
 (deps    (:x fr_LU.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets fr_LU_PREEURO.mar)
 (deps    (:x fr_LU_PREEURO.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets fr_MC.mar)
 (deps    (:x fr_MC.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ga.mar)
 (deps    (:x ga.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ga_IE.mar)
 (deps    (:x ga_IE.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ga_IE_PREEURO.mar)
 (deps    (:x ga_IE_PREEURO.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets gl.mar)
 (deps    (:x gl.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets gl_ES.mar)
 (deps    (:x gl_ES.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets gl_ES_PREEURO.mar)
 (deps    (:x gl_ES_PREEURO.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets gu.mar)
 (deps    (:x gu.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets gu_IN.mar)
 (deps    (:x gu_IN.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets gv.mar)
 (deps    (:x gv.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets gv_GB.mar)
 (deps    (:x gv_GB.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets he.mar)
 (deps    (:x he.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets he_IL.mar)
 (deps    (:x he_IL.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets hi.mar)
 (deps    (:x hi.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets hi_IN.mar)
 (deps    (:x hi_IN.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets hi__DIRECT.mar)
 (deps    (:x hi__DIRECT.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets hr.mar)
 (deps    (:x hr.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets hr_HR.mar)
 (deps    (:x hr_HR.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets hu.mar)
 (deps    (:x hu.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets hu_HU.mar)
 (deps    (:x hu_HU.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets hy.mar)
 (deps    (:x hy.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets hy_AM.mar)
 (deps    (:x hy_AM.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets hy_AM_REVISED.mar)
 (deps    (:x hy_AM_REVISED.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets id.mar)
 (deps    (:x id.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets id_ID.mar)
 (deps    (:x id_ID.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets index.mar)
 (deps    (:x index.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets is.mar)
 (deps    (:x is.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets is_IS.mar)
 (deps    (:x is_IS.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets it.mar)
 (deps    (:x it.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets it_CH.mar)
 (deps    (:x it_CH.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets it_IT.mar)
 (deps    (:x it_IT.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets it_IT_PREEURO.mar)
 (deps    (:x it_IT_PREEURO.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets iw.mar)
 (deps    (:x iw.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets iw_IL.mar)
 (deps    (:x iw_IL.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ja.mar)
 (deps    (:x ja.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ja_JP.mar)
 (deps    (:x ja_JP.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ka.mar)
 (deps    (:x ka.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ka_GE.mar)
 (deps    (:x ka_GE.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets kk.mar)
 (deps    (:x kk.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets kk_KZ.mar)
 (deps    (:x kk_KZ.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets kl.mar)
 (deps    (:x kl.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets kl_GL.mar)
 (deps    (:x kl_GL.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets kn.mar)
 (deps    (:x kn.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets kn_IN.mar)
 (deps    (:x kn_IN.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ko.mar)
 (deps    (:x ko.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ko_KR.mar)
 (deps    (:x ko_KR.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets kok.mar)
 (deps    (:x kok.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets kok_IN.mar)
 (deps    (:x kok_IN.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets kw.mar)
 (deps    (:x kw.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets kw_GB.mar)
 (deps    (:x kw_GB.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ky.mar)
 (deps    (:x ky.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ky_KG.mar)
 (deps    (:x ky_KG.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets lt.mar)
 (deps    (:x lt.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets lt_LT.mar)
 (deps    (:x lt_LT.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets lv.mar)
 (deps    (:x lv.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets lv_LV.mar)
 (deps    (:x lv_LV.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets mk.mar)
 (deps    (:x mk.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets mk_MK.mar)
 (deps    (:x mk_MK.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets mn.mar)
 (deps    (:x mn.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets mn_MN.mar)
 (deps    (:x mn_MN.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets mr.mar)
 (deps    (:x mr.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets mr_IN.mar)
 (deps    (:x mr_IN.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ms.mar)
 (deps    (:x ms.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ms_BN.mar)
 (deps    (:x ms_BN.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ms_MY.mar)
 (deps    (:x ms_MY.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets mt.mar)
 (deps    (:x mt.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets mt_MT.mar)
 (deps    (:x mt_MT.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets nb.mar)
 (deps    (:x nb.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets nb_NO.mar)
 (deps    (:x nb_NO.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets nl.mar)
 (deps    (:x nl.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets nl_BE.mar)
 (deps    (:x nl_BE.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets nl_BE_PREEURO.mar)
 (deps    (:x nl_BE_PREEURO.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets nl_NL.mar)
 (deps    (:x nl_NL.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets nl_NL_PREEURO.mar)
 (deps    (:x nl_NL_PREEURO.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets nn.mar)
 (deps    (:x nn.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets nn_NO.mar)
 (deps    (:x nn_NO.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets no.mar)
 (deps    (:x no.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets no_NO.mar)
 (deps    (:x no_NO.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets no_NO_NY.mar)
 (deps    (:x no_NO_NY.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets om.mar)
 (deps    (:x om.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets om_ET.mar)
 (deps    (:x om_ET.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets om_KE.mar)
 (deps    (:x om_KE.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets pa.mar)
 (deps    (:x pa.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets pa_IN.mar)
 (deps    (:x pa_IN.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets pl.mar)
 (deps    (:x pl.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets pl_PL.mar)
 (deps    (:x pl_PL.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets pt.mar)
 (deps    (:x pt.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets pt_BR.mar)
 (deps    (:x pt_BR.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets pt_PT.mar)
 (deps    (:x pt_PT.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets pt_PT_PREEURO.mar)
 (deps    (:x pt_PT_PREEURO.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ro.mar)
 (deps    (:x ro.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ro_RO.mar)
 (deps    (:x ro_RO.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets root.mar)
 (deps    (:x root.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ru.mar)
 (deps    (:x ru.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ru_RU.mar)
 (deps    (:x ru_RU.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ru_UA.mar)
 (deps    (:x ru_UA.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets sa.mar)
 (deps    (:x sa.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets sa_IN.mar)
 (deps    (:x sa_IN.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets sh.mar)
 (deps    (:x sh.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets sh_YU.mar)
 (deps    (:x sh_YU.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets sk.mar)
 (deps    (:x sk.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets sk_SK.mar)
 (deps    (:x sk_SK.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets sl.mar)
 (deps    (:x sl.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets sl_SI.mar)
 (deps    (:x sl_SI.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets so.mar)
 (deps    (:x so.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets so_DJ.mar)
 (deps    (:x so_DJ.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets so_ET.mar)
 (deps    (:x so_ET.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets so_KE.mar)
 (deps    (:x so_KE.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets so_SO.mar)
 (deps    (:x so_SO.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets sq.mar)
 (deps    (:x sq.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets sq_AL.mar)
 (deps    (:x sq_AL.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets sr.mar)
 (deps    (:x sr.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets sr_YU.mar)
 (deps    (:x sr_YU.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets sv.mar)
 (deps    (:x sv.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets sv_FI.mar)
 (deps    (:x sv_FI.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets sv_SE.mar)
 (deps    (:x sv_SE.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets sw.mar)
 (deps    (:x sw.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets sw_KE.mar)
 (deps    (:x sw_KE.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets sw_TZ.mar)
 (deps    (:x sw_TZ.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets syr.mar)
 (deps    (:x syr.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets syr_SY.mar)
 (deps    (:x syr_SY.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ta.mar)
 (deps    (:x ta.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ta_IN.mar)
 (deps    (:x ta_IN.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets te.mar)
 (deps    (:x te.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets te_IN.mar)
 (deps    (:x te_IN.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets test_1.mar)
 (deps    (:x test_1.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets th.mar)
 (deps    (:x th.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets th_TH.mar)
 (deps    (:x th_TH.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ti.mar)
 (deps    (:x ti.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ti_ER.mar)
 (deps    (:x ti_ER.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ti_ET.mar)
 (deps    (:x ti_ET.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets tr.mar)
 (deps    (:x tr.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets tr_TR.mar)
 (deps    (:x tr_TR.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets tt.mar)
 (deps    (:x tt.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets tt_RU.mar)
 (deps    (:x tt_RU.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets uk.mar)
 (deps    (:x uk.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets uk_UA.mar)
 (deps    (:x uk_UA.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ur.mar)
 (deps    (:x ur.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets ur_PK.mar)
 (deps    (:x ur_PK.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets uz.mar)
 (deps    (:x uz.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets uz_UZ.mar)
 (deps    (:x uz_UZ.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets vi.mar)
 (deps    (:x vi.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets vi_VN.mar)
 (deps    (:x vi_VN.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets zh.mar)
 (deps    (:x zh.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets zh_CN.mar)
 (deps    (:x zh_CN.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets zh_HK.mar)
 (deps    (:x zh_HK.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets zh_MO.mar)
 (deps    (:x zh_MO.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets zh_SG.mar)
 (deps    (:x zh_SG.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets zh_TW.mar)
 (deps    (:x zh_TW.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets zh_TW_STROKE.mar)
 (deps    (:x zh_TW_STROKE.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))


(rule
 (targets zh__PINYIN.mar)
 (deps    (:x zh__PINYIN.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %{x} .)))

