#' Shanghai stroke prediction and save service system
#' transfer data from shanghai fifth hospital to huashan
#' @param ... one or more xlsx file names
#'
#' @return write out csv file
#' @export
#'
#' @examples
#' \donttest{
#' stroke_prediction_save_service('1.xlsx','2.xlsx')
#' }
stroke_prediction_save_service <- function(...){
    xlsx <- c(...)
    for (i in xlsx) {
        spss(i)
    }
}

spss <- function(xlsx){
    library(readxl)
    library(set)
    df=as.data.frame(readxl::read_excel(xlsx))
    df <- df[,nchar(colnames(df)) >0]
    df7 <- df[as.Date(df[,"I_DATE"]) - as.Date(df[,"T_LASTWEL"]) < 7,]

    for (i in 1:ncol(df7)) {

        if (all(is.na(df7[,i]))) next(i)

        df7[!is.na(df7[,i]), i] <- do::Trim(df7[!is.na(df7[,i]), i])

        df7[!is.na(df7[,i]), i] <- do::Replace0(df7[!is.na(df7[,i]), i],c('g/L','mg'))
        if (is.character(df7[!is.na(df7[,i]), i])){
            df7[!is.na(df7[,i]), i][df7[!is.na(df7[,i]), i]=='未查'] =NA
            df7[!is.na(df7[,i]), i][df7[!is.na(df7[,i]), i]=='无法测量'] =NA
        }
        df7[!is.na(df7[,i]), i][nchar(df7[!is.na(df7[,i]), i])==0] =NA
    }




    # x <- do::Replace0(x,'\r')
    # x1=do::col_split(x[1],'\t',colnames = x2[1,])

    colnames(df7)[ toupper(colnames(df7)) == 'PAT_OUT_CODE'] ='PTID'
    colnames(df7)[ toupper(colnames(df7)) == 'NAME'] ='name'
    colnames(df7)[ toupper(colnames(df7)) == 'I_BP_SYSTOLIC'] ='I_BP_Systolic'
    colnames(df7)[ toupper(colnames(df7)) == 'I_BP_DIASTOLIC'] ='I_BP_diastolic'
    colnames(df7)[ toupper(colnames(df7)) == 'I_HOLTER'] ='DM_AFSC_HOLTER'
    colnames(df7)[ toupper(colnames(df7)) == 'U_DATE'] ='D_DATE'
    df7 <- df7[,colnames(df7) %not% c('REL','INHDAY', 'REG4',
                                      'I48AC_O_TYPE', 'D_HI_SMK', 'D_HI_WT',
                                      'D_HI_LSLL', 'D_HI_DM','RYSJ','CYSJ')]
    for (i in 1:ncol(df7)) {
        df7[,i] = as.character(df7[,i])

    }

    english <- c("PTID", "D_DATE", "name", "B_DATE", "GENDER", "AGE", "IDNO",
                 "IMPDAT", "REEADD2", "PRTADD2", "TEL_1", "TEL_2", "ETHNIC", "MAR",
                 "JOB", "EDU", "COT", "COTNUM", "MINSUR_TP1", "MINSUR_TP2", "MINSUR_TP3",
                 "MINSUR_TP4", "MINSUR_TP5", "MINSUR_TP6", "MINSUR_TP7", "FAM_INCOME",
                 "DOCTYP", "A_SYMPLACE", "A_TRANS", "A_EMS_N1", "DEPT_VISIT1",
                 "T_LASTWEL", "T_SYPM", "A_DATE", "I_DATE", "DEPT_ADMISS", "TPA_WY",
                 "wyywzjl", "SJNKYSJZ", "YCT", "CTDATE", "CTSCOT", "NOCTREA",
                 "MANSUT", "ELMDATYW", "ELMDAT", "HELEXNYW", "HELEXN", "XXBJS",
                 "XXBJSDATE", "JZXT", "JZXTDATE", "H_DRINK", "H_SMK", "H_CI",
                 "H_CI_HBNS", "H_CI_BX", "H_TIA", "H_TIA_HBNS", "H_TIA_BX", "H_MI",
                 "H_MI_HBNS", "H_MI_BX", "H_HYPT_I", "H_HYPT_I_HBNS", "HBPDATBX",
                 "H_DIAB", "H_DIAB_HBNS", "GLSDATBX", "H_AFIB", "H_AFIB_HBNS",
                 "HEADIEDATBX", "H_OTHER_HD", "H_OTHER_HD_HBNS", "OTHHEADATBX",
                 "H_PVD", "H_PVD_HBNS", "H_PVD_BX", "H_LIPID_I", "H_LIPID_I_HBNS",
                 "xzycrqbx", "H_ICH", "H_ICH_HBNS", "H_ICH_BX", "H_SAH", "H_SAH_HBNS",
                 "H_SAH_BX", "H_CSTENO", "H_CSTENO_HBNS", "H_CSTENO_BX", "H_HF",
                 "H_HF_HBNS", "H_HF_BX", "H_VALVR", "H_VALVR_HBNS", "H_VALVR_BX",
                 "H_SCD", "H_SCD_HBNS", "H_SCD_BX", "H_CURPREG", "H_DEMEN", "H_DEMEN_HBNS",
                 "H_DEMEN_BX", "H_DYSPHR", "H_DYSPHR_HBNS", "H_DYSPHR_BX", "H_COPD",
                 "H_COPD_HBNS", "H_COPD_BX", "H_HEREF", "H_HEREF_HBNS", "H_HEREF_BX",
                 "H_BLEDHIS", "H_BLEDHIS_HBNS", "H_BLEDHIS_BX", "H_PMINF", "H_UNINF",
                 "FHX", "H_OTH", "HEALTHY", "H_HYPT", "STPDON1", "STPDON2", "STPDON3",
                 "STPDON4", "STPDON5", "STPDON6", "H_DM_O", "HYC1", "HYC2", "HYC3",
                 "HYC4", "HYC5", "HYC6", "H_LIPID", "LIP1", "LIP2", "LIP3", "LIP4",
                 "LIP5", "LIP6", "H_COAG", "ANT1", "ANT2", "ANT3", "ANT4", "KN_DDJQ",
                 "KN_AJQB", "KN_LFQB", "KN_LPQB", "ANT5", "H_PLT", "ANI1", "ANI2",
                 "ANI3", "ANI4", "ANI5", "ANI6", "H_JTXBGAS", "REG1", "REG2",
                 "REG3", "BN_YSP", "H_TRADIM", "A_DIAG", "SYM_TIA", "DURA_TIA",
                 "EARLYTIA_", "DWI", "TIA_IS", "TIA_IS_DATE", "D_TIAIS", "H_MRS",
                 "DO_A_NIHSS", "A_NIHSS", "DO_I_NIHSS", "I_NIHSS", "DO_GCS", "GCS",
                 "TPA", "TPA_T1", "TPA_T1_01", "TPA_DS", "TPA_WEIGHT", "TPA_R",
                 "SFSMJL", "TPA_CR_0", "TPA0_AC1", "TPA0_AC2", "TPA0_AC3", "TPA0_AC4",
                 "TPA0_AC5", "TPA0_AC6", "TPA0_AC7", "TPA0_AC8", "TPA0_AC9", "TPA0_AC99",
                 "TPA0_RC1", "TPA0_RC2", "TPA0_RC3", "TPA0_RC4", "TPA0_RC5", "TPA0_RC6",
                 "TPA0_RC7", "TPA0_RC8", "TPA0_RC9", "TPA0_RC10", "TPA0_RC11",
                 "TPA0_RC12", "TPA0_RC99", "TPA_CR_3", "TPA3_AC1", "TPA3_AC2",
                 "TPA3_AC3", "TPA3_AC4", "TPA3_AC5", "TPA3_AC6", "TPA3_AC7", "TPA3_AC8",
                 "TPA3_AC9", "TPA3_AC99", "TPA3_RC1", "TPA3_RC2", "TPA3_RC3",
                 "TPA3_RC4", "TPA3_RC5", "TPA3_RC6", "TPA3_RC7", "TPA3_RC8", "TPA3_RC9",
                 "TPA3_RC10", "TPA3_RC11", "TPA3_RC12", "TPA3_RC99", "TPA3_ERC1",
                 "TPA3_ERC2", "TPA3_ERC3", "TPA3_ERC4", "TPA3_ERC99", "JZ", "JZCT",
                 "ISHIS", "IU", "IUTXT", "IA_CA", "IA_CA_T", "IA_CA_AG", "T_COM",
                 "T_COM0", "IF_QS", "QS_TIME", "IRR", "KUD", "ywbx", "HETNOE",
                 "RTH", "VELNOE", "VELPAT", "I_LDL", "I_HCY", "I_HGB", "I_FG",
                 "I_CR", "I_BUN", "I_UA", "I_INR", "I_FIB", "I_PLT", "I_HEIGHT",
                 "I_WEIGHT", "I_BP_Systolic", "I_BP_diastolic", "I_PULSE", "I_CDOPP",
                 "I_BVESSLE", "I_TCD", "I_CTA", "I_MRA", "I_CEMRA", "I_DSA", "REHABI",
                 "D_TCOST", "D_MCOST", "D_DEATH", "D_DTH_T", "D_DTH_R", "D_DSP",
                 "D_DC_MA", "D_DC_RS", "D_DIAG", "ADNTYPEXP", "LEATYPEXP", "D_D_HP",
                 "D_D_DM", "D_D_LD", "D_D_AF", "D_D_CI", "D_D_CIICH", "D_D_ICH",
                 "D_D_SAH", "D_D_TIA", "D_D_HF", "D_D_SCD", "D_D_MI", "D_D_HD_OTH",
                 "D_D_CS", "D_D_CSTENDING", "D_D_PVD", "D_D_CFA", "D_D_OTHER",
                 "DYS_PNEUM", "A_SFM", "DYS_RESULT", "DYS_REHA", "I48_WK", "DVT_C",
                 "DVT_C1", "DVT_C2", "DVT_C3", "DVT_C4", "I48_DV", "I48_DV_T",
                 "I48_DV1", "I48_DV2", "I48_DV3", "I48_DV4", "I48_DV5", "I48_DV6",
                 "I48_DV7", "I48_DV8", "I48_DV9", "I48_DV10", "DVT_DOPPLER", "DVT_FOUND",
                 "DVT_P_EMBOSM", "I_MI", "I_NGIH", "I_CI", "I_CH", "I_TIA", "I_NSZR",
                 "I_NHC", "I_AFIB", "I_NUTI", "I_NCA", "I_NDUC", "I_NDEP", "I48_ATC",
                 "I48_ATC0", "I48_AT", "I48AP_ASP", "I48AP_CLP", "I48AP_AZGL",
                 "I48AP_SMDM", "I48AP_PND", "I48AP_XLTZ", "I48AP_O_TP", "I48AC_LH",
                 "I48AC_H", "I48AC_WFR", "I48AC_ZJNXM", "I48AC_XAYZ", "I48AC_APSB",
                 "I48AC_YDSB", "CTTIME", "CTNO", "CT0", "CT1", "CT2", "CT3", "CT4",
                 "CT5", "CT6", "MRTIME", "MRNO", "MR0", "MR1", "MR2", "MR3", "MR4",
                 "MR5", "MR6", "NIHSSVAL", "NIHSSNO", "SICHYES", "SICHNO", "SICKNO",
                 "D_APC_C", "D_APC_C1", "D_APC_C2", "D_APC_C3", "D_APC_C4", "D_APC_C5",
                 "D_APC_C6", "D_APC_C7", "D_APC", "D_AP_ASP", "D_AP_CLP", "D_AP_AZGL",
                 "D_AP_SMDM", "D_AP_PND", "D_AP_XLTZ", "D_AP_O_TP", "D_AC_LH",
                 "D_AC_H", "D_AC_WFR", "D_AC_ZJNXM", "D_AC_XAYZ", "D_AC_APSB",
                 "D_AC_YDSB", "NOKNVAL", "DM_HP1", "D_HP_C", "D_HP_C0", "DM_HP2",
                 "D_HP_ACEI", "D_HP_BRT", "D_HP_ARB", "D_HP_CCT", "D_HP_DIU",
                 "FHZJ", "D_HP_OTHE", "D_LDL100", "D_STA", "D_LDLNO", "D_NOTALL",
                 "D_STT_C", "D_STT_C_0", "D_STT", "D_LD", "D_L_C", "D_L_C0", "D_LL",
                 "D_LL_STT", "D_LL_NTA", "D_LL_FBR", "D_LL_ABSORB", "D_LL_O",
                 "DM_DM", "D_HG_C", "D_HG_C0", "D_HG", "D_HG_INS", "D_HG_SUL",
                 "D_HG_BGN", "D_HG_GSI", "D_HG_IAE", "D_HG_NONSUL", "D_HG_O",
                 "DM_AFSC_H", "DM_AFSC_ECG", "DM_AFSC_HOLTER", "DM_AFSC_ECGM",
                 "DM_AFSC_NA", "DM_AF", "D_FAC_C", "D_FAC_C1", "D_FAC_C2", "D_FAC_C3",
                 "D_FAC_C4", "D_FAC_C5", "D_FAC_C6", "D_FAC_C7", "D_FAC", "D_FAC_H",
                 "D_FAC_LH", "D_FAC_WFR", "D_FAC_ZJNXM", "D_FAC_XAINHI", "D_FAC_APSB",
                 "D_FAC_YDSB", "D_FAC_O", "D_HI_GZ1", "D_HI_GZ2", "D_HI_GZ3",
                 "D_HI_GZ4", "D_HI_GZ5", "LEABLD", "LEABLD02", "LEAPRN", "NIHSSNO2",
                 "MRSVAL")

    chinese <- c("病例号", "出院日期", "姓名", "出生日期", "性别", "年龄", "证件号码",
                 "发病日期", "户籍地址", "现居住地址", "联系电话1", "联系电话2",
                 "民族", "婚姻状况", "职业", "受教育程度", "联系人", "联系人电话",
                 "城镇职工医疗保险", "商业保险", "公费医疗", "城镇居民医保", "农村合作医疗",
                 "自费", "其他", "家庭人均月收入", "入院方式", "患者出现卒中症状时的场所",
                 "患者由何种途径转运至本医院", "急救系统是否提前通知医院", "患者到院首诊科室",
                 "最后看起来正常时间", "症状被发现的时间", "患者到院时间", "患者住院时间",
                 "患者住院科室", "是否外院静脉溶栓/再灌注", "药物总剂量", "神经内科医师接诊时间",
                 "就诊时是否行头部CT检查", "完成头颅CT扫描时间", "头部CT检查结果",
                 "未查原因", "主诉", "首次急诊心电图检查", "首次急诊心电图检查时间",
                 "首次急诊凝血功能检查报告", "首次急诊血液学检查时间", "首次急诊血小板计数",
                 "首次急诊血小板计数时间", "首次急诊血糖", "首次急诊血糖时间",
                 "酗酒", "吸烟", "脑梗死", "脑梗死患病年数", "脑梗死不详", "短暂性脑缺血发作",
                 "短暂性脑缺血发作患病年数", "短暂性脑缺血发作不详", "心肌梗死",
                 "心肌梗死患病年数", "心肌梗死不详", "高血压", "高血压患病年数",
                 "高血压年份不详", "糖尿病", "糖尿病患病年数", "糖尿病年份不详",
                 "房颤或瓣膜性心脏病", "房颤或瓣膜性心脏病患病年数", "房颤年份不详",
                 "其他心脏病（除外房颤和心肌梗死，包括心绞痛、风湿性、高血压性等）",
                 "其他心脏病患病年数", "其他心脏病年份不详", "周围血管病", "周围血管病患病年数",
                 "周围血管病不详", "脂代谢紊乱", "脂代谢紊乱患病年数", "脂代谢紊乱年份不详",
                 "脑出血", "脑出血患病年数", "脑出血不详", "蛛网膜下腔出血", "蛛网膜下腔出血患病年数",
                 "蛛网膜下腔出血不详", "颈动脉狭窄", "颈动脉狭窄患病年数", "颈动脉狭窄不详",
                 "心力衰竭", "心力衰竭患病年数", "心力衰竭不详", "心脏瓣膜置换术",
                 "心脏瓣膜置换术患病年数", "心脏瓣膜置换术不详", "镰状细胞病",
                 "镰状细胞病患病年数", "镰状细胞病不详", "妊娠或产后6周", "痴呆",
                 "痴呆患病年数", "痴呆不详", "精神障碍", "精神障碍患病年数", "精神障碍不详",
                 "慢性阻塞性肺疾病（COPD）", "慢性阻塞性肺疾病患病年数", "慢性阻塞性肺疾病不详",
                 "肝肾功能不全", "肝肾功能不全患病年数", "肝肾功能不全不详", "出血史或者出血倾向",
                 "出血史或者出血倾向患病年数", "出血史或者出血倾向不详", "入院前两周肺部感染",
                 "入院前两周泌尿系统感染", "脑卒中家族史", "其他", "既往体健",
                 "降压", "降压_利尿药", "降压_钙拮抗剂", "降压_受体阻滞剂", "降压_ACEI",
                 "降压_ARB", "降压_其他", "降糖", "降糖_利磺脲类", "降糖_双胍类",
                 "降糖_噻唑烷二酮类", "降糖_α糖苷酶抑制剂", "降糖_胰岛素", "降糖_其他",
                 "降脂", "降脂_他汀类", "降脂_贝特类", "降脂_烟酸类", "降脂_树脂类",
                 "降脂_胆固醇吸收抑制剂", "降脂_其他", "抗凝", "抗凝_华法林",
                 "抗凝_普通肝素", "抗凝_低分子肝素", "抗凝_水蛭素", "达比加群",
                 "阿加曲班", "利伐沙班", "利派沙班", "抗凝_其他", "抗血小板",
                 "抗血小板_阿司匹林", "抗血小板_氯吡格雷", "抗血小板_噻氯匹定",
                 "抗血小板_双嘧达莫", "抗血小板_西洛他唑", "抗血小板_其他", "降同型半胱氨酸",
                 "降同型半胱氨酸_叶酸", "降同型半胱氨酸_Vitb12", "降同型半胱氨酸_Vitb6",
                 "马来依那普利叶酸片", "中成药物", "初步诊断结果", "临床症状",
                 "症状持续时间", "本次发作前7天内是否有1次早期发作", "头MR的DWI表现",
                 "患者住院期间是否发生脑梗死", "若“是”，具体发病时间", "脑梗死距首发症状的时间",
                 "发病前mRS评分事务 4.中度残疾，要求一些帮助，但行走不需帮助 5.重度残疾，不能独立行走，无他人帮助不能满足自身需求 6.严重残疾，卧床、失禁，要求持续护理和关注）",
                 "就诊时是否行NIHSS评分", "若“是”就诊时NIHSS评分分值", "入院24小时内是否NIHSS评分",
                 "若“是”NIHSS评分分值（0-42）", "就诊时是否行格拉斯哥评分",
                 "若“是”就诊时格拉斯哥评分分值", "是否本院行rtPA静脉溶栓治疗提示",
                 "溶栓开始时间", "不详", "给药总量", "体重", "若“否”原因详）",
                 "是否书面记录", "0-3小时时间窗内静脉溶栓的禁忌症或相对禁忌症",
                 "收缩压 大于 185 或 舒张压大于110 mmHg（尽管降压治疗）", "卒中发作时出现痫性发作",
                 "近期手术/外伤", "近日颅内或脊髓手术, 头部外伤, 或卒中 (小于3 月) ",
                 "既往有颅内出血或动脉瘤或血管畸形或脑肿瘤病史", "活动性内脏出血(小于22 天)",
                 "血小板 小于100,000, aPTT大于 40 秒 使用肝素后,或PT大于 15或 INR大于 1.7, 或已知出血性体质",
                 "怀疑蛛网膜下腔出血", "CT发现(ICH, SAH, 或大面积梗死征象)", "无（绝对禁忌症）",
                 "严重卒中（如NIHSS大于22）", "血糖 小于 50 or 大于400 mg/dl",
                 "左心附壁血栓", "由于合并症增加出血风险", "妊娠", "年龄大于80岁",
                 "预期寿命 小于 1年或者严重的合并症", "患者/家属 拒绝", "卒中小组不能确定患者是否符合溶栓条件",
                 "在院外静脉或动脉给予溶栓药物", "症状过于轻微", "症状迅速改善",
                 "无（相对禁忌症）", "3-4.5小时时间窗内静脉溶栓的禁忌症或相对禁忌症",
                 "收缩压 大于 185 或 舒张压大于110 mmHg（尽管降压治疗）", "卒中发作时出现痫性发作",
                 "近期手术/外伤 （小于15 天）", "近日颅内或脊髓手术, 头部外伤, 或卒中 （小于3 月）",
                 "既往有颅内出血或动脉瘤或血管畸形或脑肿瘤病史", "活动性内脏出血（小于22 天）",
                 "血小板小于100,000, PTT大于40 秒 使用肝素后,或PT大于15或 INR大于 1.7, 或已知出血性体质",
                 "怀疑蛛网膜下腔出血", "CT发现（ICH, SAH, 或大面积梗死征象）",
                 "无（3-4.5小时静脉溶栓绝对禁忌）", "严重卒中（如NIHSS大于22）",
                 "血糖小于 50 or 大于400 mg/dl ", "左心附壁血栓", "由于合并症增加出血风险",
                 "妊娠", "年龄大于80岁", "预期寿命小于1年或者严重的合并症", "患者/家属 拒绝",
                 "卒中小组不能确定患者是否符合溶栓条件", "在院外静脉或动脉给予溶栓药物",
                 "症状过于轻微", "症状迅速改善", "无（3-4.5小时静脉溶栓相对禁忌）",
                 "既往卒中和糖尿病", "就诊前使用抗凝药物（即使INR 小于 1.7）",
                 "NIHSS 大于25", "CT 提示梗死面积大于1/3 MCA供血区", "无（3-4.5小时静脉溶栓额外的额外禁忌）",
                 "（启动静脉溶栓的场所）急诊", "急诊影像科室", "神内病房", "其他",
                 "若其他，具体", "是否在本院行动脉导管再灌注治疗", "时间为", "使用药物",
                 "是否出现溶栓治疗并发症", "并发症类型", "是否取栓", "取栓时间",
                 "脉律", "腰围", "腰围不详", "心脏杂音", "心律", "颈部血管杂音",
                 "颈部血管杂音部位", "低密度脂蛋白", "同型半胱氨酸", "糖化血红蛋白",
                 "空腹血糖", "血清肌酐", "血清尿素氮", "尿酸", "INR", "纤维蛋白原FIB",
                 "PLT", "身高", "体重", "收缩压", "舒张压", "脉搏", "是否进行颈部血管超声检查",
                 "是否进行脑血管相关检查", "TCD", "CTA", "MRA", "CEMRA", "DSA",
                 "住院期间患者是否接受康复评价和/或康复锻炼", "住院总费用", "药物总费用",
                 "住院期间是否死亡", "死亡日期", "死亡原因", "出院去向", "是否医嘱出院",
                 "若“否”，原因", "最终诊断", "OCSP", "TOAST", "高血压", "糖尿病",
                 "脂代谢紊乱", "房颤", "脑梗死", "脑梗死后出血", "脑出血", "蛛网膜下腔出血",
                 "TIA", "心力衰竭", "镰状细胞病", "心肌梗死", "其他心脏病", "颈动脉狭窄",
                 "颈动脉支架置入术", "周围血管病", "凝血异常", "其他", "是否合并肺炎",
                 "在经口进食、水、口服药物之前是否进行吞咽功能评价", "若“是”吞咽功能评价结果",
                 "若“困难”是否行吞咽功能康复", "入院48小时内患者是否能行走",
                 "是否存在DVT预防的禁忌症", "抗凝禁忌症", "血栓泵禁忌症", "弹力袜禁忌症",
                 "患者或家属拒绝", "若“否”是否开展DVT预防", "若“是”日期",
                 "普通肝素", "低分子肝素", "华法林", "血栓泵", "弹力袜", "早期活动",
                 "达比加群", "利伐沙班", "阿哌沙班", "依度沙班", "是否行下肢静脉超声检查",
                 "是否合并深静脉血栓", "是否合并肺栓塞", "是否心肌梗死", "是否消化道出血",
                 "是否再发脑梗死", "是否发生或再发脑出血", "是否短暂性脑缺血发作",
                 "是否痫性发作", "是否脑积水", "是否房颤", "是否泌尿系感染", "是否心跳或呼吸停止",
                 "是否褥疮", "是否抑郁", "是否存在抗栓治疗禁忌症", "若“是”，存在的抗栓治疗禁忌症类型态/仅临床关怀）",
                 "若“否”是否给予抗栓药物治疗", "阿司匹林", "氯吡格雷", "奥扎格雷",
                 "双嘧达莫", "噻氯吡啶", "西洛他唑", "其他", "低分子肝素", "普通肝素",
                 "华法林", "直接凝血酶抑制剂", "Xa因子抑制剂", "阿哌沙班",
                 "依度沙班", "溶栓后24小时头颅CT时间", "CT未查", "无出血",
                 "沿梗死区边缘小的出血点、斑", "沿梗死区内融合的出血点、斑，但无占位效应",
                 "梗死灶内血肿小于等于30%个脑梗死区，有轻度占位效应", "血肿大于30%梗死区，有显著占位效应",
                 "远离梗死区的小至中等血肿；可有轻度占位效应", "远离梗死区的大的融合血肿；有明显占位效应",
                 "溶栓后头颅MR时间", "MR未查", "无出血", "沿梗死区边缘小的出血点、斑",
                 "沿梗死区内融合的出血点、斑，但无占位效应", "梗死灶内血肿小于等于30%个脑梗死区，有轻度占位效应",
                 "血肿大于30%梗死区，有显著占位效应", "远离梗死区的小至中等血肿；可有轻度占位效应",
                 "远离梗死区的大的融合血肿；有明显占位效应", "溶栓后24小时NIHSS评分分值",
                 "溶栓后24小时NIHSS未查", "溶栓后36h内发生症状性脑出血", "溶栓后36h内发生症状性脑出血（否）",
                 "溶栓后36h内发生症状性脑出血（不详）", "是否存在抗栓禁忌", "过敏或合并不能使用的疾病",
                 "患者/家属拒绝", "药物严重副作用", "出血风险或因出血而不能继续使用",
                 "疾病终末状态/仅临床关怀", "跌倒风险", "精神障碍不能配合", "是否给予抗栓药物治疗",
                 "阿司匹林", "氯吡格雷", "奥扎格雷", "双嘧达莫", "噻氯吡啶", "西洛他唑",
                 "其他", "低分子肝素", "普通肝素", "法华林", "达比加群", "利伐沙班",
                 "阿哌沙班", "依度沙班", "若否，原因", "是否有高血压", "是否存在降压治疗禁忌症",
                 "若“是“选择原因", "若“否“是否给予降压药物治疗", "ACEI", "贝塔受体阻滞剂",
                 "ARB", "钙通道阻滞剂", "利尿剂", "复合制剂", "其他", "LDL大于等于100mg/Dl",
                 "住院前规律服用他汀类药物", "LDL值未记录", "没有上述情况", "是否存在他汀类药物治疗禁忌症",
                 "若:”是”选择原因", "若:”否”选择:是否给予他汀药物治疗", "是否有脂质代谢紊乱",
                 "是否存在调脂治疗禁忌症", "若“是“选择原因", "若”否“是否给予调脂药物治疗",
                 "他汀类", "烟酸及其衍生物", "贝特类", "吸收型抑制剂", "其他",
                 "是否有糖尿病", "是否存在降糖治疗禁忌症", "若“是“选择原因",
                 "若“否”是否给予降糖药物治疗", "胰岛素（静脉或皮下）", "磺酰脲类",
                 "双胍类", "阿尔法糖苷酶抑制剂", "胰岛素增敏剂", "非磺酰脲类促胰岛素分泌剂",
                 "其他", "询问病史", "常规心电图", "24小时Holter", "大于24小时心电记录",
                 "未作任何检查", "是否有心房颤动/扑动", "是否存在抗凝治疗禁忌症",
                 "过敏或合并不能使用的疾病", "跌倒风险", "精神障碍不能配合", "家属/患者拒绝",
                 "药物严重副作用", "出血风险或因出血而不能继续使用", "疾病终末状态/仅临床关怀",
                 "若“否”是否给予抗凝药物", "普通肝素", "低分子肝素", "华法林",
                 "达比加群", "利伐沙班", "阿哌沙班", "依度沙班", "其他", "告知患者积极控制卒中危险因素",
                 "告知患者如何识别卒中发作症状", "告知患者卒中再发后要快速拨打“120”或“999”电话救助*",
                 "告知患者如何服用二级预防用药以及服药原因", "告知患者应定期复查",
                 "出院时血压（收缩压）", "出院时血压（舒张压）", "出院时低密度脂蛋白",
                 "出院时NIHSS评分分值", "MRS评分")

    x <- data.frame(t(chinese))
    colnames(x) <- english


    x2 <- plyr::rbind.fill(x,df7)
    x2[,"D_DATE"] <- do::Replace0(do::Trim(x2[,"D_DATE"],' '),' .*')
    x2[,"B_DATE"] <- do::Replace0(do::Trim(x2[,"B_DATE"],' '),' .*')
    x2[,"GENDER"]<- do::Replace(do::Replace0(do::Trim(x2[,"GENDER"],' '),' .*'),
                                pattern = c('男:1','女:2'))
    x2[,"IMPDAT"] <- do::Replace0(do::Trim(x2[,"IMPDAT"],' '),' .*')



    x2[,"MAR"] <- do::Trim(x2[,"MAR"])
    x2[-1,"MAR"] <- ifelse(x2[-1,"MAR"] =='未婚',1,
                           ifelse(x2[-1,"MAR"] =='已婚',2,
                                  ifelse(x2[-1,"MAR"] =='丧偶',3,
                                         ifelse(x2[-1,"MAR"] =='离婚',4,
                                                ifelse(is.na(x2[-1,"MAR"]),NA,5)))))

    x2[-1,"JOB"] <- '99'
    x2[-1,"EDU"] <- '99'
    x2[-1,"FAM_INCOME"] <- '99'



    x2[-1,"DOCTYP"] <- ifelse(x2[-1,"DOCTYP"] == '门诊',1,
                              ifelse(x2[-1,"DOCTYP"] == '急诊',2,
                                     ifelse(x2[-1,"DOCTYP"] == '转院',3,
                                            ifelse(is.na(x2[-1,"DOCTYP"]),NA,4))))

    x2[-1,"A_SYMPLACE"] <- '1'

    x2[-1,"DEPT_VISIT1"] <- ifelse(x2[-1,"DEPT_VISIT1"] %in% c('急诊内科','急诊神经内科','急诊'),2,
                                   ifelse(x2[-1,"DEPT_VISIT1"] == '神经内科',4,
                                          ifelse(is.na(x2[-1,"DEPT_VISIT1"]),NA,2)))

    x2[-1,"DEPT_ADMISS"] <- '1'




    to2 <- c("TPA_WY","H_CI","H_TIA","H_MI","H_HYPT_I","H_DIAB","H_AFIB","H_OTHER_HD","H_PVD","H_LIPID_I",
             "H_ICH","H_SAH","H_CSTENO","H_HF","H_VALVR","H_SCD","H_CURPREG",
             "H_DEMEN","H_DYSPHR","H_COPD","H_HEREF","H_BLEDHIS","H_PMINF","H_UNINF","H_OTH")
    for (i in to2) {
        x2[-1,i] <- ifelse(is.na(x2[-1,i]),'2',x2[-1,i])
    }


    to1 <- c("STPDON1", "STPDON2", "STPDON3", "STPDON4", "STPDON5", "STPDON6",
             "HYC1", "HYC2", "HYC3", "HYC4", "HYC5", "HYC6",
             "LIP1", "LIP2", "LIP3", "LIP4", "LIP5", "LIP6",
             "ANT1", "ANT2", "ANT3", "ANT4", "KN_DDJQ", "KN_AJQB", "KN_LFQB", "KN_LPQB",
             "ANT5", "H_PLT", "ANI1", "ANI2", "ANI3", "ANI4", "ANI5", "ANI6",
             "REG1", "REG2", "REG3", "BN_YSP")
    for (i in to1) {
        x2[-1,i] <- ifelse(is.na(x2[-1,i]),NA,'1')
    }


    x2[-1,"A_DIAG"] <- ifelse(x2[-1,"A_DIAG"] == '短暂性脑缺血发作','2','1')
    x2[-1,"I_NIHSS"] <- x2[-1,"A_NIHSS"]
    x2[-1,"DO_I_NIHSS"] <- x2[-1,"DO_A_NIHSS"]
    x2[-1,"TPA"] <- ifelse(x2[-1,"TPA"] == '0',2,1)
    x2[-1,"TPA_DS"] <- do::Replace0(x2[-1,"TPA_DS"],'mg')


    v1 <- c("TPA_CR_0", "TPA0_AC1", "TPA0_AC2", "TPA0_AC3", "TPA0_AC4",
            "TPA0_AC5", "TPA0_AC6", "TPA0_AC7", "TPA0_AC8", "TPA0_AC9", "TPA0_AC99",
            "TPA0_RC1", "TPA0_RC2", "TPA0_RC3", "TPA0_RC4", "TPA0_RC5", "TPA0_RC6",
            "TPA0_RC7", "TPA0_RC8", "TPA0_RC9", "TPA0_RC10", "TPA0_RC11",
            "TPA0_RC12", "TPA0_RC99", "TPA_CR_3", "TPA3_AC1", "TPA3_AC2",
            "TPA3_AC3", "TPA3_AC4", "TPA3_AC5", "TPA3_AC6", "TPA3_AC7", "TPA3_AC8",
            "TPA3_AC9", "TPA3_AC99", "TPA3_RC1", "TPA3_RC2", "TPA3_RC3",
            "TPA3_RC4", "TPA3_RC5", "TPA3_RC6", "TPA3_RC7", "TPA3_RC8", "TPA3_RC9",
            "TPA3_RC10", "TPA3_RC11", "TPA3_RC12", "TPA3_RC99", "TPA3_ERC1",
            "TPA3_ERC2", "TPA3_ERC3", "TPA3_ERC4", "TPA3_ERC99")

    for (i in v1) {
        x2[-1,i] <- ifelse(is.na(x2[-1,i]),NA,'1')
    }
    x2[-1,"IRR"] <- ifelse(x2[-1,"D_D_AF"]=='1','2','1')
    x2[-1,'IRR'][is.na(x2[-1,"D_D_AF"])] <- '1'
    x2[-1,"RTH"] <- ifelse(x2[-1,"D_D_AF"]=='1','2','1')
    x2[-1,'RTH'][is.na(x2[-1,"D_D_AF"])] <- '1'
    x2[-1,"HETNOE"] <- '2'
    x2[-1,"VELNOE"] <- '2'

    x2[-1,"I_FIB"] <- do::Replace0(x2[-1,"I_FIB"],'g/L')




    jdg <- c(FALSE,sapply(2:nrow(x2), function(i) any(!is.na(x2[i,c("I_TCD","I_CTA","I_MRA","I_CEMRA","I_DSA")]))))
    x2[jdg,"I_BVESSLE"] = '1'
    x2[-1,"REHABI"] <- '1'


    jdk <- sapply(1:nrow(x2),function(i) sum(is.na(x2[i,which(colnames(x2) =='I_LDL'):which(colnames(x2) =='I_PLT')]))<=2)
    x2 <- x2[jdk,]

    x2[is.na(x2[,"D_D_HP"]),"D_D_HP"] <- ifelse(grepl('高血压',x2[is.na(x2[,"D_D_HP"]),"D_DIAG"]),'1',NA)
    x2[is.na(x2[,"D_D_DM"]),"D_D_DM"] <- ifelse(grepl('糖尿病',x2[is.na(x2[,"D_D_DM"]),"D_DIAG"]),'1',NA)
    x2[is.na(x2[,"D_D_AF"]),"D_D_AF"] <- ifelse(grepl_or(x2[is.na(x2[,"D_D_AF"]),"D_DIAG"],
                                                         c('房颤','心房颤动')),'1',NA)

    x2[is.na(x2[,"D_D_CIICH"]),"D_D_CIICH"] <- ifelse(grepl_or(x2[is.na(x2[,"D_D_CIICH"]),"D_DIAG"],
                                                               c('出血性','出血转化')),'1',NA)



    x2[is.na(x2[,"D_D_CS"]),"D_D_CS"] <- ifelse(grepl('颈动脉狭窄',x2[is.na(x2[,"D_D_CS"]),"D_DIAG"]),'1',NA)
    x2[is.na(x2[,"D_D_PVD"]),"D_D_PVD"] <- ifelse(grepl('下肢动脉硬化',x2[is.na(x2[,"D_D_PVD"]),"D_DIAG"]),'1',NA)
    x2[is.na(x2[,"DYS_PNEUM"]),"DYS_PNEUM"] <- ifelse(grepl_or(x2[is.na(x2[,"DYS_PNEUM"]),"D_DIAG"],
                                                               c('肺炎','肺部感染')),'1',NA)
    x2[is.na(x2[,"DVT_P_EMBOSM"]),"DVT_P_EMBOSM"] <- ifelse(grepl('肺栓塞',x2[is.na(x2[,"DVT_P_EMBOSM"]),"D_DIAG"]),'1',NA)
    x2[is.na(x2[,"I_NGIH"]),"I_NGIH"] <- ifelse(grepl('消化道出血',x2[is.na(x2[,"I_NGIH"]),"D_DIAG"]),'1',NA)
    x2[is.na(x2[,"I_NSZR"]),"I_NSZR"] <- ifelse(grepl('癫痫',x2[is.na(x2[,"I_NSZR"]),"D_DIAG"]),'1',NA)
    x2[is.na(x2[,"I_NUTI"]),"I_NUTI"] <- ifelse(grepl_or(x2[is.na(x2[,"I_NUTI"]),"D_DIAG"],
                                                         c('尿路感染','泌尿系感染')),'1',NA)
    x2[-1,"I_AFIB"] <- x2[-1,"D_D_AF"]
    x2[is.na(x2[,"I_NDEP"]),"I_NDEP"] <- ifelse(grepl('抑郁',x2[is.na(x2[,"I_NDEP"]),"D_DIAG"]),'1',NA)

    x2[!is.na(x2[,"D_DIAG"]),"D_DIAG"] <- ifelse(grepl_or(x2[!is.na(x2[,"D_DIAG"]),"D_DIAG"],
                                                          c('短暂性脑缺血发作','短暂性完全性遗忘')),'2','1')


    x2[-1,"A_SFM"] <- '1'
    x2[-1,"DVT_DOPPLER"] <- '1'
    x2[-1,"DVT_FOUND"] <- '2'

    x2[is.na(x2[,"DVT_P_EMBOSM"]),"DVT_P_EMBOSM"] <- '2'


    na2 <- c("DVT_P_EMBOSM", "I_MI", "I_NGIH", "I_CI", "I_CH", "I_TIA",
             "I_NSZR", "I_NHC", "I_AFIB", "I_NUTI", "I_NCA", "I_NDUC", "I_NDEP"
    )
    for (i in na2) {
        x2[is.na(x2[,i]),i] <- '2'
    }
    x2[!is.na(x2[,"I48_ATC"] == '0'),"I48_ATC"] <- '2'
    x2[!is.na(x2[,"I48_AT"] == '0'),"I48_AT"] <- '2'
    x2[!is.na(x2[,"D_APC_C"] == '0'),"D_APC_C"] <- '2'
    x2[!is.na(x2[,"D_APC"] == '0'),"D_APC"] <- '2'
    x2[!is.na(x2[,"DM_HP1"] == '0'),"DM_HP1"] <- '2'
    x2[!is.na(x2[,"D_HP_C"] == '0'),"D_HP_C"] <- '2'
    x2[!is.na(x2[,"DM_HP2"] == '0'),"DM_HP2"] <- '2'
    x2[c(FALSE,as.numeric(x2[-1,"I_LDL"]) > 2.60),"D_LDL100"] <- '1'
    x2[is.na(x2[,"D_LDL100"]),"D_NOTALL"] <- '1'
    x2[!is.na(x2[,"D_STT_C"]=='0'),"D_STT_C"] <- '2'
    x2[!is.na(x2[,"D_LD"]=='0'),"D_LD"] <- '2'
    x2[!is.na(x2[,"D_L_C"]=='0'),"D_L_C"] <- '2'
    x2[!is.na(x2[,"D_LL"]=='0'),"D_LL"] <- '2'
    x2[-1,"DM_DM"] <- x2[-1,"D_D_DM"]
    x2[is.na(x2[,"DM_DM"] ),"DM_DM"] <- '2'
    x2[x2[,"D_HG_C"]=='0',"D_HG_C"] <- '2'
    x2[x2[,"D_HG"]=='0',"D_HG"] <- '2'
    x2[-1,"DM_AFSC_H"] <- '1'
    x2[-1,"DM_AFSC_ECG"] <- '1'
    x2[-1,"DM_AF"] <- x2[-1,"D_D_AF"]
    x2[is.na(x2[,"DM_AF"]),"DM_AF"] <- '2'
    x2[!is.na(x2[,"D_FAC_C"]=='0'),"D_FAC_C"] <- '2'


    to1 <- c("D_HI_GZ5","D_HI_GZ4","D_HI_GZ3","D_HI_GZ2","D_HI_GZ1")
    for (i in to1) {
        x2[-1,i] <- '1'
    }
    ch <- x2[1,]
    x2[1,] <- colnames(x2)
    colnames(x2) <- ch
    for (i in 1:ncol(x2)) {
        x2[is.na(x2[,i]),i] <- ''
    }
    filename <- do::Replace(xlsx,'.xlsx','.csv')
    write.csv(x2,filename,row.names = F,quote = F)
}
